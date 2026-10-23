# =============================================================================
# Member 1: reproducible multinomial logistic regression pipeline
#
# Run from the project root:
#   Rscript R/member1_mlr.R
#
# Inputs:
#   data/model_data.rds
#   data/cv_folds.rds
#   R/evaluate_model.R
#   R/make_folds.R
#
# Outputs:
#   outputs/member1_mlr_results.rds
#   outputs/member1_mlr_comparison_row.csv
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(nnet)
})

source("R/evaluate_model.R")
source("R/make_folds.R")

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

sp <- load_split("data/model_data.rds")
train <- sp$train
test <- sp$test
folds <- readRDS("data/cv_folds.rds")

stopifnot(
  identical(levels(train$result), RESULT_LEVELS),
  all(train$season <= 2022),
  all(test$season >= 2023),
  length(folds) == 5,
  all(vapply(folds, function(f) {
    max(train$date[f$train]) < min(train$date[f$valid])
  }, logical(1)))
)

baseline_feats <- c("competition_id", "season_progress")
refined_feats <- c(
  "table_pos_diff", "points_per_game_diff", "form_points_diff",
  "recent_goal_diff", "venue_form_diff", "h2h_diff",
  "rest_days_diff", "competition_id", "season_progress"
)

candidate_sets <- list(
  Full = refined_feats,
  `Without table position` = setdiff(refined_feats, "table_pos_diff"),
  `Without season PPG` = setdiff(refined_feats, "points_per_game_diff"),
  `Without venue form` = setdiff(refined_feats, "venue_form_diff")
)

decay_grid <- c(0, 0.0001, 0.001, 0.01, 0.1, 1)
weight_grid <- CJ(
  w_draw = seq(1, 2.2, by = 0.1),
  w_away = seq(0.8, 1.8, by = 0.1)
)

# Fit preprocessing on an analysis set only.
prep_fit <- function(df, feats) {
  numeric_vars <- feats[vapply(df[feats], is.numeric, logical(1))]
  factor_vars <- feats[vapply(df[feats], is.factor, logical(1))]
  medians <- vapply(df[numeric_vars], median, numeric(1), na.rm = TRUE)

  missing_vars <- numeric_vars[vapply(df[numeric_vars], anyNA, logical(1))]
  signatures <- vapply(missing_vars, function(v) {
    paste(which(is.na(df[[v]])), collapse = ",")
  }, character(1))
  indicator_vars <- missing_vars[!duplicated(signatures)]

  filled <- as.data.frame(df[numeric_vars])
  for (v in numeric_vars) {
    filled[[v]][is.na(filled[[v]])] <- medians[[v]]
  }
  centres <- vapply(filled, mean, numeric(1))
  scales <- vapply(filled, sd, numeric(1))
  scales[!is.finite(scales) | scales == 0] <- 1

  list(
    feats = feats,
    numeric_vars = numeric_vars,
    factor_vars = factor_vars,
    medians = medians,
    indicator_vars = indicator_vars,
    centres = centres,
    scales = scales,
    factor_levels = lapply(df[factor_vars], levels)
  )
}

# Apply a stored analysis-set recipe without re-estimating any quantity.
prep_apply <- function(recipe, df, include_result = TRUE) {
  out <- as.data.frame(df[recipe$feats])

  for (v in recipe$indicator_vars) {
    out[[paste0(v, "_na")]] <- as.integer(is.na(df[[v]]))
  }
  for (v in recipe$numeric_vars) {
    out[[v]][is.na(out[[v]])] <- recipe$medians[[v]]
    out[[v]] <- (out[[v]] - recipe$centres[[v]]) / recipe$scales[[v]]
  }
  for (v in recipe$factor_vars) {
    out[[v]] <- factor(out[[v]], levels = recipe$factor_levels[[v]])
  }
  if (include_result) {
    out$result <- factor(df$result, levels = RESULT_LEVELS)
  }
  out
}

fit_mlr <- function(df, decay) {
  nnet::multinom(
    result ~ ., data = df, decay = decay, trace = FALSE,
    MaxNWts = 20000, maxit = 500
  )
}

predict_prob <- function(model, newdata) {
  p <- as.matrix(predict(model, newdata = newdata, type = "probs"))
  p[, RESULT_LEVELS, drop = FALSE]
}

apply_rule <- function(prob, weights = c(1, 1, 1)) {
  weighted <- sweep(prob[, RESULT_LEVELS, drop = FALSE], 2, weights, `*`)
  factor(
    RESULT_LEVELS[max.col(weighted, ties.method = "first")],
    levels = RESULT_LEVELS
  )
}

score_weight_grid <- function(prob_list, truth_list) {
  ans <- copy(weight_grid)
  ans[, cv_macro_f1 := mapply(function(wd, wa) {
    mean(mapply(function(prob, truth) {
      evaluate_model(
        truth,
        apply_rule(prob, c(1, wd, wa))
      )$overall$macro_f1
    }, prob_list, truth_list))
  }, w_draw, w_away)]
  ans
}

run_cv_configuration <- function(feats, decay) {
  probs <- vector("list", length(folds))
  truths <- vector("list", length(folds))
  audit <- vector("list", length(folds))

  for (k in seq_along(folds)) {
    fold <- folds[[k]]
    recipe <- prep_fit(train[fold$train, ], feats)
    analysis_set <- prep_apply(recipe, train[fold$train, ])
    assessment_set <- prep_apply(recipe, train[fold$valid, ])
    model <- fit_mlr(analysis_set, decay)

    probs[[k]] <- predict_prob(model, assessment_set)
    truths[[k]] <- assessment_set$result
    audit[[k]] <- list(
      validation_season = fold$valid_season,
      train_game_id = train$game_id[fold$train],
      valid_game_id = train$game_id[fold$valid],
      medians = recipe$medians,
      centres = recipe$centres,
      scales = recipe$scales,
      indicator_vars = recipe$indicator_vars
    )
  }

  plain_f1 <- mean(mapply(function(prob, truth) {
    evaluate_model(truth, apply_rule(prob))$overall$macro_f1
  }, probs, truths))

  weight_scores <- score_weight_grid(probs, truths)
  best_weight <- weight_scores[which.max(cv_macro_f1)]

  list(
    summary = data.table(
      decay = decay,
      cv_macro_f1_plain = plain_f1,
      w_draw = best_weight$w_draw,
      w_away = best_weight$w_away,
      cv_macro_f1 = best_weight$cv_macro_f1
    ),
    probabilities = probs,
    truths = truths,
    audit = audit
  )
}

# Four candidate feature sets x six decay values.
cv_rows <- list()
cv_details <- list()
counter <- 1L
for (feature_set in names(candidate_sets)) {
  for (decay in decay_grid) {
    ans <- run_cv_configuration(candidate_sets[[feature_set]], decay)
    ans$summary[, `:=`(
      config_id = counter,
      feature_set = feature_set
    )]
    cv_rows[[counter]] <- ans$summary
    cv_details[[counter]] <- ans
    counter <- counter + 1L
  }
}

cv_table <- rbindlist(cv_rows)
setcolorder(
  cv_table,
  c("config_id", "feature_set", "decay", "cv_macro_f1_plain",
    "w_draw", "w_away", "cv_macro_f1")
)
best <- cv_table[which.max(cv_macro_f1)]
best_features <- candidate_sets[[best$feature_set]]
selected_cv_audit <- cv_details[[best$config_id]]$audit

# Tune the two-variable context baseline using the same validation design.
baseline_rows <- list()
for (i in seq_along(decay_grid)) {
  ans <- run_cv_configuration(baseline_feats, decay_grid[i])
  baseline_rows[[i]] <- ans$summary
}
baseline_cv <- rbindlist(baseline_rows)
best_base <- baseline_cv[which.max(cv_macro_f1)]

# Refit selected configurations on all training seasons, then use Final Test once.
final_fit <- function(feats, decay, weights) {
  recipe <- prep_fit(train, feats)
  train_ready <- prep_apply(recipe, train)
  test_ready <- prep_apply(recipe, test)
  model <- fit_mlr(train_ready, decay)
  probability <- predict_prob(model, test_ready)

  list(
    model = model,
    recipe = recipe,
    probability = probability,
    pred_plain = apply_rule(probability),
    pred_weighted = apply_rule(probability, weights)
  )
}

base_final <- final_fit(
  baseline_feats,
  best_base$decay,
  c(1, best_base$w_draw, best_base$w_away)
)
refined_final <- final_fit(
  best_features,
  best$decay,
  c(1, best$w_draw, best$w_away)
)

predictions <- list(
  majority = factor(rep("home_win", nrow(test)), levels = RESULT_LEVELS),
  baseline_plain = base_final$pred_plain,
  baseline = base_final$pred_weighted,
  refined_plain = refined_final$pred_plain,
  refined = refined_final$pred_weighted
)
evaluations <- lapply(
  predictions,
  function(pred) evaluate_model(test$result, pred)
)

comparison <- rbind(
  metrics_row(evaluations$majority, "Majority class", "Always home_win"),
  metrics_row(evaluations$baseline_plain, "Multinomial logistic regression",
              "Baseline, largest probability"),
  metrics_row(evaluations$baseline, "Multinomial logistic regression",
              "Baseline, tuned decision weights"),
  metrics_row(evaluations$refined_plain, "Multinomial logistic regression",
              "Refined, largest probability"),
  metrics_row(evaluations$refined, "Multinomial logistic regression",
              "Refined, tuned decision weights (final)")
)

numeric_feats <- refined_feats[refined_feats != "competition_id"]
corr_data <- as.data.frame(train[numeric_feats])
for (v in numeric_feats) {
  corr_data[[v]][is.na(corr_data[[v]])] <- median(corr_data[[v]], na.rm = TRUE)
}
corr_matrix <- cor(corr_data)

results <- list(
  comparison = comparison,
  test_metrics = evaluations$refined,
  plain_test_metrics = evaluations$refined_plain,
  baseline_test_metrics = evaluations$baseline,
  best_feature_set = best$feature_set,
  best_features = best_features,
  best_decay = best$decay,
  decision_weights = c(
    home_win = 1,
    draw = best$w_draw,
    away_win = best$w_away
  ),
  cv_table = cv_table,
  baseline_cv_table = baseline_cv,
  selected_cv_audit = selected_cv_audit,
  final_preprocessing = refined_final$recipe,
  correlations = corr_matrix,
  coefficients = coef(refined_final$model),
  test_predictions = data.frame(
    game_id = test$game_id,
    truth = test$result,
    pred_plain = predictions$refined_plain,
    pred = predictions$refined,
    refined_final$probability,
    check.names = FALSE
  ),
  provenance = list(
    model_data_md5 = unname(tools::md5sum("data/model_data.rds")),
    folds_md5 = unname(tools::md5sum("data/cv_folds.rds")),
    scoring_file_md5 = unname(tools::md5sum("R/evaluate_model.R")),
    generated_at = Sys.time()
  )
)

saveRDS(results, "outputs/member1_mlr_results.rds")
write.csv(
  comparison,
  "outputs/member1_mlr_comparison_row.csv",
  row.names = FALSE
)

cat("Selected feature set:", best$feature_set, "\n")
cat("Selected decay:", best$decay, "\n")
cat("Decision weights: 1 /", best$w_draw, "/", best$w_away, "\n")
cat("Validation Macro-F1:", sprintf("%.6f", best$cv_macro_f1), "\n")
cat("Final Test metrics:\n")
print(evaluations$refined$overall)
