# =============================================================================
# Shared scoring function (owner: Member 5)
#
# ALL five members must source() this file. Do not compute Macro-F1 yourself.
#
#   source("R/evaluate_model.R")
#   res <- evaluate_model(truth = test$result, pred = my_predictions)
#   res$overall      # Accuracy, Balanced Accuracy, Macro-F1 (one-row data.frame)
#   res$per_class    # Precision / Recall / F1 for home_win, draw, away_win
#   res$confusion    # Confusion matrix (rows = predicted, columns = truth)
#   metrics_row(res, model = "Random Forest", stage = "Refined")  # comparison row
#
# Base R only, so it runs for everyone without extra packages.
# =============================================================================

RESULT_LEVELS <- c("home_win", "draw", "away_win")

evaluate_model <- function(truth, pred, levels = RESULT_LEVELS) {
  truth <- factor(as.character(truth), levels = levels)
  pred  <- factor(as.character(pred),  levels = levels)

  if (length(truth) != length(pred)) stop("truth and pred have different lengths")
  if (anyNA(truth)) stop("truth contains NA or labels outside: ", paste(levels, collapse = ", "))
  if (anyNA(pred))  stop("pred contains NA or labels outside: ", paste(levels, collapse = ", "))

  # Rows = predicted class, columns = true class
  cm <- table(Predicted = pred, Truth = truth)

  tp        <- diag(cm)
  n_pred    <- rowSums(cm)   # how often each class was predicted
  n_true    <- colSums(cm)   # how often each class actually occurred

  # A class that is never predicted gets precision 0 (not NaN), so that a
  # model that ignores draws is penalised in Macro-F1 rather than skipped.
  precision <- ifelse(n_pred > 0, tp / n_pred, 0)
  recall    <- ifelse(n_true > 0, tp / n_true, 0)
  f1        <- ifelse(precision + recall > 0,
                      2 * precision * recall / (precision + recall), 0)

  per_class <- data.frame(
    class     = levels,
    precision = as.numeric(precision),
    recall    = as.numeric(recall),
    f1        = as.numeric(f1),
    support   = as.integer(n_true),
    predicted = as.integer(n_pred),
    row.names = NULL
  )

  overall <- data.frame(
    accuracy          = sum(tp) / sum(cm),
    balanced_accuracy = mean(recall),
    macro_f1          = mean(f1)
  )

  list(overall = overall, per_class = per_class, confusion = cm)
}

# One wide row per model, for the five-model comparison table
metrics_row <- function(res, model, stage = "") {
  pc <- res$per_class
  wide <- c(
    setNames(pc$precision, paste0("precision_", pc$class)),
    setNames(pc$recall,    paste0("recall_",    pc$class)),
    setNames(pc$f1,        paste0("f1_",        pc$class))
  )
  data.frame(model = model, stage = stage, res$overall, as.list(wide),
             check.names = FALSE)
}
