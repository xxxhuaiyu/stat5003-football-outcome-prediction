# =============================================================================
# Chronological split + 5-fold expanding-window folds (owner: Member 5)
#
# ALL five members use these objects; nobody re-splits the data.
#
#   source("R/make_folds.R")
#   sp    <- load_split()            # list(train = ..., test = ...)
#   folds <- readRDS("data/cv_folds.rds")
#   for (f in folds) {
#     fit_rows <- sp$train[f$train, ]   # integer row indices into sp$train
#     val_rows <- sp$train[f$valid, ]
#   }
#
# Split (plan §5):  train = season <= 2022,  final test = season >= 2023.
# Folds: one validation season per fold, each trained on ALL earlier seasons.
#
#   Fold 1: train 2012–2017  -> validate 2018
#   Fold 2: train 2012–2018  -> validate 2019
#   Fold 3: train 2012–2019  -> validate 2020
#   Fold 4: train 2012–2020  -> validate 2021
#   Fold 5: train 2012–2021  -> validate 2022
#
# Whole seasons are used as blocks so a validation season is never split
# mid-way, and the post-2020 home-advantage drop shows up in validation the same
# way it will in the test seasons.
# =============================================================================

MODEL_DATA_PATH <- "data/model_data.rds"   # swap for Member 2's file when published

load_split <- function(path = MODEL_DATA_PATH) {
  d <- readRDS(path)
  d <- d[order(d$date, d$game_id), ]
  rownames(d) <- NULL
  train <- d[d$season <= 2022, ]
  test  <- d[d$season >= 2023, ]
  rownames(train) <- rownames(test) <- NULL
  list(train = train, test = test)
}

make_folds <- function(train, valid_seasons = 2018:2022) {
  stopifnot(!is.unsorted(train$date))
  folds <- lapply(valid_seasons, function(s) {
    list(valid_season  = s,
         train         = which(train$season <  s),
         valid         = which(train$season == s),
         train_game_id = train$game_id[train$season <  s],
         valid_game_id = train$game_id[train$season == s])
  })
  names(folds) <- paste0("Fold", seq_along(folds))
  folds
}

if (sys.nframe() == 0) {
  sp    <- load_split()
  folds <- make_folds(sp$train)
  saveRDS(folds, "data/cv_folds.rds")
  cat("Train:", nrow(sp$train), " Test:", nrow(sp$test), "\n")
  for (f in folds) cat(sprintf("Validate %d: train n = %5d, valid n = %4d\n",
                               f$valid_season, length(f$train), length(f$valid)))
}
