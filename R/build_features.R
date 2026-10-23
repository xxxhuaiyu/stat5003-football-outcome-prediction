# =============================================================================
# Feature engineering — INTERIM version written by Member 5
#
# Feature engineering is Member 2's deliverable. This script implements plan §2
# exactly (same column names) so the Random Forest could be built while waiting.
# Once Member 2's .rds is published, point MODEL_DATA_PATH in the model files
# at that file instead; no model code needs to change.
#
# Rules implemented (plan §2):
#   * every feature uses only matches played strictly BEFORE the fixture's date
#   * form / table features use the current season only; NA if no prior match
#   * per-match rates (averages over up to 5 matches), never totals
#   * h2h_diff uses all prior meetings in the dataset (last 5); NA if none
#   * rest_days_diff uses each club's previous fixture in any competition
#     (games.csv, may be last season), each side capped at 30 days
#   * season_progress = round / max round for that league in that season
#   * NO imputation here — NA is left for the training-fold preprocessing
#
# Output: data/model_data.rds
# =============================================================================

suppressPackageStartupMessages(library(data.table))

build_features <- function(raw_path  = "data/domestic_games_full.rds",
                           games_csv = "data/games.csv",
                           out_path  = "data/model_data.rds") {

  big5 <- c("GB1", "ES1", "IT1", "FR1", "L1")
  g <- as.data.table(readRDS(raw_path))[competition_id %in% big5]
  g <- g[, .(game_id, competition_id, season = as.integer(season), round, date,
             home_club_id, away_club_id, home_club_goals, away_club_goals, result)]
  setorder(g, date, game_id)

  # ---- 1. Long format: one row per club per match ---------------------------
  home <- g[, .(game_id, competition_id, season, date, club_id = home_club_id,
                is_home = TRUE,  gf = home_club_goals, ga = away_club_goals)]
  away <- g[, .(game_id, competition_id, season, date, club_id = away_club_id,
                is_home = FALSE, gf = away_club_goals, ga = home_club_goals)]
  long <- rbind(home, away)
  long[, pts := fifelse(gf > ga, 3L, fifelse(gf == ga, 1L, 0L))]
  long[, gd  := gf - ga]
  setorder(long, club_id, date, game_id)

  # Sum of the previous (up to) k values, excluding the current one.
  # Returns NA where there is no previous value.
  lag_roll_sum <- function(x, k = 5L) {
    i  <- seq_along(x)
    w  <- pmin(i - 1L, k)                 # number of previous matches used
    cs <- cumsum(c(0, x))                 # cs[j + 1] = sum(x[1:j])
    s  <- cs[i] - cs[i - w]
    s[w == 0] <- NA_real_
    s
  }
  # Per-match rate over the previous (up to) k matches
  lag_roll_mean <- function(x, k = 5L) {
    lag_roll_sum(x, k) / pmin(seq_along(x) - 1L, k)
  }

  # Season-to-date and last-5 form (current season only)
  long[, n_prev := seq_len(.N) - 1L, by = .(club_id, season)]
  long[, `:=`(
    ppg_season = fifelse(n_prev > 0, (cumsum(pts) - pts) / n_prev, NA_real_),
    form_ppg   = lag_roll_mean(pts, 5L),
    form_gd    = lag_roll_mean(gd, 5L)
  ), by = .(club_id, season)]

  # Venue-specific form: last 5 home (or away) matches this season
  long[, venue_ppg := lag_roll_mean(pts, 5L), by = .(club_id, season, is_home)]

  # Rest days: gap to the club's previous fixture in ANY competition (league,
  # domestic cup, European cup) from games.csv, so cup congestion counts.
  # Capped at 30 days: longer gaps are the summer break or a promoted club whose
  # last top-flight game was years ago, and only mean "fully rested".
  all_fx <- fread(games_csv, select = c("date", "home_club_id", "away_club_id"))
  all_fx <- unique(rbind(all_fx[, .(club_id = home_club_id, prev_date = as.IDate(date))],
                         all_fx[, .(club_id = away_club_id, prev_date = as.IDate(date))]))
  all_fx[, lookup_date := prev_date]
  setkey(all_fx, club_id, lookup_date)
  long[, lookup_date := as.IDate(date) - 1L]          # strictly before match day
  long[, prev_date := all_fx[long, on = .(club_id, lookup_date), roll = TRUE, x.prev_date]]
  long[, rest_days := pmin(as.numeric(as.IDate(date) - prev_date), 30)]
  long[, c("lookup_date", "prev_date") := NULL]

  # ---- 2. Pre-match league table position -----------------------------------
  # Standings built from matches strictly before the fixture's date.
  # Ranking: points, then goal difference, then goals scored (ties share a rank).
  long[, `:=`(cum_pts = cumsum(pts), cum_gd = cumsum(gd), cum_gf = cumsum(gf)),
       by = .(club_id, season)]

  clubs_cs <- unique(long[, .(competition_id, season, club_id)])
  dates_cs <- unique(long[, .(competition_id, season, date)])
  grid <- clubs_cs[dates_cs, on = .(competition_id, season), allow.cartesian = TRUE]
  grid[, lookup_date := date - 1L]                    # strictly before match day

  snap <- long[, .(competition_id, season, club_id, lookup_date = date,
                   cum_pts, cum_gd, cum_gf)]
  setkey(snap, competition_id, season, club_id, lookup_date)
  grid <- snap[grid, on = .(competition_id, season, club_id, lookup_date),
               roll = TRUE]
  grid[is.na(cum_pts), `:=`(cum_pts = 0L, cum_gd = 0L, cum_gf = 0L)]
  grid[, table_pos := frank(list(-cum_pts, -cum_gd, -cum_gf), ties.method = "min"),
       by = .(competition_id, season, date)]

  long <- grid[, .(competition_id, season, club_id, date, table_pos)][
    long, on = .(competition_id, season, club_id, date)]
  long[n_prev == 0, table_pos := NA_integer_]         # matchday-1 rule

  # ---- 3. Back to one row per match -----------------------------------------
  feat_cols <- c("ppg_season", "form_ppg", "form_gd", "venue_ppg",
                 "rest_days", "table_pos")
  h <- long[is_home == TRUE,  c("game_id", feat_cols), with = FALSE]
  a <- long[is_home == FALSE, c("game_id", feat_cols), with = FALSE]
  setnames(h, feat_cols, paste0("h_", feat_cols))
  setnames(a, feat_cols, paste0("a_", feat_cols))
  m <- a[h[g, on = "game_id"], on = "game_id"]

  m[, `:=`(
    table_pos_diff      = a_table_pos  - h_table_pos,   # +ve = home higher in table
    points_per_game_diff = h_ppg_season - a_ppg_season,
    form_points_diff    = h_form_ppg   - a_form_ppg,
    recent_goal_diff    = h_form_gd    - a_form_gd,
    venue_form_diff     = h_venue_ppg  - a_venue_ppg,   # home's home form vs away's away form
    rest_days_diff      = h_rest_days  - a_rest_days
  )]

  # ---- 4. Head-to-head: last 5 prior meetings (any venue, any season) -------
  m[, pair := paste(pmin(home_club_id, away_club_id),
                    pmax(home_club_id, away_club_id), sep = "_")]
  setorder(m, pair, date, game_id)
  # +1 if the lower-id club won, -1 if the higher-id club won, 0 draw
  m[, low_id_win := fifelse(home_club_goals == away_club_goals, 0L,
                    fifelse((home_club_goals > away_club_goals) ==
                            (home_club_id < away_club_id), 1L, -1L))]
  m[, `:=`(
    h2h_low_wins  = lag_roll_sum(as.integer(low_id_win ==  1L), 5L),
    h2h_high_wins = lag_roll_sum(as.integer(low_id_win == -1L), 5L)
  ), by = pair]
  m[, h2h_diff := fifelse(home_club_id < away_club_id,
                          h2h_low_wins - h2h_high_wins,
                          h2h_high_wins - h2h_low_wins)]

  # ---- 5. Season progress (per league, per season) --------------------------
  m[, round_num := as.integer(gsub("\\D", "", round))]
  m[, season_progress := round_num / max(round_num), by = .(competition_id, season)]

  # ---- 6. Final modelling dataset -------------------------------------------
  m[, result := factor(result, levels = c("home_win", "draw", "away_win"))]
  m[, competition_id := factor(competition_id, levels = big5)]
  setorder(m, date, game_id)

  out <- m[, .(game_id, date, season, competition_id, home_club_id, away_club_id,
               result,
               table_pos_diff, points_per_game_diff, form_points_diff,
               recent_goal_diff, venue_form_diff, h2h_diff, rest_days_diff,
               season_progress)]
  out <- as.data.frame(out)
  saveRDS(out, out_path)
  invisible(out)
}

if (sys.nframe() == 0) {
  x <- build_features()
  cat("Saved data/model_data.rds:", nrow(x), "rows,", ncol(x), "columns\n")
}
