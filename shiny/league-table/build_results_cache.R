#!/usr/bin/env Rscript

# build_results_cache.R
# ------------------------------------------------------------
# Reads:  SQLite
#   - Historical: Match + Team_Match + Team  (competition_id = 9)
#   - 2025/2026+: Fotmob_Match (league_derived = "Premier League")
# Writes: results_cache_comp9.csv
#
# Output columns:
# season, match_id, match_date, home_team, away_team, home_goals, away_goals
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(readr)
  library(lubridate)
})

# ----------------------------
# Config
# ----------------------------
DB_PATH   <- Sys.getenv("FOOTBALLDB1_SQLITE", unset = "~/Data/footballdb1.sqlite")

SITE_ROOT <- "~/Projects/johnknightstats.github.io"
APP_DIR   <- file.path(SITE_ROOT, "shiny", "league-table")
dir.create(APP_DIR, recursive = TRUE, showWarnings = FALSE)

OUT_CSV   <- file.path(APP_DIR, "results_cache_comp9.csv")

COMP_ID   <- 9
FOTMOB_LEAGUE <- "Premier League"
FOTMOB_MIN_SEASON_ENDYEAR <- 2026  # 2025/2026 season and later

# ----------------------------
# Helpers
# ----------------------------
clean_team_name <- function(x) {
  x <- gsub("^(AFC|FC)\\s+", "", x, perl = TRUE)
  x <- gsub("\\s+(AFC|FC)$", "", x, perl = TRUE)
  trimws(x)
}

season_end_year <- function(season) {
  # handles both "1990-1991" and "2025/2026"
  y <- suppressWarnings(as.integer(substr(season, 6, 9)))
  y
}

# ----------------------------
# Build cache
# ----------------------------
con <- dbConnect(RSQLite::SQLite(), path.expand(DB_PATH))
on.exit(dbDisconnect(con), add = TRUE)

# ---- Historical (up to 2024/2025, and anything pre-2025/2026) from Match/Team_Match/Team
sql_hist <- '
SELECT
  m.season,
  m.match_id,
  m.match_date,
  th.team_name AS home_team,
  ta.team_name AS away_team,
  tmh.goals     AS home_goals,
  tma.goals     AS away_goals
FROM "Match" m
JOIN "Team" th
  ON th.team_id = m.home_team_id
JOIN "Team" ta
  ON ta.team_id = m.away_team_id
JOIN "Team_Match" tmh
  ON tmh.match_id = m.match_id AND tmh.team_id = m.home_team_id
JOIN "Team_Match" tma
  ON tma.match_id = m.match_id AND tma.team_id = m.away_team_id
WHERE m.competition_id = ?
ORDER BY m.match_date, m.match_id
'

hist <- dbGetQuery(con, sql_hist, params = list(COMP_ID)) |>
  mutate(
    match_date = as.Date(match_date),
    home_goals = as.integer(home_goals),
    away_goals = as.integer(away_goals),
    home_team  = clean_team_name(home_team),
    away_team  = clean_team_name(away_team),
    season_end = season_end_year(season)
  ) |>
  filter(!is.na(season_end), season_end < FOTMOB_MIN_SEASON_ENDYEAR) |>
  select(season, match_id, match_date, home_team, away_team, home_goals, away_goals)

# ---- 2025/2026+ from Fotmob_Match
# match_time_utc example: 2025-08-31T15:30:00:000Z
# We'll parse to Date in R after pulling as text.
sql_fotmob <- '
SELECT
  fm.season_derived AS season,
  fm.match_id       AS match_id,
  fm.match_time_utc AS match_time_utc,
  fm.home_team_name AS home_team,
  fm.away_team_name AS away_team,
  fm.home_score     AS home_goals,
  fm.away_score     AS away_goals
FROM "Fotmob_Match" fm
WHERE fm.league_derived = ?
ORDER BY fm.match_time_utc, fm.match_id
'

fotmob_raw <- dbGetQuery(con, sql_fotmob, params = list(FOTMOB_LEAGUE)) |>
  mutate(
    season = as.character(season),
    season_end = season_end_year(season)
  ) |>
  filter(!is.na(season_end), season_end >= FOTMOB_MIN_SEASON_ENDYEAR)

# Parse match_time_utc -> match_date
# Handle both "...:000Z" and standard ISO "...Z"
fotmob <- fotmob_raw |>
  mutate(
    match_date = suppressWarnings(as.Date(ymd_hms(match_time_utc, tz = "UTC"))),
    match_date = ifelse(
      is.na(match_date),
      as.Date(parse_date_time(match_time_utc, orders = c("Ymd HMS", "Ymd HMS:OS", "YmdTHMS", "YmdTHMS:OS"), tz = "UTC")),
      match_date
    ),
    match_date = as.Date(match_date, origin = "1970-01-01"),
    home_goals = as.integer(home_goals),
    away_goals = as.integer(away_goals),
    home_team  = clean_team_name(home_team),
    away_team  = clean_team_name(away_team)
  ) |>
  # Keep only matches with a parsable date; scores can be NA for future fixtures (that's fine)
  filter(!is.na(match_date)) |>
  select(season, match_id, match_date, home_team, away_team, home_goals, away_goals)

# ---- Combine and write
df <- bind_rows(hist, fotmob) |>
  arrange(match_date, match_id)

if (nrow(df) == 0) stop("Combined query returned 0 rows. Check DB_PATH/schema/filters.")

write_csv(df, OUT_CSV)
message("Wrote ", nrow(df), " rows to ", OUT_CSV)
message(" - Historical rows: ", nrow(hist))
message(" - Fotmob rows:     ", nrow(fotmob))
