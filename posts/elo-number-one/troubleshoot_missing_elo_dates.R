#!/usr/bin/env Rscript

# ------------------------------------------------------------
# troubleshoot_missing_dates_clubelo_number_one_daily.R
#
# Reads:  posts/elo-number-one/data/processed/clubelo_number_one_daily.csv
# Prints: Missing dates (gaps) and summary by decade/year
#
# Expected: one row per calendar day (Date)
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
  library(stringr)
})

IN_CSV <- "posts/elo-number-one/data/processed/clubelo_number_one_daily.csv"

# ----------------------------
# IO checks
# ----------------------------
if (!file.exists(IN_CSV)) stop("Input file not found: ", IN_CSV)

# ----------------------------
# Load + basic validation
# ----------------------------
df_raw <- read_csv(IN_CSV, show_col_types = FALSE)

if (!("Date" %in% names(df_raw))) stop("Missing required column: Date")

df <- df_raw %>%
  mutate(
    Date_raw = Date,
    Date = as.Date(Date)
  )

bad_date_rows <- df %>% filter(is.na(Date))
if (nrow(bad_date_rows) > 0) {
  cat("\n==============================\n")
  cat("Rows with unparseable Date values:\n")
  cat("==============================\n")
  print(bad_date_rows %>% select(Date_raw) %>% distinct() %>% head(50))
  cat("\n(Showing up to first 50 distinct bad Date strings)\n\n")
}

df_dates <- df %>%
  filter(!is.na(Date)) %>%
  distinct(Date) %>%
  arrange(Date)

if (nrow(df_dates) == 0) stop("No valid dates found after parsing.")

# ----------------------------
# Detect duplicates (multiple rows per day)
# ----------------------------
dup_dates <- df %>%
  filter(!is.na(Date)) %>%
  count(Date, name = "n") %>%
  filter(n > 1) %>%
  arrange(desc(n), Date)

cat("\n==============================\n")
cat("Duplicate dates (more than one row per day):\n")
cat("==============================\n")
if (nrow(dup_dates) == 0) {
  cat("None\n")
} else {
  print(dup_dates)
}

# ----------------------------
# Compute expected full date sequence
# ----------------------------
min_date <- min(df_dates$Date)
max_date <- max(df_dates$Date)

expected <- tibble(Date = seq.Date(min_date, max_date, by = "day"))

missing <- expected %>%
  anti_join(df_dates, by = "Date") %>%
  arrange(Date) %>%
  mutate(
    year = year(Date),
    decade = paste0(floor(year / 10) * 10, "s")
  )

# ----------------------------
# Summaries
# ----------------------------
cat("\n==============================\n")
cat("Date range covered by file:\n")
cat("==============================\n")
cat("Min date: ", as.character(min_date), "\n", sep = "")
cat("Max date: ", as.character(max_date), "\n", sep = "")
cat("Total expected days: ", nrow(expected), "\n", sep = "")
cat("Distinct days present: ", nrow(df_dates), "\n", sep = "")
cat("Missing days: ", nrow(missing), "\n", sep = "")

cat("\n==============================\n")
cat("Missing days by decade:\n")
cat("==============================\n")
if (nrow(missing) == 0) {
  cat("None\n")
} else {
  print(
    missing %>%
      count(decade, name = "missing_days") %>%
      arrange(decade)
  )
}

cat("\n==============================\n")
cat("Missing days by year:\n")
cat("==============================\n")
if (nrow(missing) == 0) {
  cat("None\n")
} else {
  print(
    missing %>%
      count(year, name = "missing_days") %>%
      arrange(year)
  )
}

# ----------------------------
# List missing dates and gap runs
# ----------------------------
cat("\n==============================\n")
cat("Missing dates (first 200 shown):\n")
cat("==============================\n")
if (nrow(missing) == 0) {
  cat("None\n")
} else {
  print(missing %>% select(Date) %>% head(200))
  if (nrow(missing) > 200) cat("\n... (", nrow(missing) - 200, " more)\n", sep = "")
}

# Identify contiguous runs of missing dates (gaps)
gap_runs <- missing %>%
  arrange(Date) %>%
  mutate(
    grp = as.integer(Date) - row_number()
  ) %>%
  group_by(grp) %>%
  summarise(
    start = min(Date),
    end = max(Date),
    days = as.integer(end - start) + 1L,
    .groups = "drop"
  ) %>%
  arrange(desc(days), start)

cat("\n==============================\n")
cat("Contiguous missing-date runs (gaps):\n")
cat("==============================\n")
if (nrow(gap_runs) == 0) {
  cat("None\n")
} else {
  print(gap_runs)
}

# ----------------------------
# Helpful note
# ----------------------------
cat("\nNOTE:\n")
cat("- If you see missing dates, those decades will render shorter in your timeline.\n")
cat("- If you see duplicate dates, you may have multiple #1 entries per day (unexpected).\n\n")
