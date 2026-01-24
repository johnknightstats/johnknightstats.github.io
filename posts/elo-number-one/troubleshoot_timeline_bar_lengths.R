#!/usr/bin/env Rscript

# ------------------------------------------------------------
# troubleshoot_timeline_bar_lengths.R
#
# Reads:  posts/elo-number-one/data/processed/clubelo_number_one_daily.csv
# Goal:  explain why some decade bars are shorter by checking:
#        - actual date coverage per decade
#        - decade_start_date computed two ways
#        - resulting xend (0..1) for the last day in decade
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(tibble)
})

IN_CSV <- "posts/elo-number-one/data/processed/clubelo_number_one_daily.csv"

if (!file.exists(IN_CSV)) stop("Input file not found: ", IN_CSV)

df <- read_csv(IN_CSV, show_col_types = FALSE) %>%
  mutate(
    Date = as.Date(Date),
    year = year(Date),
    decade = paste0(floor(year / 10) * 10, "s")
  ) %>%
  filter(!is.na(Date)) %>%
  mutate(decade = factor(decade, levels = sort(unique(decade))))

# Actual coverage by decade
cov <- df %>%
  group_by(decade) %>%
  summarise(
    first_date = min(Date),
    last_date  = max(Date),
    .groups = "drop"
  )

# Two ways to compute decade start year/date:
# 1) "risky" way (substr on factor)
# 2) robust way (coerce to character, strip trailing 's')
cov2 <- cov %>%
  mutate(
    # risky (mirrors the problematic pattern)
    start_year_substr = suppressWarnings(as.integer(substr(decade, 1, 4))),
    start_date_substr = as.Date(paste0(start_year_substr, "-01-01")),
    
    # robust
    start_year_ok = as.integer(sub("s$", "", as.character(decade))),
    start_date_ok = as.Date(paste0(start_year_ok, "-01-01")),
    
    end_date_ok = as.Date(paste0(start_year_ok + 10, "-01-01")) - days(1),
    end_excl_ok = end_date_ok + days(1),
    
    decade_days_ok = as.numeric(end_excl_ok - start_date_ok),
    
    # what xend WOULD be for the decade's last observed date
    xend_using_substr = as.numeric((last_date + days(1)) - start_date_substr) / decade_days_ok,
    xend_using_ok     = as.numeric((last_date + days(1)) - start_date_ok)     / decade_days_ok
  )

cat("\nPer-decade diagnostics:\n")
print(
  cov2 %>%
    select(
      decade, first_date, last_date,
      start_year_substr, start_date_substr,
      start_year_ok, start_date_ok, end_date_ok,
      xend_using_substr, xend_using_ok
    ) %>%
    arrange(start_year_ok)
)

cat("\nInterpretation:\n")
cat("- For a complete decade, xend_using_ok should be ~1.000 (except the final partial decade).\n")
cat("- If xend_using_substr is noticeably smaller for some decades, your decade_start_date calc is wrong.\n\n")
