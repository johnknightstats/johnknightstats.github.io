#!/usr/bin/env Rscript

# ------------------------------------------------------------
# troubleshoot_unmapped_countries_clubelo_number_one_daily.R
#
# Reads:  posts/elo-number-one/data/processed/clubelo_number_one_daily.csv
# Prints:
#   1) Country codes in the CSV not present in COUNTRY_CODE_TO_NAME
#   2) How many dates are affected (overall + by decade)
#   3) Date ranges for each unmapped code
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

# Keep this mapping identical to your main script
COUNTRY_CODE_TO_NAME <- c(
  "AUT" = "Austria",
  "BEL" = "Belgium",
  "ENG" = "England",
  "ESP" = "Spain",
  "GER" = "Germany",
  "FRG" = "Germany",
  "HUN" = "Hungary",
  "ITA" = "Italy",
  "NED" = "Netherlands",
  "POR" = "Portugal",
  "SCO" = "Scotland",
  "SWE" = "Sweden"
)

df_raw <- read_csv(IN_CSV, show_col_types = FALSE)

needed <- c("Date", "Country")
missing_cols <- setdiff(needed, names(df_raw))
if (length(missing_cols) > 0) stop("Missing columns in CSV: ", paste(missing_cols, collapse = ", "))

df <- df_raw %>%
  transmute(
    Date = as.Date(Date),
    Country = str_squish(as.character(Country)),
    year = year(Date),
    decade = paste0(floor(year / 10) * 10, "s")
  ) %>%
  filter(!is.na(Date))

# Identify unmapped codes
mapped_codes <- names(COUNTRY_CODE_TO_NAME)

unmapped <- df %>%
  filter(!(Country %in% mapped_codes) | is.na(Country) | Country == "") %>%
  mutate(
    Country = ifelse(is.na(Country) | Country == "", "<BLANK/NA>", Country)
  )

cat("\n==============================\n")
cat("Overall date range in CSV:\n")
cat("==============================\n")
cat("Min date: ", as.character(min(df$Date)), "\n", sep = "")
cat("Max date: ", as.character(max(df$Date)), "\n", sep = "")
cat("Total rows (valid Date): ", nrow(df), "\n", sep = "")

cat("\n==============================\n")
cat("Unmapped Country codes (if any):\n")
cat("==============================\n")

if (nrow(unmapped) == 0) {
  cat("None. All Country codes are covered by COUNTRY_CODE_TO_NAME.\n\n")
  quit(status = 0)
}

# List which codes are unmapped + counts
by_code <- unmapped %>%
  count(Country, name = "n_days") %>%
  arrange(desc(n_days), Country)

print(by_code)

cat("\n==============================\n")
cat("Unmapped days by decade:\n")
cat("==============================\n")

by_decade <- unmapped %>%
  count(decade, name = "n_unmapped_days") %>%
  arrange(decade)

print(by_decade)

cat("\n==============================\n")
cat("Date ranges per unmapped code:\n")
cat("==============================\n")

ranges <- unmapped %>%
  group_by(Country) %>%
  summarise(
    first_date = min(Date),
    last_date  = max(Date),
    n_days = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(n_days), Country)

print(ranges)

cat("\n==============================\n")
cat("Sample unmapped rows (first 50):\n")
cat("==============================\n")
print(unmapped %>% arrange(Date) %>% select(Date, Country, decade) %>% head(50))

cat("\nNEXT STEP:\n")
cat("- Add the missing Country codes to COUNTRY_CODE_TO_NAME (or map them into an existing country like FRG->Germany).\n")
cat("- Or, if you prefer, keep them as 'Unknown' instead of dropping those days.\n\n")
