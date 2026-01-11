# ------------------------------------------------------------
# clubelo.com daily #1 + British spells at #1 (since 1940-01-01)
# PLUS: British teams that reached #2 but never #1, with #2 spells
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
})

# -----------------------------
# Paths (edit if you want)
# -----------------------------
data_dir <- path.expand("~/Projects/elo-league-sims/data/elo")

# Your Quarto post folder (project folder for the article)
post_dir <- path.expand("~/Projects/johnknightstats.github.io/posts/elo-number-one")

out_dir <- file.path(post_dir, "data", "processed")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Output files
out_daily_csv  <- file.path(out_dir, "clubelo_number_one_daily.csv")
out_spells_csv <- file.path(out_dir, "clubelo_number_one_british_spells.csv")
out_totals_csv <- file.path(out_dir, "clubelo_number_one_british_totals.csv")

# NEW outputs
out_daily_top2_csv <- file.path(out_dir, "clubelo_top2_daily.csv")  # optional but useful
out_british_second_never_first_spells_csv <- file.path(out_dir, "clubelo_british_second_never_first_spells.csv")
out_british_second_never_first_totals_csv <- file.path(out_dir, "clubelo_british_second_never_first_totals.csv")

# -----------------------------
# Helpers
# -----------------------------
extract_date_from_filename <- function(fn) {
  m <- str_match(basename(fn), "^eloratings_(\\d{4}-\\d{2}-\\d{2})\\.csv$")
  if (is.na(m[1, 2])) return(NA_character_)
  m[1, 2]
}

# Return top N rows for a file (Rank if present/useful, else by Elo desc)
read_top_n_for_file <- function(path, n = 2L) {
  dt <- fread(path, showProgress = FALSE)
  
  needed <- c("Club", "Country", "Elo")
  missing <- setdiff(needed, names(dt))
  if (length(missing) > 0) {
    stop("Missing required columns in ", basename(path), ": ", paste(missing, collapse = ", "))
  }
  
  date_str <- extract_date_from_filename(path)
  if (is.na(date_str)) stop("Could not parse date from filename: ", basename(path))
  d <- as.IDate(date_str)
  
  dt[, Club := as.character(Club)]
  dt[, Country := as.character(Country)]
  dt[, Elo := suppressWarnings(as.numeric(Elo))]
  
  if (all(is.na(dt$Elo))) stop("All Elo values are NA in file: ", basename(path))
  
  use_rank <- FALSE
  if ("Rank" %in% names(dt)) {
    rk <- suppressWarnings(as.integer(dt$Rank))
    # Only trust Rank if we can see at least ranks 1..n
    if (any(rk == 1, na.rm = TRUE) && any(rk == n, na.rm = TRUE)) {
      dt[, Rank_num := rk]
      use_rank <- TRUE
    }
  }
  
  if (use_rank) {
    topn <- dt[Rank_num %in% 1:n][order(Rank_num, -Elo, Club)]
  } else {
    topn <- dt[order(-Elo, Club)][1:n]
    topn[, Rank_num := seq_len(.N)]
  }
  
  # Guarantee exactly n rows (if file is malformed/short, fail loudly)
  if (nrow(topn) < n) {
    stop("File has fewer than ", n, " usable rows: ", basename(path))
  }
  
  topn[, .(
    Date = d,
    Rank = as.integer(Rank_num),
    Team = Club,
    Country = Country
  )]
}

compute_spells <- function(daily_dt) {
  setorder(daily_dt, Date)
  
  daily_dt[, prev_date := shift(Date)]
  daily_dt[, prev_team := shift(Team)]
  daily_dt[, prev_country := shift(Country)]
  
  daily_dt[, is_break := FALSE]
  daily_dt[is.na(prev_date), is_break := TRUE]
  daily_dt[!is.na(prev_date) & Date != (prev_date + 1L), is_break := TRUE]
  daily_dt[!is.na(prev_team) & Team != prev_team, is_break := TRUE]
  daily_dt[!is.na(prev_country) & Country != prev_country, is_break := TRUE]
  
  daily_dt[, spell_id := cumsum(is_break)]
  
  spells <- daily_dt[, .(
    Team = first(Team),
    Country = first(Country),
    start_date = min(Date),
    end_date = max(Date),
    days = as.integer(max(Date) - min(Date) + 1L)
  ), by = spell_id]
  
  spells[]
}

# -----------------------------
# Main
# -----------------------------
files <- list.files(
  data_dir,
  pattern = "^eloratings_\\d{4}-\\d{2}-\\d{2}\\.csv$",
  full.names = TRUE
)
if (length(files) == 0) stop("No files found in: ", data_dir)

files <- sort(files)
message("Found ", length(files), " daily Elo files.")

# Build daily top-2 table (rank 1 and 2 each day)
top2_list <- lapply(files, read_top_n_for_file, n = 2L)
top2 <- rbindlist(top2_list)
setorder(top2, Date, Rank)

# Optional: write the daily top-2 file (handy for debugging / QA)
fwrite(top2, out_daily_top2_csv)
message("Wrote: ", out_daily_top2_csv)

# Daily #1 (for your existing outputs)
daily1 <- top2[Rank == 1L, .(Date, Team, Country)]
setorder(daily1, Date)

fwrite(daily1, out_daily_csv)
message("Wrote: ", out_daily_csv)

# Compute spells for #1 across full series
spells_all_1 <- compute_spells(copy(daily1))

# British spells at #1
spells_brit_1 <- spells_all_1[Country %in% c("ENG", "SCO")][, .(Team, Country, start_date, end_date, days)]
setorder(spells_brit_1, Team, start_date)

totals_brit_1 <- spells_brit_1[, .(
  total_days_number_one = sum(days),
  spells = .N,
  first_day_number_one = min(start_date),
  last_day_number_one = max(end_date)
), by = .(Team, Country)]
setorder(totals_brit_1, -total_days_number_one, Team)

fwrite(spells_brit_1, out_spells_csv)
message("Wrote: ", out_spells_csv)

fwrite(totals_brit_1, out_totals_csv)
message("Wrote: ", out_totals_csv)

message("\nTop British teams by total days at #1:")
print(head(totals_brit_1, 20))

# ------------------------------------------------------------
# NEW: British teams that reached #2 but never #1, with #2 spells
# ------------------------------------------------------------

# Who has *ever* been #1 (any country)
ever_first <- unique(daily1[, .(Team, Country)])
setkey(ever_first, Team, Country)

# Daily #2
daily2 <- top2[Rank == 2L, .(Date, Team, Country)]
setorder(daily2, Date)

# Restrict to British (#2 spells only for ENG/SCO)
daily2_brit <- daily2[Country %in% c("ENG", "SCO")]
setkey(daily2_brit, Team, Country)

# Keep those that are NOT in ever_first (never #1)
daily2_brit_never_first <- daily2_brit[!ever_first, on = .(Team, Country)]
setorder(daily2_brit_never_first, Date)

# Spells at #2 for those teams
spells_brit_2_never_first <- compute_spells(copy(daily2_brit_never_first))
setorder(spells_brit_2_never_first, Team, start_date)

# Totals at #2 for those teams (optional but usually useful)
totals_brit_2_never_first <- spells_brit_2_never_first[, .(
  total_days_number_two = sum(days),
  spells = .N,
  first_day_number_two = min(start_date),
  last_day_number_two = max(end_date)
), by = .(Team, Country)]
setorder(totals_brit_2_never_first, -total_days_number_two, Team)

# Write outputs
fwrite(spells_brit_2_never_first, out_british_second_never_first_spells_csv)
message("Wrote: ", out_british_second_never_first_spells_csv)

fwrite(totals_brit_2_never_first, out_british_second_never_first_totals_csv)
message("Wrote: ", out_british_second_never_first_totals_csv)

message("\nBritish teams that were #2 but never #1 (top by total days at #2):")
print(head(totals_brit_2_never_first, 30))
