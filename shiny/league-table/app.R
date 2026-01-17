# app.R
suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(gt)
})

# ------------------------------------------------------------
# Load cached results ONCE (in-memory)
# ------------------------------------------------------------
CACHE_CSV <- "results_cache_comp9.csv"

results_all <- read.csv(CACHE_CSV, stringsAsFactors = FALSE) |>
  mutate(
    match_date = as.Date(match_date),
    home_goals = as.integer(home_goals),
    away_goals = as.integer(away_goals)
  )

if (nrow(results_all) == 0) stop("Cache CSV is empty.")

# ------------------------------------------------------------
# Points deductions (season/team -> deduction)
# (You can extend this list any time)
# ------------------------------------------------------------
points_deductions <- tibble::tribble(
  ~season,      ~team,                ~deduct,
  "1990-1991",  "Arsenal",            2,
  "1990-1991",  "Manchester United",  1,
  "1996-1997",  "Middlesbrough",      3,
  "2009-2010",  "Portsmouth",         9,
  "2023-2024",  "Everton",            6,
  "2023-2024",  "Nottingham Forest",  4
)

# ------------------------------------------------------------
# Helper: choose season based on most recent match <= date
# ------------------------------------------------------------
season_for_date <- function(df, date) {
  d <- as.Date(date)
  latest <- df |>
    filter(match_date <= d) |>
    arrange(desc(match_date), desc(match_id)) |>
    slice_head(n = 1)
  
  if (nrow(latest) == 0) return(NA_character_)
  latest$season[[1]]
}

# ------------------------------------------------------------
# Standings calculation for a season at an "end-of-day" date
# ------------------------------------------------------------
get_standings <- function(df, season, date) {
  d <- as.Date(date)
  
  # infer season end-year (e.g. "1990-1991" -> 1991)
  year <- suppressWarnings(as.integer(substr(season, 6, 9)))
  if (is.na(year)) {
    stop("Couldn't parse season end-year from season string: ", season)
  }
  
  points_per_win <- ifelse(year <= 1981, 2L, 3L)
  tiebreaker <- ifelse(year <= 1976, "GA", "GD")
  
  results <- df |>
    filter(season == !!season, match_date <= d)
  
  # If no games yet in that season by this date, return empty table
  if (nrow(results) == 0) {
    return(list(
      standings = tibble(),
      deductions_note = character(0),
      season = season
    ))
  }
  
  results <- results |>
    mutate(
      home_win = as.integer(home_goals > away_goals),
      draw     = as.integer(home_goals == away_goals),
      away_win = as.integer(home_goals < away_goals),
      home_pts = case_when(
        home_goals > away_goals ~ points_per_win,
        home_goals < away_goals ~ 0L,
        TRUE                    ~ 1L
      ),
      away_pts = case_when(
        away_goals > home_goals ~ points_per_win,
        away_goals < home_goals ~ 0L,
        TRUE                    ~ 1L
      )
    )
  
  home_stats <- results |>
    transmute(
      team = home_team,
      won = home_win,
      drawn = draw,
      lost = away_win,
      goals_for = home_goals,
      goals_against = away_goals,
      pts = home_pts,
      games = 1L
    )
  
  away_stats <- results |>
    transmute(
      team = away_team,
      won = away_win,
      drawn = draw,
      lost = home_win,
      goals_for = away_goals,
      goals_against = home_goals,
      pts = away_pts,
      games = 1L
    )
  
  standings <- bind_rows(home_stats, away_stats) |>
    group_by(team) |>
    summarise(
      pld = sum(games),
      won = sum(won),
      drawn = sum(drawn),
      lost = sum(lost),
      goals_for = sum(goals_for),
      goals_against = sum(goals_against),
      pts = sum(pts),
      .groups = "drop"
    ) |>
    mutate(
      GD = goals_for - goals_against,
      # Goal average: if GA==0 conceded, treat as Inf (better than any finite)
      GA = ifelse(goals_against == 0, Inf, goals_for / goals_against)
    )
  
  # Apply points deductions (your rule: apply for that season)
  deductions_this <- points_deductions |>
    filter(season == !!season)
  
  if (nrow(deductions_this) > 0) {
    standings <- standings |>
      left_join(deductions_this, by = c("team")) |>
      mutate(
        deduct = tidyr::replace_na(deduct, 0L),
        pts = pts - deduct
      )
  } else {
    standings <- standings |>
      mutate(deduct = 0L)
  }
  
  # Sort by points then tiebreaker then goals_for
  if (tiebreaker == "GD") {
    standings <- standings |>
      arrange(desc(pts), desc(GD), desc(goals_for))
  } else {
    standings <- standings |>
      arrange(desc(pts), desc(GA), desc(goals_for))
  }
  
  # Build note strings
  deductions_note <- standings |>
    filter(deduct > 0) |>
    transmute(note = paste0(team, " deducted ", deduct, " point", ifelse(deduct == 1, "", "s"))) |>
    pull(note)
  
  list(
    standings = standings,
    deductions_note = deductions_note,
    season = season,
    points_per_win = points_per_win,
    tiebreaker = tiebreaker
  )
}

# ------------------------------------------------------------
# Helper: pretty gt table with superscripts + notes
# ------------------------------------------------------------
standings_gt <- function(standings_obj) {
  st <- standings_obj$standings
  
  if (nrow(st) == 0) {
    return(gt(tibble(Message = "No matches played yet by this date for the selected season.")))
  }
  
  # Superscripts: assign 1..k in table order for teams with deductions
  st <- st |>
    mutate(
      deduct_marker = ifelse(deduct > 0, dplyr::dense_rank(ifelse(deduct > 0, team, NA)), NA_integer_)
    )
  
  # Display points with superscript markers
  st <- st |>
    mutate(
      pts_disp = ifelse(
        deduct > 0,
        paste0(pts, "<sup>", deduct_marker, "</sup>"),
        as.character(pts)
      )
    )
  
  # Choose which tiebreaker column to show
  show_cols <- c("pld", "won", "drawn", "lost", "goals_for", "goals_against")
  if (standings_obj$tiebreaker == "GD") {
    show_cols <- c(show_cols, "GD")
  } else {
    show_cols <- c(show_cols, "GA")
  }
  show_cols <- c("team", show_cols, "pts_disp")
  
  tab <- st |>
    select(all_of(show_cols)) |>
    rename(
      Team = team,
      P = pld,
      W = won,
      D = drawn,
      L = lost,
      GF = goals_for,
      GA = goals_against,
      Pts = pts_disp
    ) |>
    gt() |>
    fmt_markdown(columns = Pts) |>
    cols_align(align = "center", columns = everything()) |>
    cols_align(align = "left", columns = Team)
  
  # Footnotes under table for deductions
  if (length(standings_obj$deductions_note) > 0) {
    # Match marker numbers to notes in the same order used in table
    notes_df <- st |>
      filter(deduct > 0) |>
      distinct(team, deduct, deduct_marker) |>
      arrange(deduct_marker) |>
      mutate(note = paste0("<sup>", deduct_marker, "</sup> ", team, " deducted ", deduct, " point", ifelse(deduct == 1, "", "s")))
    
    notes_html <- paste(notes_df$note, collapse = "<br>")
    
    tab <- tab |>
      tab_source_note(source_note = html(notes_html))
  }
  
  tab
}

# ------------------------------------------------------------
# Shiny UI
# ------------------------------------------------------------
ui <- fluidPage(
  titlePanel("English top flight table on any date in history"),
  sidebarLayout(
    sidebarPanel(
      dateInput(
        inputId = "date",
        label = "Date (table will INCLUDE matches played on this date)",
        value = max(results_all$match_date, na.rm = TRUE),
        min = min(results_all$match_date, na.rm = TRUE),
        max = max(results_all$match_date, na.rm = TRUE)
      )),
    mainPanel(
      uiOutput("season_text"),
      gt_output("table")
    )
  )
)

server <- function(input, output, session) {
  standings_obj <- reactive({
    d <- as.Date(input$date)
    s <- season_for_date(results_all, d)
    
    if (is.na(s)) {
      return(list(
        standings = tibble(),
        deductions_note = character(0),
        season = NA_character_,
        points_per_win = NA_integer_,
        tiebreaker = NA_character_
      ))
    }
    
    get_standings(results_all, s, d)
  })
  
  output$season_text <- renderUI({
    obj <- standings_obj()
    if (is.na(obj$season)) return(tags$p("No matches exist on or before this date in the dataset."))
    
    tags$p(
      tags$b("Season:"), paste0(obj$season),
      " | ",
      tags$b("Date:"), as.character(as.Date(input$date))
    )
  })
  
  output$table <- render_gt({
    standings_gt(standings_obj())
  })
}

shinyApp(ui, server)
