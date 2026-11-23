#
#### Functions generally useful across pages ####
#
library(DBI)
library(duckdb)
library(tidyverse)
library(glue)
# library(xlsx)
library(here)

# Load the archer scores
load_archer_scores <- function(theDate) {
  conduck <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = here("data", "data.db"), read_only = TRUE)
  stmnt <- glue("SELECT e.date_of_event AS event_date, s.score, s.hits, s.golds, a.archer, a.bowstyle, a.club, a.sex, v.location
            FROM (
              SELECT id, date_of_event, venue_id
              FROM events
              WHERE abs(date_diff('day', date_of_event, DATE '{theDate}')) <= 3
              ORDER BY abs(date_diff('day', date_of_event, DATE '{theDate}')) ASC
              LIMIT 1
            ) e
               LEFT JOIN venues v ON e.venue_id = v.id
               INNER JOIN event_scores s ON e.id = s.event_id
               INNER JOIN archers a ON s.archer_id = a.id
            ORDER BY a.archer;")
  query <- dbSendQuery(conduck, stmnt)
  scores <- dbFetch(query) |> as_tibble()
  DBI::dbDisconnect(conduck)
  return(scores)
}

load_all_archer_scores <- function() {
  conduck <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = here("data", "data.db"), read_only = TRUE)
  stmnt <- "SELECT e.date_of_event AS event_date, s.score, s.hits, s.golds, a.archer, a.bowstyle, a.club, a.sex, v.location
            FROM events e
               LEFT JOIN venues v ON e.venue_id = v.id
               INNER JOIN event_scores s ON e.id = s.event_id
               INNER JOIN archers a ON s.archer_id = a.id
            ORDER BY a.archer;"
  query <- dbSendQuery(conduck, stmnt)
  scores <- dbFetch(query) |> as_tibble()
  DBI::dbDisconnect(conduck)
  return(scores)
}

venue <- function(theDate) {
  conduck <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = here("data", "data.db"), read_only = TRUE)
  stmnt <- glue("SELECT e.date_of_event AS event_date, v.location, v.town, v.postcode, v.w3w, v.lat, v.lon
            FROM (
              SELECT id, date_of_event, venue_id
              FROM events
              WHERE abs(date_diff('day', date_of_event, DATE '{theDate}')) <= 3
              ORDER BY abs(date_diff('day', date_of_event, DATE '{theDate}')) ASC
              LIMIT 1
            ) e
            LEFT JOIN venues v ON e.venue_id = v.id;")
  query <- dbSendQuery(conduck, stmnt)
  venues <- dbFetch(query)
  DBI::dbDisconnect(conduck)
  return(venues)
}

# Return a data frame for the scores for a particular bowstyle and sex
score_table <- function(bow, s, thescores, badges = FALSE) {
  tbl <- thescores |>
    filter(bowstyle == bow, sex == s) |>
    arrange_at(c("score", "golds"), desc)

  if (badges) {
    b_df <- read.csv(here("badges.csv"))
    target_style <- if (bow %in% c("Traditional", "Longbow")) "Traditional" else bow
    b_subset <- b_df[b_df$bowstyle == target_style, ]
    b_subset <- b_subset[order(b_subset$minimum), ]

    tbl <- tbl |>
      mutate(badge = if (nrow(b_subset) > 0) c("None", b_subset$badge)[findInterval(score, b_subset$minimum) + 1] else "None") |>
      select(c("archer", "club", "score", "hits", "golds", "badge"))
  } else {
    tbl <- tbl |>
      select(c("archer", "club", "score", "hits", "golds"))
  }

  return(tbl)
}

# Load scores for each club for the given date
club_scores <- function(theclub, thescores) {
  thescores |>
    filter(club == theclub) |>
    arrange_at(c("score", "golds"), desc) |>
    select(c("archer", "bowstyle", "score", "hits", "golds"))
}

team_results <- function(thescores) {
  thescores |>
    select(c(club, score, hits, golds)) |>
    group_by(club) |>
    arrange_at(c("score", "golds"), desc) |>
    slice_head(n = 4) |>
    summarise(across(score:golds, sum)) |>
    arrange_at(c("score", "golds"), desc)
}

current_league_table <- function(scores, clubs) {
  month_order <- c(10L, 11L, 12L, 1L, 2L, 3L)
  month_names <- c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar")

  event_scores <- scores |>
    mutate(score = as.numeric(score)) |>
    filter(!is.na(score), score > 0) |>
    group_by(event_date, location, club) |>
    slice_max(order_by = score, n = 4, with_ties = FALSE) |>
    summarise(event_score = sum(score), .groups = "drop") |>
    group_by(event_date, location) |>
    mutate(
      event_position = min_rank(desc(event_score)),
      points = case_when(
        event_position == 1L ~ 5L,
        event_position == 2L ~ 4L,
        event_position == 3L ~ 3L,
        event_position == 4L ~ 2L,
        event_position == 5L ~ 1L,
        TRUE ~ 0L
      )
    ) |>
    ungroup() |>
    mutate(month = lubridate::month(event_date))

  recorded_months <- unique(event_scores$month)
  monthly_points <- event_scores |>
    group_by(club, month) |>
    summarise(points = sum(points), .groups = "drop")

  month_points <- tidyr::expand_grid(club = clubs, month = month_order) |>
    left_join(monthly_points, by = c("club", "month")) |>
    mutate(points = if_else(month %in% recorded_months, coalesce(points, 0L), NA_integer_)) |>
    pivot_wider(names_from = month, values_from = points, names_prefix = "month_") |>
    rename(Oct = month_10, Nov = month_11, Dec = month_12, Jan = month_1, Feb = month_2, Mar = month_3)

  arrow_totals <- event_scores |>
    group_by(club) |>
    summarise(arrow_score = sum(event_score), .groups = "drop")

  tibble(club = clubs) |>
    left_join(month_points, by = "club") |>
    left_join(arrow_totals, by = "club") |>
    mutate(
      arrow_score = coalesce(arrow_score, 0),
      Total = rowSums(across(all_of(month_names), ~ replace_na(.x, 0L)))
    ) |>
    arrange(desc(Total), desc(arrow_score), club) |>
    mutate(
      Position = row_number(),
      across(all_of(month_names), ~ if_else(is.na(.x), "-", as.character(.x)))
    ) |>
    select(Position, Club = club, all_of(month_names), Total)
}
