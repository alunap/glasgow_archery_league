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
