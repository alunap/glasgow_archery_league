#
#### Functions generally useful across pages ####
#
library(DBI)
library(duckdb)
library(tidyverse)
library(glue)
library(xlsx)
library(here)
here::i_am("functions/utility_functions.R")

# Load the archer scores
load_archer_scores <- function(theDate) {
  conduck <- DBI::dbConnect(duckdb::duckdb(), dbdir = here("data", "data.db"))
  stmnt <- glue("SELECT e.date_of_event AS event_date, s.score, s.hits, s.golds, a.archer, a.bowstyle, a.club, a.sex, v.location
            FROM events e
               LEFT JOIN venues v ON e.venue_id = v.id
               INNER JOIN event_scores s ON e.id = s.event_id
               INNER JOIN archers a ON s.archer_id = a.id
            WHERE e.date_of_event = '{theDate}'
            ORDER BY a.archer;")
  query <- dbSendQuery(conduck, stmnt)
  scores <- dbFetch(query) |> as_tibble()
  DBI::dbDisconnect(conduck)
  return(scores)
}

load_all_archer_scores <- function() {
  conduck <- DBI::dbConnect(duckdb::duckdb(), dbdir = here("data", "data.db"))
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
  conduck <- DBI::dbConnect(duckdb::duckdb(), dbdir = here("data", "data.db"))
  stmnt <- glue("SELECT e.date_of_event AS event_date, v.location, v.town, v.postcode, v.w3w, v.lat, v.lon
            FROM events e
            LEFT JOIN venues v ON e.venue_id = v.id
            WHERE e.date_of_event = '{theDate}'
            LIMIT 1;" )
  query <- dbSendQuery(conduck, stmnt)
  venues <- dbFetch(query)
  DBI::dbDisconnect(conduck)
  return(venues)
}

# Return a data frame for the scores for a particular bowstyle and sex
score_table <- function(bow, s, thescores) {
  tbl <- thescores %>%
    filter(bowstyle == bow, sex == s) |> 
    arrange_at(c("score", "golds"), desc) |> 
    select(c("archer", "club", "score", "hits", "golds"))
  return(tbl)
}

# Load scores for each club for the given date
club_scores <- function(theclub, thescores)
  tbl <- thescores |> 
  filter(club == theclub) |> 
  arrange_at(c("score", "golds"), desc) |> 
  select(c("archer", "bowstyle", "score", "hits", "golds"))

team_results <- function(thescores) {
  thescores |> 
    select(c(club, score, hits, golds)) |> 
    group_by(club) |> 
    arrange_at(c("score", "golds"), desc) |> 
    slice_head(n=4) |> 
    summarise(across(score:golds, sum)) |> 
    arrange_at(c("score", "golds"), desc)
}
