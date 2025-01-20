#
#### Functions generally useful across pages ####
#
library(DBI)
library(duckdb)
library(tidyverse)
library(dbplyr)
library(glue)
here::i_am("functions/utility_functions.R")
library(here)


# Load the archer scores
load_archers <- function(theDate) {
  conduck <- DBI::dbConnect(duckdb::duckdb(), dbdir = here("data", "data.db"))
  stmnt <- glue("SELECT e.event_date, e.score, e.hits, e.golds, a.archer, a.bowstyle, a.club, a.sex, v.location
            FROM events e
            LEFT JOIN archers a ON a.id = e.archer_id
            LEFT JOIN venues v ON  e.venue_id = v.id
            WHERE e.event_date = '{theDate}'
            ORDER BY archer;")
  query <- dbSendQuery(conduck, stmnt)
  scores <- dbFetch(query) |> as_tibble()
  DBI::dbDisconnect(conduck)
  return(scores)
}

load_all_archers <- function() {
  conduck <- DBI::dbConnect(duckdb::duckdb(), dbdir = here("data", "data.db"))
  stmnt <- "SELECT e.event_date, e.score, e.hits, e.golds, a.archer, a.bowstyle, a.club, a.sex, v.location
            FROM events e
            LEFT JOIN archers a ON a.id = e.archer_id
            LEFT JOIN venues v ON  e.venue_id = v.id
            ORDER BY archer;"
  query <- dbSendQuery(conduck, stmnt)
  scores <- dbFetch(query) |> as_tibble()
  DBI::dbDisconnect(conduck)
  return(scores)
}

venue <- function(theDate) {
  conduck <- DBI::dbConnect(duckdb::duckdb(), dbdir = here("data", "data.db"))
  stmnt <- glue("SELECT e.event_date, v.location, v.town, v.postcode, v.w3w, v.lat, v.lon
            FROM events e
            LEFT JOIN venues v ON e.venue_id = v.id
            WHERE e.event_date = '{theDate}'
            LIMIT 1" )
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


