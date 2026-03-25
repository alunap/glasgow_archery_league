### Functions for loading new data each month. We use a local PostgreSQL database, but export to duckdb for production. ###

library(RPostgres)
library(DBI)
library(tidyverse)
library(glue)
library(readODS)
library(xlsx)
library(here)
here::i_am("functions/utility_functions.R")

# Load the current list of archers

connect <- function() {
  tryCatch({
      drv <- RPostgres::Postgres()
      conn <- dbConnect(drv,
                        dbname = "glasgow_archery_league",
                        host = "localhost",
                        port = 5432,
                        user = "postgres",
                        password = "strawberrySundae"
      )
  })
}

conn <- connect()

load_current_archers <- function(conn) {
  archers <- tbl(conn, "archers") |> as_tibble()
  return(archers)
}

read_openlibre <- function(fname) {
  latest <- read_ods(fname, sheet = "Results",  range = "B3:I41")
  newnames <- c("target", "archer", "club", "sex", "bowstyle", "score", "hits", "golds")
  names(latest) <- newnames
  latest <- subset(latest, select = -target)
  return(latest)
}

# Extract monthly data from spreadsheet and save to PostgreQSL
read_excel <- function(fname) {
  latest <- read.xlsx(fname, sheetName = "Results", startRow = 3, endRow = 43, colIndex = c(2, 3, 4, 5, 6, 7, 8, 9))
  newnames <- c("target", "archer", "club", "sex", "bowstyle", "score", "hits", "golds")
  names(latest) <- newnames
  latest <- subset(latest, select = -target)
  return(latest)
}


# Load data and check. 
# NOTE: don't assume the new data is fine as-is. Check the original excel file, check the new_archers tibble. Since the data is typed in
# it is easy to get spelling mistakes or variants of previous codes for sex, misspelled archer and club names, etc.
# Nov 2025 changed Antonios -> Antonis, Livingstone -> Livingston, Samual -> Sam

clubs <- c("Glasgow Archers" = "Glasgow", "EK Archery Club"="East Kilbride", "Strathclyde AC"="Strathclyde", "Monklands AC"="Monklands", "Linwood AC"="Linwood", "Orion's Archers"="Orion's", "UWS Archery Club"="UWS", "Giffnock AC" = "Giffnock")
bowstyles <- c("Com"="Compound", "Rec"="Recurve", "BB"="Barebow", "LB"="Traditional")


# change the next line to whatever is the next file to load
file_name <- here("data", "2025-6", "Glasgow League Mar 2026.ods")

results <- if(endsWith(file_name, "ods")) {
  read_openlibre(file_name)
  } else { 
    read_excel(file_name)
  }
              
these_archers <- results %>%
                  select(c("archer", "club", "bowstyle", "sex")) %>%
                  filter(archer != 0) %>%
                  mutate(sex = case_when(grepl('F', sex) ~ 'Ladies',
                                         grepl('M', sex) ~ 'Gents')) %>%
                  mutate(bowstyle = bowstyles[bowstyle]) %>%
                  mutate(club = clubs[club])

current_archers <- load_current_archers(conn) |> as_tibble()
new_archers <- anti_join(these_archers, current_archers, by=c("archer", "club", "bowstyle"))

# After checking the above, standardise and save new archers to the postgres table
# Why still use PostgreSQL? Because I can't easily visually edit a DuckDB file (eg to add new venues, which don't appear in the spreadsheets)
# without paying a licence fee. I can do this in Postgres, so that is the master copy, and we save to DuckDB from there for production.

add_new_archers <- function(new_archers, current_archers, con){
  max_id <- max(current_archers$id)
  mutate(new_archers, id=row_number() + max_id)
  dbWriteTable(con, "archers", new_archers, append = TRUE)
}

add_new_archers(new_archers, current_archers, conn)

add_new_scores <- function(results, con, event) {
  # Get the archer ids for the new scores
  current_archers <- load_current_archers(conn)
  these_scores <- results %>%
    filter(archer != 0, score != 0 ) %>%
    mutate(sex = case_when(grepl('F', sex) ~ 'Ladies',
                           grepl('M', sex) ~ 'Gents')) %>%
    mutate(bowstyle = bowstyles[bowstyle]) %>%
    mutate(club = clubs[club]) %>%
    left_join(current_archers, by=c("archer", "club", "bowstyle")) %>%
    select(archer_id = id, score, hits, golds) %>%
    mutate(event_id = event)
  
  dbWriteTable(con, "event_scores", these_scores, append = TRUE)
}

add_new_scores(results, conn, 12) # change the event id to the correct one for the new scores

DBI::dbDisconnect(conn)
