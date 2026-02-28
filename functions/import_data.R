### Functions for loading new data each month. We use a local PostgreSQL database, but export to duckdb for production. ###

library(RPostgres)
library(DBI)
library(tidyverse)
library(glue)
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



# Extract monthly data from spreadsheet and save to PostgreQSL
#read_excel <- function(fname) {
#  latest <- read.xlsx(fname, sheetName = "Results", startRow = 3, endRow = 43, colIndex = c(2, 3, 4, 5, 6, 7, 8, 9))
#  newnames <- c("target", "archer", "club", "sex", "bowstyle", "score", "hits", "golds")
#  names(latest) <- newnames
#  latest <- subset(latest, select = -target)
#  return(latest)
#}
read_excel <- function(fname) {
  latest <- read.xlsx(fname, sheetName = "Sheet1", startRow = 1, endRow = 42, colIndex = c(1, 2, 3, 4, 5, 6))
  newnames <- c("target", "archer", "sex", "club", "bowstyle", "score")
  names(latest) <- newnames
  latest <- subset(latest, select = -target)
  return(latest)
}


# Load data and check. 
# NOTE: don't assume the new data is fine as-is. Check the original excel file, check the new_archers tibble. Since the data is typed in
# it is easy to get spelling mistakes or variants of previous codes for sex, misspelled archer and club names, etc.
# Nov 2025 changed Antonios -> Antonis, Livingstone -> Livingston, Samual -> Sam

clubs <- c("GA" = "Glasgow", "EK"="East Kilbride", "USAC"="Strathclyde", "MAC"="Monklands", "LAC"="Linwood", "O"="Orion's", "UWSAC"="UWS")
bowstyles <- c("Compound"="Compound", "Recurve"="Recurve", "Barebow"="Barebow", "Traditional"="Traditional")

# change the next line to whatever is the next file to load
excel_file <- here("data", "2025-6", "Final GLM5.xlsx")
results <- read_excel(excel_file)
these_archers <- results %>%
                  select(c("archer", "club", "bowstyle", "sex")) %>%
                  filter(archer != 0)  %>%
                  mutate(sex = case_when(grepl('F', sex) ~ 'Ladies',
                                         grepl('M', sex) ~ 'Gents')) %>%
                  mutate(bowstyle = bowstyles[bowstyle]) %>%
                  mutate(club = clubs[club])

current_archers <- load_current_archers(conn)
new_archers <- anti_join(these_archers, current_archers, by="archer")

# After checking the above, standardise and save new archers to the postgres table
# Why still use PostgreSQL? Because I can't easily visually edit a DuckDB file (eg to add new venues, which don't appear in the spreadsheets)
# without paying a licence fee. I can do this in Postgres, so that is the master copy, and we save to DuckDB from there for production.

add_new_archers <- function(new_archers, current_archers, con){
  max_id <- max(current_archers$id)
  mutate(new_archers, id=row_number() + max_id)
  dbWriteTable(con, "archers", new_archers, append = TRUE)
}

if(nrow(new_archers) > 0) {
  add_new_archers(new_archers, current_archers, conn)
}

add_new_scores <- function(results, con) {
  # Get the archer ids for the new scores
  current_archers <- load_current_archers(conn)
  these_scores <- results %>%
    left_join(current_archers, by="archer") %>%
    select(archer_id = id, score) %>%
    mutate(event_id = 11) # change this to the correct event id for the new scores
  
  dbWriteTable(con, "event_scores", these_scores, append = TRUE)
}
  
add_new_scores(results, conn)
  
DBI::dbDisconnect(conn)
