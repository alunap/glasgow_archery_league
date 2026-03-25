###########################################################################################################################
### Functions for loading new data each month. We use a local PostgreSQL database, but export to duckdb for production. ###
### Before running make sure you have changed these lines as appropriate:                                               ###
### file_name                                                                                                           ###
### event id in the add_new_scores function                                                                             ###
### The lists of club names and bowstyle names, as they can vary month-to-month in the spreadsheets.                    ###
###########################################################################################################################
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

# Load data and check. 
# NOTE: don't assume the new data is fine as-is. Check the original excel file, check the new_archers tibble. Since the data is typed in
# it is easy to get spelling mistakes or variants of previous codes for sex, misspelled archer and club names, etc.
# Nov 2025 changed Antonios -> Antonis, Livingstone -> Livingston, Samual -> Sam
# NB: the next two vectors are used to standardise the club and bowstyle names in the new data, 
# so they need to be updated if there are any new clubs or bowstyles in the new data. 
# The keys are the values in the spreadsheet, and the values are the standardised names that match those in the database.
# **** Different people tend to use different names in the spreadsheets, so check ****
clubs <- c("Glasgow Archers" = "Glasgow", "EK Archery Club"="East Kilbride", "Strathclyde AC"="Strathclyde", "Monklands AC"="Monklands", "Linwood AC"="Linwood", "Orion's Archers"="Orion's", "UWS Archery Club"="UWS", "Giffnock AC" = "Giffnock")
bowstyles <- c("Com"="Compound", "Rec"="Recurve", "BB"="Barebow", "LB"="Traditional")

#### Read in the new data ####
file_name <-  file.choose()

if(endsWith(file_name, "ods")) {
    results <-  read_ods(file_name, sheet = "Results",  range = "B3:I41")
  } else { 
    results <- read.xlsx(file_name, sheetName = "Results", startRow = 3, endRow = 43, colIndex = c(2, 3, 4, 5, 6, 7, 8, 9))
  }
newnames <- c("target", "archer", "club", "sex", "bowstyle", "score", "hits", "golds")
names(results) <- newnames
results <- subset(results, select = -target)
              
these_archers <- results %>%
                  select(c("archer", "club", "bowstyle", "sex")) %>%
                  filter(archer != 0) %>%
                  mutate(sex = case_when(grepl('F', sex) ~ 'Ladies',
                                         grepl('M', sex) ~ 'Gents')) %>%
                  mutate(bowstyle = bowstyles[bowstyle]) %>%
                  mutate(club = clubs[club])

# Find any archers in the file that do not exist in the database
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

# Add the new scores to the event_scores table, with the correct event id.
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

add_new_scores(results, conn, 12) # ****change the event id to the correct one for the new scores ****

DBI::dbDisconnect(conn)
