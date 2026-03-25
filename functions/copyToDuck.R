# Copy data from Postgres to DuckDB
# When new data is received run through import_data.R first to check the data and add any new archers and scores to Postgres.
# Then run this script to make a local DuckDB copy once that is all checked.

# New format, now we just read the three tables and write out to duckDB
library(DBI)
library(RPostgres)
library(duckdb)
library(tidyverse)
library(here)
here::i_am("functions/copyToDuck.R")

# Code to rerun each month to populate the data file from Postgres
conn <- dbConnect(RPostgres::Postgres(),
                  dbname = "glasgow_archery_league",
                  host = "127.0.0.1",
                  port = 5432,
                  user = "postgres",
                  password = "strawberrySundae"
)

## Read in new tables from Postgres
venues <- dbReadTable(conn, "venues")
archers <- dbReadTable(conn, "archers")
events <- dbReadTable(conn, "events")
event_scores <- dbReadTable(conn, "event_scores")
DBI::dbDisconnect(conn)

## Write to DuckDB
conduck <- DBI::dbConnect(duckdb::duckdb(), dbdir = here("data", "data.db"))
dbWriteTable(conduck, "venues", venues, overwrite = TRUE)
dbWriteTable(conduck, "archers", archers, overwrite = TRUE)
dbWriteTable(conduck, "events", events, overwrite = TRUE)
dbWriteTable(conduck, "event_scores", event_scores, overwrite = TRUE)
DBI::dbDisconnect(conduck)
print("Data written to DuckDB")
