# Test code for dealing with data
# We want to extract the original data, which was in the archer_scores table and the original venue table (renamed 'stages')
# and split this out into 3NF, with a venues table, an archers table, and an events table that has the scores and likes to
# the venue where the stage took place, and the archer details of those taking part.
# This is a one-off conversion of original flat data format to a relational format.

# New format, now we just read the three tables and write out to duckDB
library(DBI)
library(RPostgres)
library(duckdb)
library(tidyverse)
library(here)
here::i_am("setup.R")

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

########################################################################

##### Test code #####
dbListTables(conn)
dbListFields(conn, "archer_scores")

scores_df <- dbReadTable(conn, "archer_scores") |> as_tibble()
stages_df <- dbReadTable(conn, "stages") |> as_tibble()

# selects
latest_q <- dbSendQuery(conn, "SELECT * FROM archer_scores WHERE event_date = '2025-03-22' ")
latest <- dbFetch(latest_q)
dbClearResult(latest)


# scores per club to date
scores_df |>
  count(club, wt = score)

# Create a venues table
venues <- stages_df |>
  select(location = venue_name, town, postcode) |>
  mutate(id = row_number(), .before = 1) |>
  filter(id < 3) |>
  mutate(w3w = ifelse(postcode == "ML5 1DL", "hoping.gloves.shins", "fluid.trip.tanks")) |>
  mutate(lat = ifelse(postcode == "G77 6NQ", "55.780977", "55.859699")) |>
  mutate(lon = ifelse(postcode == "G77 6NQ", "-4.329692", "-4.034571"))

# Create an archers table
# id, first_name, last_name, club, sex, bowstyle
archers <- scores_df |>
  select(archer, club, bowstyle, sex) |>
  group_by(archer) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(id = row_number(), .before = 1)

# Create a events table
events <- scores_df |>
  select(event_date, archer, score, hits, golds) |>
  left_join(archers, join_by(archer)) |>
  select(event_date, archer_id = id, score, hits, golds) |>
  mutate(venue_id = ifelse(event_date == "2024-10-20", 1, 2))

# Check using the new 3NF setup, create the original scores table
query <- "SELECT e.event_date, e.score, e.hits, e.golds, a.archer, a.bowstyle, a.club, a.sex, v.location
FROM events e
LEFT JOIN archers a ON a.id = e.archer_id
LEFT JOIN venues v ON  e.venue_id = v.id
ORDER BY archer;"
scores <- dbSendQuery(conduck)


## Write to PostgreSQL
dbWriteTable(conn, "events", events)
dbWriteTable(conn, "archers", archers)
dbWriteTable(conn, "venues", venues)
DBI::dbDisconnect(conn)


# Check using the new 3NF setup, create the original scores table
txt <- "SELECT e.event_date, e.score, e.hits, e.golds, a.archer, a.bowstyle, a.club, a.sex, v.location
FROM events e
LEFT JOIN archers a ON a.id = e.archer_id
LEFT JOIN venues v ON  e.venue_id = v.id
WHERE e.event_date = '2024-11-24'
ORDER BY archer;"
query <- dbSendQuery(conduck, txt)
scores <- dbFetch(query) |> as_tibble()

DBI::dbDisconnect(conduck)


## Test code for a rank table
query <- dbSendQuery(conn, txt)
source(here("functions", "utility_functions.R"))
scores <- load_all_archers()
clubs <- tibble(unique(scores$club), 0)

dates <- unique(scores$event_date)
for (theDate in dates) {
  theScores <- filter(scores, event_date == theDate)
  df <- theScores |>
    select(!c(event_date, location)) |>
    team_results() |>
    rowid_to_column() |>
    mutate(rank = case_when(
      rowid == 1 ~ 5,
      rowid == 2 ~ 4,
      rowid == 3 ~ 3,
      rowid == 4 ~ 2,
      rowid == 5 ~ 1,
      TRUE ~ 0
    ))
  summarise(group_by(club), sum(rank))
}


df <- theScores |>
  select(!c(event_date, location)) |>
  team_results() |>
  rowid_to_column() |>
  mutate(rank = case_when(
    rowid == 1 ~ 5,
    rowid == 2 ~ 4,
    rowid == 3 ~ 3,
    rowid == 4 ~ 2,
    rowid == 5 ~ 1,
    TRUE ~ 0
  )) |>
  rowwise() |>
  mutate(total = sum(rank))


select(scores, !c(event_date, location))

event_date <- ymd(20241124)
scores <- load_archers(event_date)

uws <- club_scores("UWS", scores)
