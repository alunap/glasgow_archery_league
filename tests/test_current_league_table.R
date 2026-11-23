source("functions/utility_functions.R")

score_rows <- function(event_date, club_scores) {
  data.frame(
    event_date = as.Date(event_date),
    location = "Test venue",
    club = rep(names(club_scores), lengths(club_scores)),
    score = unlist(club_scores, use.names = FALSE)
  )
}

clubs <- c("Alpha", "Bravo", "Charlie", "Delta", "Echo", "Foxtrot", "Giffnock", "UWS")

scores <- dplyr::bind_rows(
  score_rows("2026-10-18", list(
    Alpha = c(100, 90, 80, 70, 1),
    Bravo = c(100, 90, 80, 70),
    Charlie = c(100, 90, 80, 60),
    Delta = c(100, 90, 70, 60),
    Echo = c(100, 80, 70, 60),
    Foxtrot = c(100, 80, 60, 60)
  )),
  score_rows("2026-11-22", list(
    Alpha = c(100, 100, 100, 100),
    Charlie = c(95, 90, 85, 80),
    Delta = c(90, 80, 80, 70),
    Echo = c(80, 80, 80, 70),
    Foxtrot = c(75, 75, 75, 75)
  ))
)

table <- current_league_table(scores, clubs)

stopifnot(
  identical(names(table), c("Position", "Club", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Total")),
  nrow(table) == 8L,
  identical(table$Club[1:4], c("Alpha", "Charlie", "Delta", "Bravo")),
  table$Oct[table$Club == "Alpha"] == "5",
  table$Oct[table$Club == "Bravo"] == "5",
  table$Oct[table$Club == "Charlie"] == "3",
  table$Nov[table$Club == "Bravo"] == "0",
  table$Nov[table$Club == "Giffnock"] == "0",
  table$Dec[table$Club == "Alpha"] == "-",
  table$Jan[table$Club == "UWS"] == "-",
  table$Total[table$Club == "Alpha"] == 10L,
  table$Total[table$Club == "Delta"] == table$Total[table$Club == "Bravo"],
  table$Position[table$Club == "Delta"] < table$Position[table$Club == "Bravo"]
)

empty_table <- current_league_table(scores[0, ], clubs)

stopifnot(
  nrow(empty_table) == 8L,
  all(unlist(empty_table[c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar")]) == "-"),
  all(empty_table$Total == 0L)
)
