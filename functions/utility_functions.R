#
#### Functions generally useful across pages ####
#


# Load the archer scores
load_archers <- function() {
  archers <- read_csv(here("data", "archer_scores.csv"), col_types = "Dccciiic")
}

load_venues <- function() {
  venues <- read_csv(here("data", "venues.csv"), col_type = "Dcccc")
}

# Return a data frame for the scores for a particular bowstyle and sex
score_table <- function(bow, s, thescores) {
  tbl <- thescores %>%
    filter(bowstyle == bow, sex == s) %>%
    arrange_at(c("score", "golds"), desc) %>%
    select(c("archer", "club", "score", "hits", "golds"))
  
  return(tbl)
}

# Load scores for each club for the given date
club_scores <- function(theclub, thescores)
  tbl <- thescores %>%
  filter(club == theclub) %>%
  arrange_at(c("score", "golds"), desc) %>%
  select(c("archer", "bowstyle", "score", "hits", "golds"))

team_results <- function(thescores) {
  thescores %>%
    select(c(club, score, hits, golds)) %>%
    group_by(club) %>%
    arrange_at(c("score", "golds"), desc) %>%
    slice_head(n=4) %>%
    summarise(across(everything(), sum)) %>%
    arrange_at(c("score", "golds"), desc)
}