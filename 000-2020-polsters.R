library(tidyverse)
library(lubridate)
library(tidylog)
library(knitr)

# LOAD CSV
pres_polls_2020 <- read.csv('data/president_polls.csv')

pres_nominees_2020 <- c('Biden', 'Trump')

# BUILD DF FOR POLLS 2020
pres_polls_2020 <- pres_polls_2020 %>%
  # ONLY INTERESTED IN DEM AND REPUBLICAN CANDIDATE
  filter(answer %in% pres_nominees_2020) %>%
  # REMOVE CURRENTLY UNUSED DATA
  select(-pollster_rating_id, -pollster_rating_name, -office_type, -seat_number, -seat_name, -created_at, -notes, -url, -candidate_id, -sponsor_ids, -candidate_name,
         -ranked_choice_reallocated, -nationwide_batch, -partisan) %>%
  # CONVERT DATES TO DATES
  mutate(start_date = mdy(start_date),
         end_date = mdy(end_date),
         # MERGE CANDIDATE AND PARTY
         answer = paste(answer, candidate_party, sep = '_')) %>%
  # DROP PARTY COLUMS
  select(-candidate_party) %>%
  # SPREAD DATA TO ONE ROW PER POLL
  spread(key = answer, value = pct) %>%
  # ADD SPREAD
  mutate(spread = Trump_REP/100 - Biden_DEM/100)

# EMPTY STATES CONVERTEN TO NATIONAL
pres_polls_2020$state[pres_polls_2020$state == ''] <- 'National'

save(pres_polls_2020, file = 'rda/pres_polls_2020.rda')
