# 2020 ELECTION RESULTS

library(tidyverse)
library(rvest)
library(tidylog)

url <- 'https://en.wikipedia.org/wiki/2020_United_States_presidential_election'
h <- read_html(url)

# XML NODE TABLE
pres_results_2020 <- h %>% html_nodes('table')

# HTML TO TABLE
pres_results_2020 <- pres_results_2020[[32]] %>% html_table(fill = TRUE)

# RENAME COLUMNS
pres_results_2020 <- pres_results_2020 %>% setNames(c('state', 'Biden_DEM_vote', 'Biden_DEM_share', 'Biden_DEM_EV', 'Trump_REP_vote', 'Trump_REP_share', 'Trump_REP_EV', 'Jorgensen_LIB_vote', 'Jorgensen_LIB_share',
                                                      'Jorgensen_LIB_EV', 'Hawkins_GRN_vote', 'Hawkins_GRN_share', 'Hawkins_GRN_EV', 'Other_vote', 'Other_share', 'Other_EV', 'Margin_vote', 'Margin_share', 'total_votes', 
                                                      'sources'))

# OMIT ROW 1 - DUPLICATE OF HEADER
pres_results_2020 <- pres_results_2020[c(-1, -58, -59),]

head(pres_results_2020)

pres_results_2020 <- pres_results_2020 %>%
  select(-Margin_vote, -Margin_share, -total_votes, -sources) %>%
  mutate(Biden_DEM_vote = as.numeric(gsub(",", "", Biden_DEM_vote)),
         Biden_DEM_share = as.numeric(gsub("%", "", Biden_DEM_share)),
         Biden_DEM_EV = ifelse(is.na(as.numeric(Biden_DEM_EV)), 0, as.numeric(Biden_DEM_EV)),
         Trump_REP_vote = as.numeric(gsub(",", "", Trump_REP_vote)),
         Trump_REP_share = as.numeric(gsub("%", "", Trump_REP_share)),
         Trump_REP_EV = ifelse(is.na(as.numeric(Trump_REP_EV)), 0, as.numeric(Trump_REP_EV)),
         Jorgensen_LIB_vote = ifelse(is.na(as.numeric(Jorgensen_LIB_vote)), 0, as.numeric(Jorgensen_LIB_vote)),
         Jorgensen_LIB_share = as.numeric(gsub("%", "", Jorgensen_LIB_share)),
         Jorgensen_LIB_EV = ifelse(is.na(as.numeric(Jorgensen_LIB_EV)), 0, as.numeric(Jorgensen_LIB_EV)),
         Hawkins_GRN_vote = ifelse(is.na(as.numeric(Hawkins_GRN_vote)), 0, as.numeric(Hawkins_GRN_vote)),
         Hawkins_GRN_share = ifelse(is.na(as.numeric(gsub("%", "", Hawkins_GRN_share))), 0, as.numeric(gsub("%", "", Hawkins_GRN_share))),
         Hawkins_GRN_EV = ifelse(is.na(as.numeric(Hawkins_GRN_EV)), 0, as.numeric(Hawkins_GRN_EV)),
         Other_vote = ifelse(is.na(as.numeric(Other_vote)), 0, as.numeric(Other_vote)),
         Other_share = ifelse(is.na(as.numeric(gsub("%", "", Other_share))), 0, as.numeric(gsub("%", "", Other_share))),
         Other_EV = ifelse(is.na(as.numeric(Other_EV)), 0, as.numeric(Other_EV)),
         Margin_vote_DvR = Trump_REP_vote - Biden_DEM_vote,
         Margin_share_DvR = Trump_REP_share / 100 - Biden_DEM_share / 100,
         total_vote = Biden_DEM_vote + Trump_REP_vote + Jorgensen_LIB_vote + Hawkins_GRN_vote + Other_vote)

pres_results_2020$state[pres_results_2020$state == 'Ala.'] <- 'Alabama'
pres_results_2020$state[pres_results_2020$state == 'Ariz.'] <- 'Arizona'
pres_results_2020$state[pres_results_2020$state == 'Ark.'] <- 'Arkansas'
pres_results_2020$state[pres_results_2020$state == 'Calif.'] <- 'California'
pres_results_2020$state[pres_results_2020$state == 'Colo.'] <- 'Colorado'
pres_results_2020$state[pres_results_2020$state == 'Conn.'] <- 'Connecticut'
pres_results_2020$state[pres_results_2020$state == 'Del.'] <- 'Delaware'
pres_results_2020$state[pres_results_2020$state == 'D.C.'] <- 'District of Columbia'
pres_results_2020$state[pres_results_2020$state == 'Fla.'] <- 'Florida'
pres_results_2020$state[pres_results_2020$state == 'Ga.'] <- 'Georgia'
pres_results_2020$state[pres_results_2020$state == 'Ill.'] <- 'Illinois'
pres_results_2020$state[pres_results_2020$state == 'Ind.'] <- 'Indiana'
pres_results_2020$state[pres_results_2020$state == 'Kan.'] <- 'Kansas'
pres_results_2020$state[pres_results_2020$state == 'Ky.'] <- 'Kentucky'
pres_results_2020$state[pres_results_2020$state == 'La.'] <- 'Louisiana'
pres_results_2020$state[pres_results_2020$state == 'Maine †'] <- 'Maine'
pres_results_2020$state[pres_results_2020$state == 'ME-1'] <- 'Maine CD-1'
pres_results_2020$state[pres_results_2020$state == 'ME-2'] <- 'Maine CD-2'
pres_results_2020$state[pres_results_2020$state == 'Md.'] <- 'Maryland'
pres_results_2020$state[pres_results_2020$state == 'Mass.'] <- 'Massachusetts'
pres_results_2020$state[pres_results_2020$state == 'Mich.'] <- 'Michigan'
pres_results_2020$state[pres_results_2020$state == 'Minn.'] <- 'Minnesota'
pres_results_2020$state[pres_results_2020$state == 'Miss.'] <- 'Mississippi'
pres_results_2020$state[pres_results_2020$state == 'Mo.'] <- 'Missouri'
pres_results_2020$state[pres_results_2020$state == 'Mont.'] <- 'Montana'
pres_results_2020$state[pres_results_2020$state == 'Neb. †'] <- 'Nebraska'
pres_results_2020$state[pres_results_2020$state == 'NE-1'] <- 'Nebraska CD-1'
pres_results_2020$state[pres_results_2020$state == 'NE-2'] <- 'Nebraska CD-2'
pres_results_2020$state[pres_results_2020$state == 'NE-3'] <- 'Nebraska CD-3'
pres_results_2020$state[pres_results_2020$state == 'Nev.'] <- 'Nevada'
pres_results_2020$state[pres_results_2020$state == 'N.H.'] <- 'New Hampshire'
pres_results_2020$state[pres_results_2020$state == 'N.J.'] <- 'New Jersey'
pres_results_2020$state[pres_results_2020$state == 'N.M.'] <- 'New Mexico'
pres_results_2020$state[pres_results_2020$state == 'N.Y.'] <- 'New York'
pres_results_2020$state[pres_results_2020$state == 'N.C.'] <- 'North Carolina'
pres_results_2020$state[pres_results_2020$state == 'N.D.'] <- 'North Dakota'
pres_results_2020$state[pres_results_2020$state == 'Okla.'] <- 'Oklahoma'
pres_results_2020$state[pres_results_2020$state == 'Ore.'] <- 'Oregon'
pres_results_2020$state[pres_results_2020$state == 'Pa.'] <- 'Pennsylvania'
pres_results_2020$state[pres_results_2020$state == 'R.I.'] <- 'Rhode Island'
pres_results_2020$state[pres_results_2020$state == 'S.C.'] <- 'South Carolina'
pres_results_2020$state[pres_results_2020$state == 'S.D.'] <- 'South Dakota'
pres_results_2020$state[pres_results_2020$state == 'Tenn.'] <- 'Tennessee'
pres_results_2020$state[pres_results_2020$state == 'Okla.'] <- 'Oklahoma'
pres_results_2020$state[pres_results_2020$state == 'Vt.'] <- 'Vermont'
pres_results_2020$state[pres_results_2020$state == 'Va.'] <- 'Virginia'
pres_results_2020$state[pres_results_2020$state == 'Wash.'] <- 'Washington'
pres_results_2020$state[pres_results_2020$state == 'W.Va.'] <- 'West Virginia'
pres_results_2020$state[pres_results_2020$state == 'Wis.'] <- 'Wisconsin'
pres_results_2020$state[pres_results_2020$state == 'Wyo.'] <- 'Wyoming'

pres_results_2020 <- pres_results_2020 %>%
  select(state, Biden_DEM_vote, Biden_DEM_share, Trump_REP_vote, Trump_REP_share, Margin_vote_DvR, Margin_share_DvR)

pres_results_2020$state[pres_results_2020$state %in% c('Nebraska', 'Nebraska CD-1', 'Nebraska CD-2', 'Nebraska CD-3')] <- 'Nebraska'
pres_results_2020$state[pres_results_2020$state %in% c('Maine', 'Maine CD-1', 'Maine CD-2')] <- 'Maine'

Nebraska <- pres_results_2020 %>%
  filter(state == 'Nebraska') %>%
  group_by(state) %>%
  summarize(Biden_DEM_vote = sum(Biden_DEM_vote),
            Trump_REP_vote = sum(Trump_REP_vote),
            Margin_vote_DvR = Trump_REP_vote - Biden_DEM_vote,
            Biden_DEM_share = Biden_DEM_vote / (Biden_DEM_vote + Trump_REP_vote),
            Trump_REP_share = Trump_REP_vote / (Biden_DEM_vote + Trump_REP_vote),
            Margin_share_DvR = Trump_REP_share - Biden_DEM_share) %>%
  select(state, Biden_DEM_vote, Biden_DEM_share, Trump_REP_vote, Trump_REP_share, Margin_vote_DvR, Margin_share_DvR)

Maine <- pres_results_2020 %>%
  filter(state == 'Maine') %>%
  group_by(state) %>%
  summarize(Biden_DEM_vote = sum(Biden_DEM_vote),
            Trump_REP_vote = sum(Trump_REP_vote),
            Margin_vote_DvR = Trump_REP_vote - Biden_DEM_vote,
            Biden_DEM_share = Biden_DEM_vote / (Biden_DEM_vote + Trump_REP_vote),
            Trump_REP_share = Trump_REP_vote / (Biden_DEM_vote + Trump_REP_vote),
            Margin_share_DvR = Trump_REP_share - Biden_DEM_share) %>%
  select(state, Biden_DEM_vote, Biden_DEM_share, Trump_REP_vote, Trump_REP_share, Margin_vote_DvR, Margin_share_DvR)

pres_results_2020 <- pres_results_2020 %>%
  filter(!state %in% c('Nebraska', 'Maine')) %>%
  mutate(Biden_DEM_share = Biden_DEM_share / 100,
         Trump_REP_share = Trump_REP_share / 100) %>%
  rbind(Nebraska, Maine) %>%
  rename(state_name = state,
         lean_2020 = Margin_share_DvR) %>%
  arrange(state_name)

head(pres_results_2020)

save(pres_results_2020, file = 'rda/pres_results_2020_filered.rda')


