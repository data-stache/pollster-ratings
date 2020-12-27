library(tidyverse)
library(stringr)
library(lubridate)

# CLEAR ENVIRONS
rm(list=ls())

##### PULL POLITICAL DATA ####
dat_slpli <- read.csv("data/slpli-and-party.csv")
dat_pvi <- read.csv("data/cook-pvi.csv")
load('rda/pres_results_2020_filered.rda')
states <- read.csv("data/us-census-population-data.csv")
states <- states %>% select(state, state_name)


##### SLPLI #####
dat_slpli <- dat_slpli %>%
  # MAKE DC DEMOCRAT
  mutate(SLPLI = ifelse(is.na(SLPLI), "Democrat", SLPLI),
         # ADD PARTY ABBREVIATIONS
         slpli_party = case_when(SLPLI == "Republican" ~ "R",
                                 SLPLI == "Democrat" ~ "D")) %>%
  rename(slpli = party_by) %>%
  select(state, slpli, slpli_party)


##### COOK PVI #####
# ADD STATE ABBREVIATIONS
dat_pvi <- dat_pvi %>%
  rename(state_name = State,
         pvi = PVI)
dat_pvi <- dat_pvi[,1:2]

# FILTER NA
dat_pvi <- dat_pvi %>%
  filter(!is.na(pvi))

dat_pvi$pvi <- str_replace(dat_pvi$pvi, "\\+", " ")
dat_pvi <- dat_pvi %>%
  extract(pvi, c("party", "index"), regex = "^(D|R)\\s(\\d+)") %>%
  mutate(party = ifelse(is.na(party), "I", party),
         index = as.numeric(index),
         index = ifelse(is.na(index), 0, index),
         index = ifelse(party == "D", index * (-1), index)) %>%
  rename(pvi = index,
         pvi_party = party)
dat_pvi <- dat_pvi[,c(1, 3, 2)]


##### 2020 ELECTION CLEANUP #####
# ADD STATE ABBREVIATIONS
dat_pres_results_2020 <- pres_results_2020 %>%
  select(state_name, lean_2020) %>%
  mutate(party_2020 = case_when(lean_2020 > 0 ~ 'R',
                                lean_2020 < 0 ~ 'D'))


##### BUILD POLITICAL COVID DATA SET #####
state_partisan <- states %>%
  filter(!state == "PR") %>%
  left_join(dat_slpli) %>% 
  left_join(dat_pvi) %>%
  left_join(dat_pres_results_2020) %>%
  select(state, state_name, slpli, slpli_party, pvi, pvi_party, lean_2020, party_2020)

head(state_partisan)

# APPLY CA SLPLI RATING TO DC TO POL DF
dc_ind <- state_partisan$state == "DC"
max_dem <- state_partisan %>%
  filter(slpli_party == "D") %>%
  .$slpli
max_dem <- max(max_dem, na.rm = TRUE)
state_partisan$slpli[dc_ind] <- max_dem

# STATE LEAN AVG
state_partisan <- state_partisan %>%
  mutate(slpli = ifelse(slpli_party == "D", slpli * (-1), slpli), 
         pvi = pvi / 100,
         lean_avg = (pvi + slpli + lean_2020) / 3,
         state_lean = case_when(lean_avg < 0 ~ "D",
                                lean_avg == 0 ~"I",
                                lean_avg > 0 ~ "R")) %>%
  select(state, state_name, lean_avg, state_lean, slpli, slpli_party, pvi, pvi_party, lean_2020, party_2020)

library(knitr)
state_partisan %>% arrange(lean_avg) %>% kable()

save(state_partisan, file = "rda/state_partisan.rda")


# CLEAR ENVIRONS
rm(list=ls())
