library(tidyverse)
library(tidylog)


# LOAD 2020 POLLS
load('rda/pres_polls_2020.rda')

# RENAME STATE
pres_polls_2020 <- pres_polls_2020 %>% rename(state_name = state)

# LOAD STATE NAMES AND STATE ABBREVIATIONS
states <- read.csv("data/us-census-population-data.csv")
states <- states %>% select(state, state_name)

# ADD STATE ABBREVIATIONS
pres_polls_2020 <- pres_polls_2020 %>% left_join(states)

# BUILD POLL RESULTS OF FINAL WEEK OF POLLS
poll_results <- pres_polls_2020 %>%
  filter(!state_name %in% c('National', 'Maine CD-1', 'Maine CD-2', 'Nebraska CD-2') &
           end_date >= ymd(20201026) &
           !fte_grade %in% c('D-', 'C/D')) %>%
  group_by(state_name, state) %>%
  summarize(poll_avg = mean(spread),
            poll_sd = sd(spread),
            poll_n = n())

# REPLACE NA SDs WITH MEDIAN SDs
poll_results <- poll_results %>%
  mutate(poll_sd = ifelse(is.na(poll_sd), median(poll_results$poll_sd, na.rm = TRUE), poll_sd))

# Reduce and spread
load('rda/st_lean_efx.rda')
prior <- st_lean_efx %>%
  select(state, mu, sd)

# JOIN PRIORS AND POLLS
dat <- prior %>%
  left_join(poll_results)

# STATES WITH NO POLLS
dat <- dat %>% 
  filter(state %in% poll_results$state)

dat <- dat[, c(4, 1:3, 5:7)]

# LOAD 2020 RESUTS
load('rda/state_partisan.rda')
head(state_partisan)

# 2020 SMALLER
dat_2020 <- state_partisan %>%
  select(state, lean_2020)

# ELECTORAL VOTES
data("polls_us_election_2016")
head(results_us_election_2016)

# EV SMALLER
electoral <- results_us_election_2016 %>%
  rename(state_name = state) %>%
  select(state_name, electoral_votes)

# ADD EV
dat <- dat %>%
  left_join(electoral)

load('rda/st_lean_efx.rda')
dat_no <- st_lean_efx %>%
  filter(!state %in% dat$state) %>%
  left_join(states) %>%
  left_join(electoral)
dat_no <- dat_no[, c(12, 1:11, 13)]

# SIMULATE ELECTION NIGHT
N <- 40000
biden_ev <- replicate(N, {
polled <- dat %>%
  rename(tau = sd) %>%
  mutate(bias_sd = .05,
         sigma = sqrt(poll_sd^2 / poll_n + bias_sd^2),
         B = sigma^2 / (sigma^2 + tau^2),
         posterior_mean = B * mu + (1 - B) * poll_avg,
         posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
         z = qt(.95, poll_n),
         t_dist = z * poll_sd / sqrt(poll_n),
         simulated_result = rnorm(length(posterior_mean), posterior_mean, t_dist),
         biden = ifelse(simulated_result < 0, electoral_votes, 0)) %>%
  summarize(biden = sum(biden)) %>%
  .$biden

not_polled <- dat_no %>%
  mutate(simulated_result = rnorm(length(mu), mu, t_dist),
         biden = ifelse(simulated_result < 0, electoral_votes, 0)) %>%
  summarize(biden = sum(biden)) %>%
  .$biden

polled + not_polled
})

mean(biden_ev > 269)

hist(biden_ev)

biden_ev


