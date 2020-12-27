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


##### BAYES - STATE LEAN PRIOR / POLLS POST / NO UNCERTAINTY #####
# BUILD POLL RESULTS OF FINAL WEEK OF POLLS
poll_results <- pres_polls_2020 %>%
  filter(!state_name %in% c('National', 'Maine CD-1', 'Maine CD-2', 'Nebraska CD-2') &
           end_date >= ymd(20201026) &
           !fte_grade %in% c('D-', 'C/D')) %>%
  group_by(state) %>%
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

# LOAD 2020 RESUTS
load('rda/state_partisan.rda')
head(state_partisan)

# 2020 SMALLER
dat_2020 <- state_partisan %>%
  select(state, lean_2020)

# Build Posterior
posterior_2020 <- dat %>%
  mutate(sigma = poll_sd / sqrt(poll_n),
         B = sigma^2 / (sigma^2 + sd^2),
         posterior_mean = B * mu + (1 - B) * poll_avg,
         posterior_se = sqrt( 1 / (1/sigma^2 + 1/sd^2)),
         post_start = posterior_mean - qnorm(.95) * posterior_se,
         post_end = posterior_mean + qnorm(.95) * posterior_se,
         z = qt(.95, poll_n),
         t_dist = z * poll_sd / sqrt(poll_n),
         z_start = posterior_mean - t_dist,
         z_end = posterior_mean + t_dist) %>%
  left_join(dat_2020) %>%
  mutate(hit = ifelse(lean_2020 >= post_start & lean_2020 <= post_end, TRUE, FALSE))

# AVERAGE HIT
mean(posterior_2020$hit)

P_no_error <- posterior_2020 %>%
  ggplot(aes(x = state, ymin = z_start, ymax = z_end)) + 
  geom_hline(yintercept = 0, color = 'grey40') +
  geom_errorbar() +
  geom_point(aes(y = lean_2020)) +
  coord_flip()


##### BAYES - STATE LEAN PRIOR / POLLS POST / WITH UNCERTAINTY #####
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
  group_by(state) %>%
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

# LOAD 2020 RESUTS
load('rda/state_partisan.rda')
head(state_partisan)

# 2020 SMALLER
dat_2020 <- state_partisan %>%
  select(state, lean_2020)

sigma = sqrt(sd^2/(n) + bias_sd^2)

# Build Posterior
posterior_2020_error <- dat %>%
  rename(tau = sd) %>%
  mutate(bias_sd = .05,
         sigma = sqrt(poll_sd^2 / poll_n + bias_sd^2),
         B = sigma^2 / (sigma^2 + tau^2),
         posterior_mean = B * mu + (1 - B) * poll_avg,
         posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
         post_start = posterior_mean - qnorm(.95) * posterior_se,
         post_end = posterior_mean + qnorm(.95) * posterior_se,
         z = qt(.95, poll_n),
         t_dist = z * poll_sd / sqrt(poll_n),
         z_start = posterior_mean - t_dist,
         z_end = posterior_mean + t_dist) %>%
  left_join(dat_2020) %>%
  mutate(hit = ifelse(lean_2020 >= post_start & lean_2020 <= post_end, TRUE, FALSE))

# AVERAGE HIT
mean(posterior_2020_error$hit)

P_err <- posterior_2020_error %>%
  ggplot(aes(x = state, ymin = z_start, ymax = z_end)) + 
  geom_hline(yintercept = 0, color = 'grey40') +
  geom_errorbar() +
  geom_point(aes(y = lean_2020)) +
  coord_flip()

library(gridExtra)
grid.arrange(P_no_error, P_err, ncol = 1)

head(posterior_2020)
head(posterior_2020_error)









load('rda/elections.rda')
head(elections)

partisan <- elections %>%
  # Only General Elections, Only top ticket candidates
  filter(type == 'GE' & !party == 'other' & !state %in% c('AS', 'GU', 'MP', 'PR', 'VI') & year >= 2002) %>%
  # Calculate Spread
  group_by(state, year) %>%
  summarize(spread = share_vote[2] - share_vote[1]) %>%
  ungroup() %>%
  # Calculate Average Spread and Sample Size
  group_by(state) %>%
  summarize(mu = mean(spread),
            sd = sd(spread),
            n = n()) %>%
  # Calculate SE and 95% Confidence Interval
  mutate(se = sd / sqrt(n),
         low_ci = mu - (qnorm(.975) * se),
         hi_ci = mu + (qnorm(.975) * se),
         z = qt(0.975, n - 1),
         t_dist = z * sd / sqrt(n),
         low_moe = mu - t_dist,
         high_moe = mu + t_dist)

partisan %>%
  filter(state %in% posterior_2020$state) %>%
  left_join(dat_2020) %>%
  mutate(hit = ifelse(lean_2020 >= low_moe & lean_2020 <= high_moe, TRUE, FALSE)) %>%
  ggplot(aes(x = state, ymin = low_moe, ymax = high_moe)) + 
  geom_hline(yintercept = 0, color = 'grey40') +
  geom_errorbar() +
  geom_point(aes(y = lean_2020)) +
  coord_flip()
  













