library(tidyverse)
library(tidylog)

load('rda/elections.rda')

head(elections)

##### STATE LEAN TABLE #####
st_lean <- elections %>%
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
         z = qt(0.975, n - 1),
         t_dist = z * sd / sqrt(n),
         low_moe = mu - t_dist,
         high_moe = mu + t_dist,
         battle = ifelse(low_moe < 0 & high_moe > 0, TRUE, FALSE))

##### STATE LEAN FORECAST - LINEAR MODEL #####
library(broom)

# EV STATES
states <- st_lean$state

# FUNCTION TO MAP st_lean_efx TREND TO TABLE
fit <- map_df(states, function(st) {
  fit <- elections %>%
    # Only General Elections, Only top ticket candidates
    filter(type == 'GE' & !party == 'other' & state == st) %>%
    # Calculate Spread
    group_by(state, year) %>%
    summarize(spread = share_vote[2] - share_vote[1]) %>%
    do(tidy(lm(spread ~ year, data = .)))
})

fit_test <- elections %>%
  # Only General Elections, Only top ticket candidates
  filter(type == 'GE' & !party == 'other') %>%
  # Calculate Spread
  group_by(state, year) %>%
  summarize(spread = share_vote[2] - share_vote[1]) %>%
  ungroup() %>%
  group_by(state) %>%
  do(fit = lm(spread ~ year, data = .))

head(fit)

# LOAD 2020 RESUTS
load('rda/state_partisan.rda')
head(state_partisan)

# 2020 SMALLER
dat_2020 <- state_partisan %>%
  select(state, lean_2020)

fit %>%
  group_by(state) %>%
  summarise(intercept = estimate[1],
           slope = estimate[2],
           se = std.error[2]) %>%
  mutate(x2020 = intercept + (slope*2020),
         x2020_low = x2020 - qnorm(.975) * se,
         x2020_high = x2020 + qnorm(.975) * se) %>%
  left_join(dat_2020) %>%
  ggplot(aes(x = state, y = x2020, ymin = x2020_low, ymax = x2020_high)) +
  geom_hline(yintercept = 0) +
  geom_errorbar() +
  geom_point(alpha = .5) +
  geom_point(aes(y = lean_2020), color = 'red') +
  coord_flip()
