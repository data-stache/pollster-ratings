library(tidyverse)
library(tidylog)

load('rda/elections.rda')

head(elections)

##### st_lean_efx TABLE #####
st_lean_efx <- elections %>%
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

head(st_lean_efx)
save(st_lean_efx, file = 'rda/st_lean_efx.rda')


##### LINEAR MODEL - STATE st_lean_efx SHIFT #####
library(broom)

# EV STATES
states <- st_lean_efx$state

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

# LOAD 2020 RESUTS
load('rda/state_partisan.rda')
head(state_partisan)

# 2020 SMALLER
dat_2020 <- state_partisan %>%
  select(state, lean_2020)

# GRAPH RESULTS VS EST AB LINE
fit %>%
  group_by(state) %>%
  summarize(est_2020 = estimate[1] + (2020 * estimate[2])) %>%
  left_join(dat_2020) %>%
  ggplot(aes(x = est_2020, y = lean_2020, label = state)) +
  geom_abline() +
  geom_hline(yintercept = 0, size = .5, color = 'grey60') +
  geom_vline(xintercept = 0, size = .5, color = 'grey60') +
  geom_point() +
  scale_x_continuous(breaks = seq(-1,1,.05)) +
  scale_y_continuous(breaks = seq(-1,1,.05)) +
  ggrepel::geom_text_repel()

fit %>%
  filter(state == 'TX') %>%
  summarize(est_2020 = estimate[1] + (2020 * estimate[2]),
            est_2022 = estimate[1] + (2022 * estimate[2]),
            est_2024 = estimate[1] + (2024 * estimate[2]),
            est_2026 = estimate[1] + (2026 * estimate[2]),
            est_2028 = estimate[1] + (2028 * estimate[2]),
            est_2030 = estimate[1] + (2030 * estimate[2]))
