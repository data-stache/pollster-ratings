{# Libraries
  library(tidyverse)
  library(tidylog)
  library(broom)
}
options(scipen = 999)

# Load Election Results 1976 - 2016
results <- read.csv("data/potus_results_76_16.csv", stringsAsFactors = FALSE, header = TRUE)

# Fill NAs with 0s
results$other[is.na(results$other)] <- 0

# Calculate Spreads
results <- results %>%
  mutate(spread = rep - dem) %>%
  select(state, year, total_votes, spread)

# State LMs
results %>%
  ggplot(aes(x = year, y = spread)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth(method = 'lm') +
  coord_flip() +
  scale_x_reverse(breaks = seq(1976, 2020, 4)) +
  facet_wrap(. ~ state)

# State Vector
states <- unique(results$state)

fit_states <- map_df(states, function(ST) {
  fit <- results %>%
    select(state, year, spread) %>%
    filter(state == ST) %>%
    lm(spread ~ year, data = .)
  
  data <- data.frame(state = paste(ST),
                     year = 2020)
  
  pred <- predict.lm(fit, data, se.fit = TRUE)
  
  data.frame(state = paste(ST),
             year = 2020,
             pred_spread = pred$fit,
             pred_se = pred$se.fit,
             pred_df = pred$df,
             pred_res = pred$residual.scale)
})

head(fit_states)

row.names(fit_states) <- c()

fit_states <- fit_states %>%
  mutate(mu = pred_spread,
         z = qt(0.975, pred_df),
         t_dist = z * pred_se,
         start = mu - t_dist,
         end = mu + t_dist) %>%
  select(state, year, mu, z, t_dist, start, end)

save(fit_states, file = 'rda/fit_states.rda')

# Corelation Matrix ---------------------------------------------
head(results)

dat <- results %>%
  select(state, year, spread)

columns <- unique(dat$state)
rows <- unique(dat$year)

dat <- map_df(rows, function(t) {
  year <- dat$spread[dat$year == t]
  data.frame(t(year))
})

row.names(dat) <- rows
colnames(dat) <- columns

C <- cor(dat)

library(corrplot)

corrplot(C, method = "pie", type = "upper", order = "hclust")



# Graphics ------------------------------------------------------
# LOAD 2020 RESUTS
load('rda/state_partisan.rda')
head(state_partisan)

# 2020 SMALLER
dat_2020 <- state_partisan %>%
  select(state, lean_2020)

state_lean_ord <- fit_states %>% arrange(mu) %>% .$state

fit_states %>%
  left_join(dat_2020) %>%
  mutate(state = factor(state, levels = state_lean_ord)) %>%
  ggplot(aes(x = state, ymin = start, ymax = end, y = mu)) +
  geom_hline(yintercept = 0) +
  geom_errorbar() +
  geom_point() +
  geom_point(aes(y = lean_2020), color = 'red') +
  coord_flip()

battle_states <- fit_states %>% filter(start < 0 & end > 0) %>% arrange(mu) %>% .$state

fit_states %>%
  left_join(dat_2020) %>%
  filter(state %in% battle_states) %>%
  mutate(state = factor(state, levels = battle_states)) %>%
  ggplot(aes(x = state, ymin = start, ymax = end, y = mu)) +
  geom_hline(yintercept = 0) +
  geom_errorbar() +
  geom_point() +
  geom_point(aes(y = lean_2020), color = 'red') +
  coord_flip()


# State LMs
results %>%
  select(state, year, spread) %>%
  rbind(fit_states %>% select(state, year, spread = mu)) %>%
  filter(state %in% c('TX')) %>%
  ggplot(aes(x = year, y = spread)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth(method = 'lm') +
  coord_flip() +
  scale_x_reverse(breaks = seq(1976, 2020, 4)) +
  facet_wrap(. ~ state)










