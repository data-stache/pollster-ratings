{
  library(tidyverse)
  library(tidylog)
}

library(dslabs)
polls <- polls_us_election_2016 %>%
  filter(!state == 'U.S.') %>%
  mutate(poll_spread = rawpoll_trump / 100 - rawpoll_clinton / 100,
         rawpoll_other = ifelse(is.na(rawpoll_johnson), 0, rawpoll_johnson) + ifelse(is.na(rawpoll_mcmullin), 0, rawpoll_mcmullin)) %>%
  select(state,
         start_date = startdate,
         end_date = enddate,
         pollster, grade,
         sample_size = samplesize,
         population,
         rawpoll_dem = rawpoll_clinton,
         rawpoll_rep = rawpoll_trump,
         rawpoll_other, 
         poll_spread)

results <- results_us_election_2016 %>%
  mutate(act_spread = trump / 100 - clinton / 100) %>%
  select(state, act_spread)

head(polls)

polls %>%
  left_join(results) %>%
  group_by(state, pollster) %>%
  summarize(mu = mean(poll_spread),
            sd = sd(poll_spread),
            n = n(),
            grade = grade[1],
            act_spread = act_spread[1]) %>%
  mutate(error = act_spread - mu) %>%
  group_by(pollster) %>%
  summarize(mu = mean(mu),
            mu_error = mean(error, na.rm = TRUE),
            sd_srror = sd(error, na.rm = TRUE),
            n = n(),
            grade = grade[1]) %>%
  summarize(mu_error = mean(mu_error, na.rm = TRUE))


head(results_us_election_2016)
