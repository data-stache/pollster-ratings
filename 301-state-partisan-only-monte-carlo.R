library(dslabs)
load('rda/fit_states.rda')

# LOAD STATE NAMES AND STATE ABBREVIATIONS
states <- read.csv("data/us-census-population-data.csv")
states <- states %>% select(state, state_name)

# ELECTORAL VOTES
data("polls_us_election_2016")
head(results_us_election_2016)

# EV SMALLER
electoral <- results_us_election_2016 %>%
  select(state_name = state, electoral_votes) %>%
  left_join(states) %>%
  select(state, state_name, electoral_votes)

# ADD ELECTORAL VOTES TO FIT  
fit_states <- fit_states %>%
  left_join(electoral) %>%
  mutate(electoral_votes = as.numeric(electoral_votes))

# MONTE CARLO
biden_ev <- replicate(1000, {
  fit_states %>% mutate(simulated_result = rnorm(length(mu), mu, t_dist),
                        biden = ifelse(simulated_result < 0, electoral_votes, 0)) %>%    # award votes if Biden wins state (spread negative)
    summarize(biden = sum(biden)) %>%
    .$biden     # total votes for Biden
})

# MEAN
mean(biden_ev > 269)    # over 269 votes wins election

# HISTOGRAM
data.frame(biden_ev) %>%
  data.frame(biden_ev) %>%
  ggplot(aes(biden_ev)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)
