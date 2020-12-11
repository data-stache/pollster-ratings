library(tidyverse)
library(lubridate)
library(tidylog)
library(knitr)

load('rda/pres_polls_2020.rda')
load('rda/pres_results_2020.rda')

# Filter dates that are after the conventions and the week before the election
dat <- pres_polls_2020 %>%
  filter(end_date %in% ymd(20200901):ymd(20201027))

dat %>%
  group_by(pollster, state) %>%
  summarize(n = n(),
            mean = mean(spread)) %>%
  arrange(desc(state))

unique(dat$state)

