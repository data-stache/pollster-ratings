load('rda/elections.rda')
load('rda/pres_results_2020.rda')

head(pres_results_2020)

party_col <- c('darkblue', 'red4', 'green4')

elections %>%
  mutate(party = factor(party, levels = c('dem', 'rep', 'other'))) %>%
  filter(state == 'FL' & type == 'GE') %>%
  ggplot(aes(x = year, y = share_vote, col = party)) +
  geom_point(size = .6) +
  scale_color_manual(values = party_col) +
  geom_smooth()


##### INITIAL TRENDS #####
# ALL STATES
elections %>%
  filter(!party == 'other' & !state %in% c('AS', 'GU', 'MP', 'PR', 'VI')) %>%
  arrange(year) %>%
  group_by(state, year, type) %>%
  summarize(spread = share_vote[2] - share_vote[1]) %>%
  ggplot(aes(x = year, y = spread, col = type)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_line() +
  coord_flip() +
  scale_x_reverse(breaks = seq(2002, 2018, 2)) +
  facet_wrap(. ~ state)

##### TRENDS OVER TIME ####
ge <- elections %>%
  # Only General Elections, Only top ticket candidates
  filter(!party == 'other' & !state %in% c('AS', 'GU', 'MP', 'PR', 'VI') & type == 'GE') %>%
  # Calculate Spread
  group_by(state, year) %>%
  summarize(spread = share_vote[2] - share_vote[1]) %>%
  arrange(year) %>%
  ungroup() 

pres <- elections %>%
  # Only General Elections, Only top ticket candidates
  filter(!party == 'other' & !state %in% c('AS', 'GU', 'MP', 'PR', 'VI') & type == 'PRES') %>%
  # Calculate Spread
  group_by(state, year) %>%
  summarize(spread = share_vote[2] - share_vote[1]) %>%
  arrange(year) %>%
  ungroup() 


ge %>%
  ggplot(aes(x = year, y = spread)) +
  geom_hline(yintercept = 0) +
  geom_point(col = 'darkblue') +
  geom_line(group = 1, col = 'darkblue') +
  geom_smooth(method = 'lm') +
  scale_x_reverse(breaks = seq(2000, 2020, 2)) +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip() +
  facet_wrap(. ~ state)

pres %>%
  ggplot(aes(x = year, y = spread)) +
  geom_hline(yintercept = 0) +
  geom_point(col = 'darkblue') +
  geom_line(group = 1, col = 'darkblue') +
  geom_smooth(method = 'lm') +
  scale_x_reverse(breaks = seq(2000, 2020, 2)) +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip() +
  facet_wrap(. ~ state)
