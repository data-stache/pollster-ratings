library(tidyverse)
library(tidylog)

load('rda/state_partisan.rda')
load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda")

head(state_partisan)

state_order <- state_partisan %>%
  arrange(desc(lean_avg)) %>%
  .$state_name

head(state_partisan)

state_partisan <- state_partisan %>%
  group_by(state) %>%
  mutate(lean_min = min(slpli, pvi, lean_2020),
         lean_max = max(slpli, pvi, lean_2020))

party_color <- c('darkblue', 'red4')

state_partisan %>%
  mutate(state_name = factor(state_name, levels = state_order)) %>%
  ggplot(aes(x = state_name, y = lean_avg, label = state, fill = state_lean, col = state_lean)) +
  geom_hline(yintercept = 0, size = .25, color = 'grey40') +
  geom_pointrange(aes(ymin = lean_min, ymax = lean_max), fatten = 0, size = .25) +
  geom_text(aes(y = ifelse(state_partisan$lean_avg < 0, state_partisan$lean_min - .01, state_partisan$lean_max + .01)), hjust = .5, size = 1) +
  scale_fill_manual(values = party_color) +
  scale_color_manual(values = party_color) +
  scale_y_continuous(breaks = seq(-1, 1, .05)) +
  coord_flip() +
  theme_DataStache() +
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank())

p_width <- 6
p_height <- (9/16) * p_width

ggsave("figs/state-lean.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")
