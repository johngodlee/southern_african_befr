# How much AGB do small stems have?
# John Godlee (johngodlee@gmail.com)
# 2019_12_11

# Packages 
library(dplyr)


# Load data ----

load("data/clean_input_data.Rdata")
load("data/seosaw_plot_summary5Apr2019.Rdata")

# agb of stems < 5 cm DBH
s_small <- s %>%
  filter(diam < 5) %>%
  group_by(plotcode) %>%
  summarise(agb_small = sum(Bchave))

# proportion of total agb
s_all <- s %>%
  group_by(plotcode) %>%
  summarise(agb_all = sum(Bchave)) %>%
  left_join(s_small, ., by = "plotcode") %>%
  mutate(perc_small = agb_small / agb_all * 100) %>%
  filter(perc_small < 30) %>%
  arrange(desc(perc_small))

median(s_all$perc_small)

zambia_plotcodes <- ssaw8$plotInfoFull %>%
  filter(pi == "Siampale") %>%
  pull(plotcode)

s %>%
  filter(plotcode %in% zambia_plotcodes) %>%
  group_by(plotcode) %>%
  summarise(diam_5 = sum(diam < 15)) %>%
  pull(diam_5) %>%
  unique(.)


