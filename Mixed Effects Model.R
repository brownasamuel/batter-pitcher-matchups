## fitting the mixed effects model

library(tidyverse)
library(lme4)
library(brms)

files = list.files("/Users/ben/Desktop/Code/Batter Pitcher Matchups/Statcast 2022/")
filepaths = paste0("Statcast 2022/",files)

data = map_dfr(filepaths, ~ read_csv(.x))

pitchers = read_csv("PitcherClusterInfo.csv")

data_clusters = data %>%
  left_join(pitchers, by = "pitcher") %>%
  mutate(pitcher_platoon_advantage = ifelse(hand == stand,1,0)) %>%
  mutate(cluster = factor(cluster))

data_outcomes = data_clusters %>%
  filter(!is.na(woba_value) & woba_denom == 1 & !is.na(woba_denom))

mixed_model = lmer(woba_value ~ (pitcher_platoon_advantage | cluster) + cluster + (1 | batter) + (1 | pitcher),
                   data = data_outcomes)

ranef(mixed_model)
fixef(mixed_model)

predict(mixed_model)
