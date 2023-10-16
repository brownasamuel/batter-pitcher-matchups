library(tidyverse)

files = list.files("/Users/ben/Desktop/Code/Batter Pitcher Matchups/Statcast 2022/")
filepaths = paste0("Statcast 2022/",files)

data = map_dfr(filepaths, ~ read_csv(.x))

pitchers = read_csv("PitcherClusterInfo.csv")

data_clusters = data %>%
  left_join(pitchers, by = "pitcher") %>%
  mutate(pitcher_platoon_advantage = ifelse(hand == stand,"pitch_platoon","hit_platoon"))

data_clusters %>%
  group_by(cluster, pitcher_platoon_advantage) %>%
  summarise(bbe = n(),
            woba = sum(woba_value, na.rm = TRUE) / sum(woba_denom, na.rm = TRUE)) %>%
  filter(!is.na(cluster)) %>%
  pivot_wider(id_cols = cluster, names_from = pitcher_platoon_advantage, values_from = bbe:woba) %>%
  mutate(platoon_advantage = woba_hit_platoon - woba_pitch_platoon) %>%
  mutate(avg_woba = woba_hit_platoon * (bbe_hit_platoon / (bbe_hit_platoon + bbe_pitch_platoon)) +
                    woba_pitch_platoon * (bbe_pitch_platoon / (bbe_hit_platoon + bbe_pitch_platoon)))
