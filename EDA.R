library(tidyverse)
library(baseballr)

files = list.files("/Users/ben/Desktop/Code/Batter Pitcher Matchups/Statcast 2022/")
filepaths = paste0("Statcast 2022/",files)

data = map_dfr(filepaths, ~ read_csv(.x))

data %>%
  distinct(pitch_name, .keep_all = TRUE) %>%
  select(pitch_name, pitch_type)

## pitcher features: PRIMARY FASTBALL

pitcher_primary_fastballs = data %>%
  filter(pitch_type %in% c("FF","SI","FC")) %>%
  group_by(pitcher) %>%
  summarise(
    FF_TOTAL = length(pitch_type[pitch_type == "FF"]),
    FT_TOTAL = length(pitch_type[pitch_type == "SI"]),
    CUT_TOTAL = length(pitch_type[pitch_type == "FC"])
  ) %>%
  mutate(PRIMARY_FASTBALL = case_when(
    FF_TOTAL > FT_TOTAL & FF_TOTAL > CUT_TOTAL ~ "FF",
    FT_TOTAL > FF_TOTAL & FT_TOTAL > CUT_TOTAL ~ "FT",
    CUT_TOTAL > FF_TOTAL & CUT_TOTAL > FT_TOTAL ~ "CUT",
    TRUE ~ NA
  ))

## pitcher features: PRIMARY FASTBALL MEAN VELOCITY

data_fastballs = data %>%
  filter(pitch_type %in% c("FF","SI","FC")) %>%
  mutate(pitch_type = case_when(
    pitch_type == "FF" ~ "FF",
    pitch_type == "SI" ~ "FT",
    pitch_type == "FC" ~ "CUT"
  )) %>%
  left_join(pitcher_primary_fastballs, by = "pitcher")

average_fastball_velocity = data_fastballs %>%
  filter(pitch_type == PRIMARY_FASTBALL) %>%
  group_by(pitcher, PRIMARY_FASTBALL) %>%
  summarise(AVG_VELO = mean(effective_speed, na.rm = TRUE),
            MAX_VELO = max(effective_speed, na.rm = TRUE),
            PITCHES = n())

## pitcher features: PRIMARY SECONDARY PITCH

pitcher_primary_secondary = data %>%
  group_by(pitcher) %>%
  summarise(
    CH_TOTAL = length(pitch_type[pitch_type == "CH"]),
    CB_TOTAL = length(pitch_type[pitch_type == "CU"]),
    SL_TOTAL = length(pitch_type[pitch_type == "SL"]),
    KC_TOTAL = length(pitch_type[pitch_type == "KC"]),
    SP_TOTAL = length(pitch_type[pitch_type == "FS"]),
    PITCHES = n()
  ) %>%
  mutate(across(CH_TOTAL:SP_TOTAL, function(x) {x / PITCHES})) %>%
  mutate(SECONDARY_RATE = CH_TOTAL + CB_TOTAL + SL_TOTAL + KC_TOTAL + SP_TOTAL,
         PRIMARY_SECONDARY = case_when(
           pmax(CH_TOTAL, CB_TOTAL, SL_TOTAL, KC_TOTAL, SP_TOTAL, na.rm = TRUE) == CH_TOTAL ~ "CH",
           pmax(CH_TOTAL, CB_TOTAL, SL_TOTAL, KC_TOTAL, SP_TOTAL, na.rm = TRUE) == CB_TOTAL ~ "CB",
           pmax(CH_TOTAL, CB_TOTAL, SL_TOTAL, KC_TOTAL, SP_TOTAL, na.rm = TRUE) == SL_TOTAL ~ "SL",
           pmax(CH_TOTAL, CB_TOTAL, SL_TOTAL, KC_TOTAL, SP_TOTAL, na.rm = TRUE) == KC_TOTAL ~ "KC",
           pmax(CH_TOTAL, CB_TOTAL, SL_TOTAL, KC_TOTAL, SP_TOTAL, na.rm = TRUE) == SP_TOTAL ~ "SP"
           
         ))

pitcher_secondary = pitcher_primary_secondary %>%
  select(pitcher, pitches = PITCHES, offspeed_rate = SECONDARY_RATE, primary_offspeed = PRIMARY_SECONDARY)

pitcher_fastball = average_fastball_velocity %>%
  select(pitcher, fastball = PRIMARY_FASTBALL, avg_velo = AVG_VELO, max_velo = MAX_VELO)

pitcher_hand = data %>%
  distinct(pitcher, .keep_all = TRUE) %>%
  select(pitcher, hand = p_throws)

pitcher_slot = data %>%
  group_by(pitcher) %>%
  summarise(avg_rel_ext = mean(release_extension, na.rm = TRUE),
            avg_rel_x = mean(release_pos_x, na.rm = TRUE),
            avg_rel_z = mean(release_pos_z, na.rm = TRUE)) %>%
  left_join(pitcher_hand, by = "pitcher") %>%
  mutate(adj_avg_rel_x = ifelse(hand == "L", - avg_rel_x, avg_rel_x),
         abs_avg_rel_x = abs(avg_rel_x),
         release_angle_ground = 180 - (atan2(avg_rel_z, adj_avg_rel_x) / pi * 180)) %>%
  select(pitcher, avg_rel_ext, release_angle_ground)

# ggplot(pitcher_slot, aes(x = avg_rel_x, y = avg_rel_z)) +
#   geom_point(aes(color = release_angle_ground))

pitcher_full = pitcher_hand %>%
  left_join(pitcher_fastball, by = "pitcher") %>%
  left_join(pitcher_secondary, by = "pitcher") %>%
  left_join(pitcher_slot, by = "pitcher") %>%
  na.omit() %>%
  select(pitcher, pitches, everything())

pitcher_id_lookup = chadwick_player_lu()

pitcher_key = pitcher_id_lookup %>%
  mutate(name = paste(name_first, name_last)) %>%
  select(pitcher = key_mlbam, name)

pitcher_named = pitcher_full %>%
  left_join(pitcher_key, by = "pitcher") %>%
  select(name, everything())

write_csv(pitcher_named, "Pitchers.csv")
