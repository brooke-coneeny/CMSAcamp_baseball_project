###################################################################################################################################
#This file intends to look at how different variables affect fair vs foul contact balls
#Brooke Coneeny, Sarah Sult, and Erin Franke 
#CMSAcamp 2021
####################################################################################################################################

#Loading Libraries
library(tidyverse)

#Loading Data 
batter_all_2016 <- read_rds("private_data/all2016data.rds")
batter_all_2017 <- read_rds("private_data/all2017data.rds")
batter_all_2018 <- read_rds("private_data/all2018data.rds")
batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2020 <- read_rds("private_data/all2020data.rds")
batter_all_2021 <- read_rds("private_data/all2021data.rds")

batter_all_2016 <- batter_all_2016 %>%
  mutate(year = "2016")
batter_all_2017 <- batter_all_2017 %>%
  mutate(year = "2017")
batter_all_2018 <- batter_all_2018 %>%
  mutate(year = "2018")
batter_all_2019 <- batter_all_2019 %>%
  mutate(year = "2019")
batter_all_2020 <- batter_all_2020 %>%
  mutate(year = "2020")
batter_all_2021 <- batter_all_2021 %>%
  mutate(year = "2021")

batter_all <- bind_rows(batter_all_2016, batter_all_2017, batter_all_2018, 
                        batter_all_2019, batter_all_2020, batter_all_2021)


#Creating more columns that describe the result of the pitch and the approach of the pitch 
batter_all <- batter_all %>%
  #Creating a column which describes the result of the pitch 
  mutate(description2 = case_when(description %in% c("automatic_ball", "ball", "blocked_ball", "intent_ball") ~ "ball", 
                                  description %in% c("bunt_foul_tip", "foul_bunt", "hit_by_pitch", 
                                                     "missed_bunt", "pitchout", "called_strike") ~ "other", 
                                  description %in% c("foul", "foul_tip") ~ "foul", 
                                  description %in% c("swinging_strike", "swinging_strike_blocked") ~ "swinging_strike",
                                  description %in% c("hit_into_play", "hit_into_play_no_out", "hit_into_play_score") ~ "in_play",
                                  TRUE ~ description)) %>%
  #Creating a column that groups together types of pitches
  mutate(pitch_group = case_when(
    pitch_name %in% c("4-Seam Fastball", "Fastball", "Cutter", "Sinker") ~ "Fastball", 
    pitch_name %in% c("Changeup", "Split-Finger", "Screwball") ~ "Offspeed", 
    pitch_name %in% c("Slider", "Knuckle Curve", "Curveball", "Knuckle Curve", 
                      "Knuckleball", "Eephus") ~ "Breaking", 
    TRUE ~ pitch_name
  )) %>%
  #Using Adam's recommended physics equations to calculate the approach angle of the pitch
  #Negative because of the direction of v and a vectors
  mutate(approach_angle = -(atan((vz0 + ((-vy0 - sqrt((vy0^2) - 2.0 * ay * (50.0 - 1.417))) / ay) * az) / 
                                   (vy0 + ((-vy0 - sqrt((vy0^2) - 2.0 * ay * (50.0 - 1.417))) / ay) * ay)) * 180.0/pi)) %>%
  #Creating a column which determines if contact was made with the pitch 
  mutate(contact = ifelse(description2 %in% c("foul", "in_play"), TRUE, FALSE))


#How does pitch type effect the proportion of pitches that are fair vs foul?
#Result: different pitch types do not have a big effect on whether a pitch is hit fair or foul 
pitch_type_effect <- batter_all %>%
  select("pitch_group", "contact") %>%
  group_by(pitch_group, contact) %>%
  summarize(num = n()) %>%
  ungroup() %>%
  pivot_wider(
    names_from = contact,
    values_from = num
  ) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  rename(
    foul = 'FALSE',
    fair = 'TRUE'
  ) %>%
  group_by(pitch_group) %>%
  mutate(percent_fair = 
           fair / (foul + fair)) %>%
  filter(pitch_group %in% c("2-Seam Fastball", "Breaking", "Fastball", "Forkball", "Offspeed")) %>%
  ggplot(aes(x = pitch_group, y = percent_fair)) +
  geom_col() +
  theme_bw() + 
  labs(
    title = "Which pitches are hit fair most often?",
    subtitle = "Looking at made-contact pitches only",
    x = "Pitch Type",
    y = "Percent Hit Fair"
  )
  
#How does a batters stance effect the amount of pitches hit fair vs foul?
#Result: there is only a difference of 0.002 between different stances 
stance_effect <- batter_all %>%
  select("stand", "contact") %>%
  group_by(stand, contact) %>%
  summarize(num = n()) %>%
  ungroup() %>%
  pivot_wider(
    names_from = contact,
    values_from = num
  ) %>%
  rename(
    foul = 'FALSE',
    fair = 'TRUE'
  ) %>%
  group_by(stand) %>%
  mutate(percent_fair = 
           fair / (foul + fair)) %>%
  ggplot(aes(x = stand, y = percent_fair)) +
  geom_col() + 
  theme_bw() + 
  labs(
    title = "Does a batters stance change amount of pitches hit fair?",
    subtitle = "Looking at made-contact pitches only",
    x = "Stance",
    y = "Percent Hit Fair"
  )

#Looking at the most common release spin rates 
#Result: LARGE majority between 1500-3000
spin <- batter_all %>%
  ggplot(aes(x = release_spin_rate)) +
  geom_density()

#How does release spin rate effect the amount of pitches hit fair vs foul? 
#Result: does not look like there is much of a difference between spin rate and fair percentage 
spin_effect <- batter_all %>%
  select("release_spin_rate", "contact") %>%
  filter(!is.na(release_spin_rate)) %>%
  #filter(release_spin_rate >= 1500 & release_spin_rate <= 3000) %>%
  mutate(spin_group = case_when(
    release_spin_rate < 1500 ~ "0-1500",
    release_spin_rate >= 1500 & release_spin_rate < 1800 ~ "1500-1800",
    release_spin_rate >= 1800 & release_spin_rate < 2100 ~ "1800-2100",
    release_spin_rate >= 2100 & release_spin_rate < 2400 ~ "2100-2400",
    release_spin_rate >= 2400 & release_spin_rate < 2700 ~ "2400-2700",
    release_spin_rate >= 2700 & release_spin_rate < 3000 ~ "2700-3000",
    release_spin_rate >= 3000 ~ "3000-max"
  )) %>%
  group_by(spin_group, contact) %>%
  summarize(num = n()) %>%
  ungroup() %>%
  pivot_wider(
    names_from = contact,
    values_from = num
  ) %>%
  rename(
    foul = 'FALSE',
    fair = 'TRUE'
  ) %>%
  group_by(spin_group) %>%
  mutate(percent_fair = 
           fair / (foul + fair)) %>%
  ggplot(aes(x = spin_group, y = percent_fair)) +
  geom_col() + 
  theme_bw() + 
  labs(
    title = "Does release spin rate change amount of pitches hit fair?",
    subtitle = "Looking at made-contact pitches only",
    x = "Release Spin Rate",
    y = "Percent Hit Fair"
  )





  
  
  


