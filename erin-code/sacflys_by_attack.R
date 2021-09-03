# PURPOSE: Find the average number of sacflys per at bat at each attack angle

# load data and libraries ---------------------------------------------------
library(tidyverse)
library(baseballr)

batter_all_2016 <- read_rds("private_data/all2016data.rds") %>%
  mutate(year = "2016")
batter_all_2017 <- read_rds("private_data/all2017data.rds") %>%
  mutate(year = "2017")
batter_all_2018 <- read_rds("private_data/all2018data.rds") %>%
  mutate(year = "2018")
batter_all_2019 <- read_rds("private_data/all2019data.rds") %>%
  mutate(year = "2019")
batter_all_2020 <- read_rds("private_data/all2020data.rds") %>%
  mutate(year = "2020")
batter_all_2021 <- read_rds("private_data/all2021data.rds") %>%
  mutate(year = "2021")
batter_all_1621 <- bind_rows(batter_all_2016, batter_all_2017, batter_all_2018, 
                             batter_all_2019, batter_all_2020, batter_all_2021)
'%!in%' <- Negate('%in%')

#add column that calculates attack angle 
attack_angles <- batter_all_1621 %>%
  filter(description == "hit_into_play") %>%
  group_by(player_name, year) %>% 
  mutate(at_bats = n()) %>%
  filter(at_bats >=50) %>%
  filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>%
  filter(launch_speed >= quantile(launch_speed, .9, na.rm = TRUE)) %>%
  summarize(attack_angle = median(launch_angle))

batter_all_1621 <- batter_all_1621 %>%
  filter(description == "hit_into_play") %>%
  left_join(attack_angles, by = c("player_name", "year")) %>%
  filter(attack_angle >0 & attack_angle <=25) %>%
  mutate(attack_angle = round(attack_angle,0))
  
#find number of sac flys at each attack angle
sacs_per_hit_into_play <- batter_all_1621 %>%
  filter(events %!in% c("catcher_interf", "game_advisory")) %>%
  mutate(event_sac_fly = case_when(events == "sac_fly" ~ 1, 
                                   TRUE ~0)) %>%
  group_by(attack_angle, event_sac_fly) %>%
  count() %>%
  pivot_wider(id_cols = attack_angle, names_from = event_sac_fly, values_from = n) %>%
  rename(`sac`=`1`, `other`=`0`) %>%
  mutate(sacs_per_hip = sac/(sac+other)) %>%
  select(attack_angle, sacs_per_hip)

#add this column to batter_all_1621
batter_all_1621 <- batter_all_1621 %>%
  left_join(sacs_per_hit_into_play, by=c("attack_angle"))

#graph of attack angle versus sac
sacs_per_hit_into_play %>%
  mutate(sacs_per_hip = sacs_per_hip *100) %>%
  ggplot(aes(x=attack_angle, y=sacs_per_hip))+
  geom_line()+
  labs(x="attack angle", y="sac flys / 100 balls hit into play", title = "Sac flys increase with attack angle")+
  theme_minimal()+
  theme(plot.title.position = "plot")
  
  