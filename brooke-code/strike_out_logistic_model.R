####################################################################################################################################
#This file intends to model strikeout probability based on different launch angles. 
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


#Creating a column which describes the result of the pitch 
batter_all_2019 <- batter_all_2019 %>%
  mutate(description2 = case_when(description %in% c("ball", "blocked_ball", "intent_ball") ~ "ball", 
                                  description %in% c("bunt_foul_tip", "foul_bunt", "hit_by_pitch", 
                                                     "missed_bunt", "pitchout") ~ "other", 
                                  description %in% c("foul", "foul_tip") ~ "foul", 
                                  description %in% c("swinging_strike", "swinging_strike_blocked") ~ "swinging_strike",
                                  TRUE ~ description))

#Finding each player's attack angle in each season
attack_angles <- batter_all_2019 %>%
  filter(description == "hit_into_play") %>%
  group_by(player_name) %>% 
  filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>%
  filter(launch_speed >= quantile(launch_speed, .9, na.rm = TRUE)) %>%
  summarize(attack_angle = median(launch_angle))

#Finding how many balls each player put in play 
hit_in_play <- batter_all_2019 %>%
  group_by(player_name) %>%
  filter(description == "hit_into_play") %>%
  count() %>%
  rename(balls_in_play = n)

####################################################################################################################################

#Looking at data pitch by pitch, did they make contact with the attack angle or did they miss? How is this attack angle effecting the
#amount of pitches they can hit? 

#Lets only look at pitches they fouled, hit in play, or missed. This means were ignoring pitches they did not swing at 
#If they swing and miss, there is no contact (1) if they foul or hit in play there is contact (0)
contact_2019 <- batter_all_2019 %>%
  filter(description2 %in% c("swinging_strike", "foul", "hit_into_play")) %>%
  mutate(contact = case_when(description2 == "swinging_strike" ~ 1, 
                             description2 %in% c("foul", "hit_into_play") ~ 0))

#Combining with the attack angles of each player in each year 
#Combining with number of balls in play by player and year 
contact_batter_all <- hit_in_play %>%
  filter(balls_in_play >= 50) %>%
  left_join(contact_2019, by=c("player_name")) %>%
  left_join(attack_angles, by = c("player_name")) %>%
  select(player_name, attack_angle, launch_speed, launch_angle, balls_in_play, pitch_type, 
         woba_value, description, description2, events, balls, strikes, plate_z, contact) %>%
  filter(plate_z <=5 & plate_z >= -2.5)


####################################################################################################################################

#Creating training and testing data sets for logistic model

#Group by player and year so that all the pitches that a player swung at in a season are in either the test or train data set. 
player_num_pitches <- contact_batter_all%>%
  group_by(player_name) %>%
  count()

#75 percent of the sample size
smp_size <- floor(0.75 * nrow(player_num_pitches))

#Set the seed to make partition reproducible
set.seed(315)
sample_rows <- sample(nrow(player_num_pitches), smp_size)

player_year_train <- player_num_pitches[sample_rows,]
player_year_test <- player_num_pitches[-sample_rows,]

contact_train <- contact_batter_all %>%
  right_join(player_year_train, by = c("player_name")) %>%
  mutate(id = row_number())

contact_test <- contact_batter_all %>%
  right_join(player_year_test, by = c("player_name"))

#Use training data to predict 
init_logit <- glm(contact ~ attack_angle + plate_z,
                  data = contact_train,
                  family = "binomial")

#Viewing predicted probability relationship
logit_results <- as.data.frame(init_logit$fitted.values) %>%
  mutate(id = row_number())

logit_results <- logit_results %>%
  left_join(contact_train, by = c("id")) %>%
  select("player_name", "init_logit$fitted.values", "attack_angle", "plate_z", "contact") 

logit_plot <- logit_results %>%
  ggplot(aes(x = attack_angle, y = init_logit$fitted.values)) +
  geom_point()


####################################################################################################################################






#Find each player's number of plate appearances
plate_appearances <- batter_all %>%
  mutate(PA_id = paste(game_pk, at_bat_number, sep = "-")) %>%
  group_by(player_name, year) %>%
  summarise(n_pa = length(unique(PA_id))) %>%
  filter(n_pa >= 50)

#Find number of balls hit into play by each batter 
balls_in_play <- batter_all %>%
  filter(description == "hit_into_play") %>%
  group_by(player_name, year) %>%
  summarise(n_in_play = n())

#Find how many times they made fair contact, fouled, did not swing, swing and miss, and attempted bunting 
pitch_result <- batter_all %>%
  group_by(player_name, year, description2) %>%
  count() %>%
  pivot_wider(id_cols = player_name:description2, names_from = description2, values_from = n) %>%
  mutate_all(~replace(., is.na(.), 0))

####################################################################################################################################

#Find the rates at which people swing and miss at their given attack angle for the season 
swing_and_miss_rate <- pitch_result %>%
  left_join(plate_appearances, by = c("player_name", "year")) %>%
  left_join(attack_angles, by = c("player_name", "year")) %>%
  filter(n_pa >= 50) %>%
  mutate(
    swing_miss_rate = swing_and_miss / (fair_contact + foul + swing_and_miss)
  )

#What is the relationship between attack angle and swing & miss rate 
swing_and_miss_rate %>%
  ggplot(aes(x = attack_angle, y = swing_miss_rate)) +
  geom_point() +
  theme_bw()

#linear correlation is 0.1982
cor(swing_and_miss_rate$attack_angle, swing_and_miss_rate$swing_miss_rate)

