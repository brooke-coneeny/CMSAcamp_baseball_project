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
batter_all <- batter_all %>%
  mutate(description2 = case_when(
           description %in% c("automatic_ball", "ball", "blocked_ball", 
                              "called_strike", "hit_by_pitch", "pitchout") ~ "no_swing", 
           description %in% c("foul", "foul_tip") ~ "foul",
           description %in% c("foul_bunt", "missed_bunt", "bunt_foul_tip") ~ "bunting",
           description %in% c("hit_into_play", "hit_into_play_no_out", "hit_into_play_score") ~ "fair_contact",
           description %in% c("swinging_strike", "swinging_strike_blocked") ~ "swing_and_miss",
           TRUE ~ description)
  )

#Finding each player's attack angle in each season
attack_angles <- batter_all %>%
  filter(description == "hit_into_play") %>%
  group_by(player_name, year) %>% 
  filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>%
  filter(launch_speed >= quantile(launch_speed, .9, na.rm = TRUE)) %>%
  summarize(attack_angle = median(launch_angle))

####################################################################################################################################

#Looking at data pitch by pitch, did they make contact with the attack angle or did they miss? How is this attack angle effecting the
#amount of pitches they can hit? 

#Lets only look at pitches they fouled, hit in play, or missed. This means were ignoring pitches they did not swing at 
#If they swing and miss, there is no contact (0) if they foul or hit in play there is contact (1)
batter_all <- batter_all %>%
  filter(description2 %in% c("foul", "fair_contact", "swing_and_miss")) %>%
  mutate(contact = case_when(description2 == "swing_and_miss" ~ 0, 
                             description2 %in% c("foul", "fair_contact") ~ 1))

#Combining with the attack angles of each player in each year 
contact_batter_all <- batter_all %>%
  left_join(attack_angles, by = c("player_name", "year")) %>%
  filter(!is.na(launch_speed), !is.na(launch_angle), !is.na(woba_value))

#Creating a logistic regression model which predicts if contact was made based off of attack angle and pitch height 
init_logit <- glm(contact ~ attack_angle + plate_z,
                  data = contact_batter_all,
                  family = "binomial")


####################################################################################################################################

#Creating training and testing data sets for logistic model

#Group by player and year so that all the pitches that a player swung at in a season are in either the test or train data set. 
player_year <- contact_batter_all%>%
  group_by(player_name, year) %>%
  count()

#75 percent of the sample size
smp_size <- floor(0.75 * nrow(player_year))

#Set the seed to make partition reproducible
set.seed(315)
sample_rows <- sample(nrow(player_year), size = smp_size)

player_year_train <- player_year[sample_rows,]
player_year_test <- player_year[-sample_rows,]

contact_train <- contact_batter_all %>%
  right_join(player_year_train, by = c("player_name", "year"))
contact_test <- contact_batter_all %>%
  right_join(player_year_test, by = c("player_name", "year"))

#Use training data to predict 
init_logit <- glm(contact ~ attack_angle + plate_z,
                  data = contact_train,
                  family = "binomial")

#Viewing predicted probability relationship
contact_train %>%
  mutate(pred_prob = init_logit$fitted.values) %>%
  ggplot(aes(x = attack_angle)) +
  geom_line(aes(y = pred_prob),
            color = "blue") +
  geom_point(aes(y = contact),
             alpha = 0.3,
             color = "darkorange") +
  theme_bw()


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

