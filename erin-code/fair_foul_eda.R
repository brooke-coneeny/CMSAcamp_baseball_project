# This file intends to see what increases/decreases the probability of a ball being hit fair. 
#It also begins to explore how the probability of a batter swinging at a pitch changes by location. 
#Brooke Coneeny, Sarah Sult, and Erin Franke 
#CMSAcamp 2021
####################################################################################################################################

#Loading Libraries
library(tidyverse)
library(plotly)
library(broom)
library(mgcv)
library(viridis)
'%!in%' <- Negate('%in%')

#Loading Data
batter_all_2016 <- read_rds("private_data/all2016data.rds")
batter_all_2017 <- read_rds("private_data/all2017data.rds")
batter_all_2018 <- read_rds("private_data/all2018data.rds")
batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2020 <- read_rds("private_data/all2020data.rds")
batter_all_2021 <- read_rds("private_data/all2021data.rds")

batter_all_1621 <- bind_rows(batter_all_2016, batter_all_2017, batter_all_2018, 
                             batter_all_2019, batter_all_2020, batter_all_2021)
batter_all_1621 <- batter_all_1621 %>%
  mutate(description2 = case_when(description %in% c("ball", "blocked_ball", "intent_ball") ~ "ball", 
                                  description %in% c("bunt_foul_tip", "foul_bunt", "hit_by_pitch", 
                                                     "missed_bunt", "pitchout") ~ "other", 
                                  description %in% c("foul") ~ "foul", 
                                  description %in% c("swinging_strike", "swinging_strike_blocked", "foul_tip") ~ "swinging_strike",
                                  TRUE ~ description), 
         #Using Adam's recommended physics equations to calculate the approach angle of the pitch
         #Negative because of the direction of v and a vectors
         approach_angle = -(atan((vz0 + ((-vy0 - sqrt((vy0^2) - 2.0 * ay * (50.0 - 1.417))) / ay) * az) / 
                                   (vy0 + ((-vy0 - sqrt((vy0^2) - 2.0 * ay * (50.0 - 1.417))) / ay) * ay)) * 180.0/pi))

batted_balls <- read_rds("public_data/batted_balls.rds")
strikeout_eda <- read_rds("public_data/strikeout_eda.rds")
attack_angles <- read_rds("public_data/attack_angles_1621.rds")
####################################################################################################################################
#EDA TO EXPLORE WHAT CORRELATES WITH FAIR/FOUL (foul tips not included)

#heatmap to visualize the proportion of balls hit fair at a given release speed and spin rate. 
batter_all_1621 %>%
  filter(description %in% c("foul", "hit_into_play"), 
         release_speed >70, 
         release_spin_rate >= 1400, 
         release_spin_rate <= 3000) %>% 
  mutate(fair_foul = case_when(description == "foul" ~ 0, 
                               description == "hit_into_play" ~ 1), 
         fair_foul = as.factor(fair_foul),
         release_speed_level = cut(release_speed, 8, 
                                   labels = c("70.1-70.4", "74.4-78.6", "78.6-82.9", "82.9-87.2", "87.2-91.5", "91.5-95.8", "95.8-100", "100-104")), 
         spin_rate_level = cut(release_spin_rate, 8, 
                               labels = c("1400-1600", "1600-1800", "1800-2000", "2000-2200", "2200-2400", "2400-2600", "2600-2800", "2800-3000"))) %>%
  select(fair_foul, release_speed_level, spin_rate_level) %>%
  group_by(release_speed_level, spin_rate_level) %>%
  count(fair_foul) %>%
  pivot_wider(id_cols = release_speed_level:spin_rate_level, names_from = fair_foul, values_from = n) %>%
  rename(foul = `0`, fair=`1`) %>%
  mutate(fair_proportion = fair/(fair+foul)) %>%
  filter(fair+foul >= 100) %>%
  ggplot(aes(release_speed_level, spin_rate_level, fill = fair_proportion)) +
  geom_tile()+
  scale_fill_viridis(option = "D")+
  theme_minimal()+
  labs(x="release speed (mph)", y="spin rate(rpm)", 
       title = "Given contact, the proportion of balls hit fair decreases as pitch speed and spin rate rise", 
       fill = "", 
       subtitle = "For all MLB contact batted balls 2016-2021 with at least 100 datapoints per grouping")+
  theme(plot.title.position = "plot", 
        plot.title = element_text(family = "mono", face = "bold", size = 10),
        plot.subtitle = element_text(family = "mono", size = 9),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(family = "mono", face = "bold", size=9), 
        axis.title = element_text(family = "mono", face = "bold", size=7))


#find average vertical strike zone height for top and bottom - 1.58 and 3.42
batter_all_1621 %>%
  summarize(top = mean(sz_top, na.rm = TRUE), 
            bottom = mean(sz_bot, na.rm = TRUE))

#heatmap to visualize proportion of balls hit fair at a given pitch height and horizontal width
batter_all_1621 %>%
  filter(description %in% c("foul", "hit_into_play"), 
         plate_x >= -2, 
         plate_x <= 2, 
         plate_z >0.8, 
         plate_z <=4) %>%
  mutate(fair_foul = case_when(description == "foul" ~ 0, 
                               description == "hit_into_play" ~ 1), 
         fair_foul = as.factor(fair_foul), 
         plate_x = cut(plate_x, 8,
                       labels = c("-2 to -1.5", "-1.5 to -1", "-1 to -0.5", "-0.5 to 0", "0 - 0.5", "0.5 - 1", "1 - 1.5", "1.5 - 2")), 
         plate_z = cut(plate_z, 8, 
                       labels = c("0.8-1.2", "1.2-1.6", "1.6-2", "2-2.4", "2.4-2.8", "2.8-3.2", "3.2-3.6", "3.6-4"))) %>%
  select(plate_x, plate_z, fair_foul) %>%
  group_by(plate_x, plate_z) %>%
  count(fair_foul) %>%
  pivot_wider(id_cols = plate_x:plate_z, names_from = fair_foul, values_from = n) %>%
  rename(foul = `0`, fair=`1`) %>%
  mutate(fair_proportion = fair/(fair+foul)) %>%
  filter(fair+foul >= 100) %>%
  ggplot(aes(plate_x, plate_z, fill = fair_proportion)) +
  geom_tile()+
  geom_vline(aes(xintercept = (which(levels(plate_x) == "-1 to -0.5") +
                                 which(levels(plate_x) == "-1.5 to -1")) / 1.9), linetype = "dashed", color = "red")+
  geom_vline(aes(xintercept = (which(levels(plate_x) == "0.5 - 1") +
                                 which(levels(plate_x) == "1 - 1.5")) / 2.05), linetype = "dashed", color = "red")+
  geom_hline(aes(yintercept = (which(levels(plate_z) == "1.2-1.6") +
                                 which(levels(plate_z) == "1.6-2")) / 2.05), linetype = "dashed", color = "red")+
  geom_hline(aes(yintercept = (which(levels(plate_z) == "3.2-3.6"))), linetype = "dashed", color = "red")+
  scale_fill_viridis(option = "D")+
  theme_minimal()+
  labs(x="distance from center of home plate (ft)", y="pitch height (ft)", 
       title = "Given contact, the proportion of balls hit fair decreases moving away from the heart of the <strong><span style='color:red'>strike zone</span></strong></b>", 
       fill = "", 
       subtitle = "For all MLB contact batted balls 2016-2021 with at least 100 datapoints per grouping. 'Average' strike zone shown.")+
  theme(plot.title.position = "plot", 
        plot.title = element_markdown(family = "mono", face = "bold", size = 10),
        plot.subtitle = element_text(family = "mono", size = 9),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(family = "mono", face = "bold", size=9), 
        axis.title = element_text(family = "mono", face = "bold", size=7))
####################################################################################################################################
# FAIR or FOUL GAM
fair_foul_batted_balls <- batter_all_1621 %>%
  filter(description %in% c("hit_into_play", "foul")) %>%
  mutate(fair_ball = case_when(description2 == "hit_into_play" ~ 1, 
                               description2 == "foul" ~0))

# Create the fair/foul dataset. This takes all pitches that were either hit foul or fair, and assigns it the player's attack 
#angle in that appropriate season. By joining with batted_balls, we filter for players that had at least 50 batted 
#balls in a given season. Filter for pitches with a height less than or equal to 5 ft and greater than -2.5 feet 
#(I assume this just means they bounce far in front of the plate). Also break the pitch type down into broader categories. 
fair_foul_dataset <- batted_balls %>%
  filter(balls_in_play >= 50) %>%
  left_join(fair_foul_batted_balls, by=c("year", "player_name")) %>%
  left_join(attack_angles, by = c("year", "player_name")) %>%
  select(player_name, year, attack_angle, launch_speed, launch_angle, balls_in_play, pitch_type, 
         woba_value, description, description2, events, balls, strikes, plate_z, fair_ball, plate_x, 
         release_speed, release_spin_rate, stand, approach_angle) %>%
  filter(pitch_type %!in% c("PO") & !is.na(pitch_type)) %>%
  mutate(pitch_type = case_when(pitch_type %in% c("CH", "EP") ~ "Offspeed", 
                                pitch_type %in% c("CS", "CU", "KC", "KN", "SC", "SL") ~ "Breaking", 
                                pitch_type %in% c("FA", "FO", "FS", "FT", "SI", "FC", "FF") ~ "Fastball", 
                                TRUE ~ pitch_type)) %>%
  filter(plate_z <=5 & plate_z >= -2.5)

#Reflect the lefties plate x values to match the rightys.  
fair_foul_dataset_lefty <- fair_foul_dataset %>%
  filter(stand == "L") %>%
  mutate(plate_x = -1*plate_x)
fair_foul_dataset_rest <- fair_foul_dataset %>%
  filter(stand != "L")
fair_foul_dataset <- bind_rows(fair_foul_dataset_rest, fair_foul_dataset_lefty)

# Create training and test dataset from the contact_dataset. Group by player 
#and year so that all the pitches that a player swung at in a season are in either 
#the test or train dataset. 
player_year_ff <- fair_foul_dataset %>%
  group_by(player_name, year) %>%
  count()

set.seed(216)

nrow(player_year)*0.75
sample_rows3 <- sample(nrow(player_year), 1972)

player_year_train_ff <- player_year_ff[sample_rows3,]
player_year_test_ff <- player_year_ff[-sample_rows3,]

ff_py_train <- fair_foul_dataset %>%
  right_join(player_year_train_ff, by = c("player_name", "year"))
ff_py_test <- fair_foul_dataset %>%
  right_join(player_year_test_ff, by = c("player_name", "year"))

# Create the GAM. Predict whether contact will be made given a player's attack angle and 
#the height of the pitch. 

fair_foul_gam <- gam(fair_ball ~ s(plate_x, plate_z, k=20) + s(release_speed, release_spin_rate, k=20) 
                   + s(attack_angle, approach_angle, k=20), 
                   data = ff_py_train, family = "binomial", method = "REML")
summary(fair_foul_gam)

write_rds(fair_foul_gam, "private_data/fair_foul_gam_model.rds")

fair_foul_gam <- read_rds("private_data/fair_foul_gam_model.rds")

# Test the model on the test dataset. First, find the average rate of fair balls in the test dataset. 
#It is  0.499. 
ff_py_test %>%
  group_by(fair_ball) %>%
  count()

# Determine an optimal threshold at which to predict a fair ball. If the probability of a fair ball is above this threshold, 
#predict the batter will hit it fair(1). If not, predict they will foul off the pitch (0).  
#It seems like a good threshold that might balance the accuracy of both categories is 0.5, which makes sense because the 
#dataset is almost perfectly evenly divided between fair and foul balls. 
ff_py_test$prob <- predict(fair_foul_gam, type = "response", newdata = ff_py_test)
ff_py_test$pred[ff_py_test$prob >= .5] = 1
ff_py_test$pred[ff_py_test$prob < .5] = 0
ff_py_test$pred[is.na(ff_py_test$prob)] = 0

# Compute the overall accuracy, break down the accuracy into categories for correctly predicting 
#foul and fair balls. 
#Overall model accuracy on the test dataset is 58.2%. 63.7% of fair balls are predicted correctly 
#and 52.7% of foul balls are predicted correctly at the threshold of 05.
mean(ff_py_test$pred == ff_py_test$fair_ball) 
ff_py_test %>%
  group_by(fair_ball) %>%
  count(pred) %>%
  mutate(freq = n/sum(n))

# Create side by side boxplots for the predicted probability.
fair_foul_gam %>%
  augment(type.predict = "response") %>%
  ggplot(aes(y = .fitted, x = fair_ball, group = fair_ball)) + 
  geom_boxplot() + 
  ylab("Predicted probability of a fair ball") + 
  xlab("Acutal fair/foul (1 = fair, 0 = foul") + 
  theme_classic()

####################################################################################################################################
# EDA TO EXPLORE WHAT PITCHES ARE SWUNG AT 

# First, denote whether a pitch was swung at, create a variable called swung_at that denotes 
#with a 1 if the pitch was swung at and a 0 if the pitch was not swung at. 
batter_all_2019_lefty<- batter_all_2019 %>%
  filter(stand == "L") %>%
  mutate(plate_x = -1*plate_x)
batter_all_2019_rest <- batter_all_2019 %>%
  filter(stand != "L")
batter_all_2019_reflect <- bind_rows(batter_all_2019_lefty, batter_all_2019_rest)

swing_prob_data <- batter_all_2019_reflect %>%
  mutate(swung_at = case_when(description %in% c("ball", "blocked_ball", "called_strike", "hit_by_pitch") ~ 0, 
                              description %in% c("foul", "foul_tip", "hit_into_play", "swinging_strike", "swinging_strike_blocked") ~ 1, 
                              TRUE ~ 2), 
         swung_at = as.factor(swung_at), 
         pitch_type = case_when(pitch_type %in% c("CH", "EP") ~ "Offspeed", 
                                pitch_type %in% c("CS", "CU", "KC", "KN", "SC", "SL") ~ "Breaking", 
                                pitch_type %in% c("FA", "FO", "FS", "FT", "SI", "FC", "FF") ~ "Fastball", 
                                TRUE ~ pitch_type), 
         in_horizontal_zone = case_when(abs(plate_x) > 11/12 ~ 0, 
                                        TRUE ~ 1), 
         in_horizontal_zone = as.factor(in_horizontal_zone)) %>%
  filter(swung_at %in% c(0, 1)) %>%
  select(swung_at, pitch_type, in_horizontal_zone, everything())

# Visualization that selects 2500 pitches and creates a scatterplot of zone coordinates showing what 
#was swung at and what was taken
sample_n(swing_prob_data, 2500) %>%
  ggplot(aes(x=plate_x, y=plate_z, color=swung_at)) +
  geom_point(alpha=0.7)+
  geom_vline(xintercept = 11/12, linetype = "dashed")+
  geom_vline(xintercept = -11/12, linetype = "dashed")+
  theme_minimal()+
  scale_color_manual(values = c("firebrick", "cadetblue3"))+
  labs(x="Horizontal pitch location (ft)", y="Pitch height (ft)", 
       title = "MLB Hitter Pitch Selection: <strong><span style='color:firebrick'>Takes</span></strong></b> versus <strong><span style='color:cadetblue3'>swings</span></strong></b>", 
       subtitle = "With positive numbers representing outside pitches and negative representing inside pitches")+
  theme(plot.title.position = "plot",
        plot.title = element_markdown(size = 10), 
        axis.title = element_markdown(size=9),
        plot.subtitle = element_markdown(size=8),
        legend.position = "none")

#For pitches swung at outside of the horizontal strike zone what are more often swung at? 
fastballs <- swing_prob_data %>%
  filter(pitch_type == "Fastball") %>%
  group_by(in_horizontal_zone, swung_at) %>%
  count() %>%
  ungroup() %>%
  group_by(in_horizontal_zone) %>%
  mutate(freq = n / sum(n), 
         pitch_type = "Fastball")

offspeeds <- swing_prob_data %>%
  filter(pitch_type == "Offspeed") %>%
  group_by(in_horizontal_zone, swung_at) %>%
  count() %>%
  ungroup() %>%
  group_by(in_horizontal_zone) %>%
  mutate(freq = n / sum(n), 
         pitch_type = "Offspeed")

breakings <- swing_prob_data %>%
  filter(pitch_type == "Breaking") %>%
  group_by(in_horizontal_zone, swung_at) %>%
  count() %>%
  ungroup() %>%
  group_by(in_horizontal_zone) %>%
  mutate(freq = n / sum(n), 
         pitch_type = "Breaking")

pitch_selection <- bind_rows(fastballs, offspeeds, breakings)
pitch_selection %>%
  filter(in_horizontal_zone == 0) %>%
  ggplot(aes(x=pitch_type, y=freq, fill=swung_at))+
  geom_col(position = "fill")+
  theme_minimal()+
  labs(x = "", y="",
       title = "Offspeed pitches outside the horizontal strike zone are more often <strong><span style='color:cadetblue3'>swung at</span></strong></b> than fastballs and breaking balls")+
  scale_y_discrete(expand = c(0,0))+
  scale_fill_manual(values = c("firebrick", "cadetblue3"))+
  theme(plot.title = element_markdown(size=8), 
        legend.position = "none")