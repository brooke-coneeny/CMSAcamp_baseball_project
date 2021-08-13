####################################################################################################################################
#This file intends to model strikeout probability based on different launch angles. 
#Brooke Coneeny, Sarah Sult, and Erin Franke 
#CMSAcamp 2021
####################################################################################################################################

#Loading Libraries
library(tidyverse)
library(caret)
library(e1071)
library(ROCR)
library(ROCit)

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
  mutate(description2 = case_when(description %in% c("automatic_ball", "ball", "blocked_ball", "intent_ball") ~ "ball", 
                                  description %in% c("bunt_foul_tip", "foul_bunt", "hit_by_pitch", 
                                                     "missed_bunt", "pitchout", "called_strike") ~ "other", 
                                  description %in% c("foul", "foul_tip") ~ "foul", 
                                  description %in% c("swinging_strike", "swinging_strike_blocked") ~ "swinging_strike",
                                  description %in% c("hit_into_play", "hit_into_play_no_out", "hit_into_play_score") ~ "in_play",
                                  TRUE ~ description))

#Finding each player's attack angle in each season
attack_angles <- batter_all %>%
  filter(description == "hit_into_play") %>%
  group_by(player_name, year) %>% 
  filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>%
  filter(launch_speed >= quantile(launch_speed, .9, na.rm = TRUE)) %>%
  summarize(attack_angle = median(launch_angle))

#Finding how many balls each player put in play 
hit_in_play <- batter_all %>%
  group_by(player_name, year) %>%
  filter(description == "hit_into_play") %>%
  count() %>%
  rename(balls_in_play = n)

####################################################################################################################################

#Looking at data pitch by pitch, did they make contact with the attack angle or did they miss? How is this attack angle effecting the
#amount of pitches they can hit? 

#Lets only look at pitches they fouled, hit in play, or missed. This means were ignoring pitches they did not swing at 
#If they swing and miss, there is no contact (1) if they foul or hit in play there is contact (0)
contact_all <- batter_all %>%
  filter(description2 %in% c("swinging_strike", "foul", "in_play")) %>%
  mutate(contact = case_when(description2 == "in_play" ~ 1, 
                             description2 %in% c("foul", "swinging_strike") ~ 0))

#Combining with the attack angles of each player in each year 
#Combining with number of balls in play by player and year 
contact_batter_all <- hit_in_play %>%
  filter(balls_in_play >= 50) %>%
  left_join(contact_all, by=c("player_name", "year")) %>%
  left_join(attack_angles, by = c("player_name", "year")) %>%
  filter(plate_z <=5 & plate_z >= -2.5)


####################################################################################################################################

#Creating training and testing data sets for logistic model

#Group by player and year so that all the pitches that a player swung at in a season are in either the test or train data set. 
player_num_pitches <- contact_batter_all%>%
  group_by(player_name, year) %>%
  count()

#75 percent of the sample size
smp_size <- floor(0.75 * nrow(player_num_pitches))

#Set the seed to make partition reproducible
set.seed(315)
sample_rows <- sample(nrow(player_num_pitches), smp_size)

player_year_train <- player_num_pitches[sample_rows,]
player_year_test <- player_num_pitches[-sample_rows,]

contact_train <- contact_batter_all %>%
  right_join(player_year_train, by = c("player_name", "year")) 

contact_test <- contact_batter_all %>%
  right_join(player_year_test, by = c("player_name", "year")) 

#Use training data to predict 
init_logit <- glm(contact ~ attack_angle + plate_z + plate_x + release_speed,
                  data = contact_train,
                  family = "binomial")

####################################################################################################################################

#Finding predicted values from test data 
hit_pred <- data.frame(predict(init_logit, contact_test, type = "response"))

#Find optimal cutoff probability to use to maximize accuracy 
threshold <- median(contact_test$prob <- predict(init_logit, contact_test, type = "response"))

#Convert to 1s and 0s for predicted values 
hit_pred$contact <- ifelse(hit_pred[1] >= threshold, 1, 0)
hit_pred$contact <- ifelse(hit_pred[1] < threshold, 0, 1)
hit_pred$contact <- as.factor(hit_pred$contact)

#Creating confusion matrix
#Accuracy ~ 0.52
contact_test$contact <- as.factor(contact_test$contact)
cfm <- confusionMatrix(contact_test$contact, hit_pred$contact)

#Calculate sensitivity ~ 0.641
sensitivity(contact_test$contact, hit_pred$contact)

#Calculate specificity ~ 0.403
specificity(contact_test$contact, hit_pred$contact)

#Calculate total misclassification rate ~ 0.478
mis_class_rate <- mean(hit_pred$contact != contact_test$contact)

#Creating the class and score for ROC plot
class <- init_logit$y
score <- init_logit$fitted.values

#Plotting ROC curve ~ this curve is not ideal, we want it to reach the top left corner but as as can see it 
#is very close to the middle line
ROCit_obj <- rocit(score = score,
                   class = class)
plot(ROCit_obj)



