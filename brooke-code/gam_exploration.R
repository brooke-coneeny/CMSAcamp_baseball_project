#Exploring GAM Model
library(glmnet)
library(mgcv)
library(tidyverse)
library(gratia)

#data we are exploring 
batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2021 <- read_rds("private_data/all2021data.rds")

#relationship between launch angle and exit velocity with wOBA
#for a GAM model these relationships should not be linear 

batter_all_2019 %>%
  ggplot(aes(x = launch_angle, y = woba_value)) +
  geom_smooth()

batter_all_2019 %>%
  ggplot(aes(x = launch_speed, y = woba_value)) +
  geom_smooth()

#gam models
woba_model <- gam(woba_value ~ s(launch_angle) + s(launch_speed), 
                 data = batter_all_2019, method = "REML") 

woba_model_interaction <- gam(woba_value ~ s(launch_speed, launch_angle, k = 50), 
                  data = batter_all_2019, method = "REML")

woba_model_interaction_intercepts <- gam(woba_value ~ s(launch_speed) + 
                  s(launch_angle) + ti(launch_speed, launch_angle),
                  data = batter_all_2019, method = "REML")

summary(woba_model)
summary(woba_model_interaction)
summary(woba_model_interaction_intercepts)

#visualize partial response variables
draw(woba_model)
draw(woba_model_interaction)
draw(woba_model_interaction_intercepts)

#check for number of basis functions
gam.check(woba_model)
gam.check(woba_model_interaction)
gam.check(woba_model_interaction_interacpts) #gets a different type of output 

#testing our model with a few players 
mike_trout <- batter_all_2021 %>%
  filter(player_name == "Trout, Mike", description == "hit_into_play") 

aaron_judge <- batter_all_2021 %>%
  filter(player_name == "Judge, Aaron", description == "hit_into_play")

david_fletcher <- batter_all_2021 %>%
  filter(player_name == "Fletcher, David", description == "hit_into_play") 

#function purpose: given the predicted average wOBA, increase the all the launch 
#angles by 1 degree, if the wOBA increases after doing this, repeat process
#if it decreases, stop and go back to previous launch angles 

changing_launch_angle <- function(player_data, woba_model, net_change) {
  #create tibble of predicted using model
  tibbletest <- tibble(gam.preds = predict(woba_model, newdata = player_data))
  #find mean woba
  woba_mean <- mean(tibbletest$gam.preds, na.rm = TRUE)
  
  #begin by increasing launch angles by 1 to see what happens to mean 
  add_player_data <- player_data
  add_player_data$launch_angle <- player_data$launch_angle + 1
  tibbletest <- tibble(gam.preds = predict(woba_model, newdata = add_player_data))
  add_woba_mean <- mean(tibbletest$gam.preds, na.rm = TRUE)
  
  #begin by decreasing launch angles by 1 to see what happens to mean 
  subtract_player_data <- player_data
  subtract_player_data$launch_angle <- player_data$launch_angle - 1
  tibbletest <- tibble(gam.preds = predict(woba_model, newdata = subtract_player_data))
  subtract_woba_mean <- mean(tibbletest$gam.preds, na.rm = TRUE)
  
  #if increasing the angles leads to a greater woba
  if (add_woba_mean > woba_mean) {
    #increase the angles again
    net_change <- net_change + 1
    changing_launch_angle(add_player_data, woba_model, net_change)
  } else if (subtract_woba_mean > woba_mean) {
    #decrease the angles again 
    net_change <- net_change -1
    changing_launch_angle(subtract_player_data, woba_model, net_change)
  } else {
    #no change 
    return (tibble(wOBA = woba_mean, avg_launch_angle = mean(player_data$launch_angle, na.rm = TRUE), 
            chng_in_angle = net_change))
  }
}
    
changing_launch_angle(mike_trout, woba_model_interaction, 0)
changing_launch_angle(aaron_judge, woba_model_interaction, 0)
changing_launch_angle(david_fletcher, woba_model_interaction, 0)







