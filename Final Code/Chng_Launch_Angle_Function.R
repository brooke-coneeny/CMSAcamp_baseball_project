###################################################################################################################################
#This file contains the function we used to adjust launch angle to increase wOBA
#In the end this function we decided was not ideal because although it functionally works, it does not maintain the distribution of 
#launch angles for the player

#Brooke Coneeny, Sarah Sult, and Erin Franke 
#CMSAcamp 2021
####################################################################################################################################

#Loading Libraries
library(tidyverse)
library(mgcv)

#Loading Data
batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2019hp <- batter_all_2019 %>%
  filter(description == "hit_into_play")

#Loading in the Model
woba_model <- read_rds("public_data/woba_model.rds")

###################################################################################################################################

### Purpose: Given the predicted average wOBA for a player, increase or decrease all the launch angles by 1 degree, if the wOBA
##            increases after this change, repeat the process. If it decreases, stop the search and use the previous launch angles
### Parameters: 
##   woba_model: our gam model which predicted woba values given launch angles and exit velocities   
##   player_data: clean data for the player we are currently adjusting
##   net_change: how much the launch angles are changing as a result of the function 
##   change_in_attack: how much the attack angle has changed 
### Return: returns a tibble with the original woba, final predicted woba, and change in launch angle  


changing_launch_angle <- function(player_data, woba_model, net_change) {
  
  #create tibble of predicted using model
  tibbletest <- tibble(gam.preds = predict(woba_model, newdata = player_data))
  
  #find mean woba
  woba_mean <- round(mean(tibbletest$gam.preds, na.rm = TRUE), 3)
  
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
    return (tibble(`predicted wOBA` = woba_mean, `recommended LA` = round(mean(player_data$launch_angle, na.rm = TRUE), 0), 
                   `degrees change` = net_change))
  }
}

###################################################################################################################################

#Testing function on real players

#Testing on Mike Trout, expecting no recommendation
mike_trout <- batter_all_2019hp %>%
  filter(player_name == "Trout, Mike", description == "hit_into_play",
         !is.na(launch_angle), !is.na(launch_speed)) 

result_trout <- changing_launch_angle(mike_trout, woba_model, 0) 
result_trout <- result_trout %>% 
  add_column(true_wOBA = mean(mike_trout$woba_value)) %>%
  add_column(true_LA = mean(mike_trout$launch_angle))
result_trout <- result_trout %>% relocate(true_wOBA) %>% relocate(true_LA, .after = 'predicted wOBA')

#Testing on Jason Heyward, we are expecting the function to recommend an increase 
jason_heyward <- batter_all_2019hp %>%
  filter(player_name == "Heyward, Jason", description == "hit_into_play", 
         !is.na(launch_angle), !is.na(launch_speed)) 

result_heyward <- changing_launch_angle(jason_heyward, woba_model, 0)
result_heyward <- result_heyward %>% 
  add_column(true_wOBA = mean(jason_heyward$woba_value)) %>%
  add_column(true_LA = mean(jason_heyward$launch_angle))
result_heyward <- result_heyward %>% relocate(true_wOBA) %>% relocate(true_LA, .after = 'predicted wOBA')


