#Exploring GAM Model
library(glmnet)
library(mgcv)
library(tidyverse)

#data we are exploring 
batter_all_2019 <- read_rds("private_data/all2019data.rds")

#relationship between launch angle and exit velocity with wOBA
#for a GAM model these relationships should not be linear 

batter_all_2019 %>%
  ggplot(aes(x = launch_angle, y = woba_value)) +
  geom_smooth()

batter_all_2019 %>%
  ggplot(aes(x = launch_speed, y = woba_value)) +
  geom_smooth()

#our initial gam model 
woba_model <- gam(woba_value ~ s(launch_angle) + s(launch_speed), 
                  data = batter_all_2019, method = "REML")

#testing our model with a few players 
mike_trout <- batter_all_2021 %>%
  filter(player_name == "Trout, Mike", description == "hit_into_play") 

jason_heyward <- batter_all_2021 %>%
  filter(player_name == "Heyward, Jason", description == "hit_into_play") 

tibbletest <- tibble(gam.preds = predict(woba_model, newdata = jason_heyward))
mean(tibbletest$gam.preds, na.rm = TRUE)

#function purpose: given the predicted average wOBA, increase the all the launch 
#angles by 1 degree, if the wOBA increases after doing this, repeat process
#if it decreases, stop and go back to previous launch angles 

set.seed(5130)
changing_launch_angle <- function(player_data, woba_model) {
  #create tibble of predicted using model
  tibbletest <- tibble(gam.preds = predict(woba_model, newdata = player_data))
  #find mean woba
  woba_mean <- mean(tibbletest$gam.preds, na.rm = TRUE)
  
  #begin by increasing launch angles by 1 to see what happens to mean 
  player_data$launch_angle <- player_data$launch_angle + 1
  new_tibbletest <- tibble(gam.preds = predict(woba_model, newdata = player_data))
  new_woba_mean <- mean(tibbletest$gam.preds, na.rm = TRUE)
  
  while(flag == TRUE) {
    if(new_woba_mean > woba_mean){
      #increase launch angles 
      player_data$launch_angle <- player_data$launch_angle + 1
      flag = TRUE
    } else {
      #else less than or equal, revert back to previous launch angles
      player_data$launch_angle <- player_data$launch_angle - 1
      flag = FALSE
    }
  }
}









