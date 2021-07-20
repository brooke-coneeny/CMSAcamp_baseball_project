#Copy over Sarah's work here -----------------------------------------------------

#Attack angle and height of pitch —> model launch angle
#Get predicted launch angle, figure out what to use as exit velocity by sampling from player’s actual 
#batted balls

library(tidyverse)

# Data --------------------------------------------------------------------

batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2019hp <- batter_all_2019 %>%
  filter(description == "hit_into_play")
batter_all_2020 <- read_rds("private_data/all2020data.rds")
batter_all_2021 <- read_rds("private_data/all2021data.rds")

batter_all_1921 <- bind_rows(batter_all_2019, batter_all_2020, batter_all_2021)
batter_all_1921hp <- batter_all_1921 %>%
  filter(description == "hit_into_play")


# Create model for attack angle and height predicting LA ------------------

# Need to calculate the attack angle for every at bat (needs to be the same for each individual player)
batter_all_2019hp <- batter_all_2019hp %>% group_by(player_name) %>% 
  filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>%
  filter(launch_speed >= quantile(launch_speed, .9, na.rm = TRUE)) %>%
  summarize(attack_angle = median(launch_angle)) %>%
  right_join(batter_all_2019hp, by = c("player_name"))

predicted_LA <- glm(launch_angle ~ attack_angle + plate_z, data=batter_all_2019hp)


# Update attack angle -----------------------------------------------------

predicted_LA_adjust_attack <- function(model, player_data, orig_woba, orig_attack, attack){
  # Model the predicted angles given the original attack angle
  pred_angles <- tibble(gam.preds = predict(model, newdata = player_data))
  
  # Need to sample the data for each predicted angle to find what exit velocity we would give it
  
  # Repeat with a +1 attack angle
  
  # Repeat with a -1 attack angle
  
  # If original < +1
  # Recursively call with +1 attack angle data
  # Else if original < -1
  # Recursively call with -1 attack angle data
  # Else
  # Return the orig_woba, xwOBA, orig_attack, and attack angles
}

#erin code here----------------------------------------------------------

#plot relationship of launch angle and launch speed for Jason Heyward
batter_all_1921hp %>%
  ggplot(aes(x=launch_angle, y=launch_speed))+
  geom_point()

#trying to create function to give an expected exit velocity at a predicted launch angle

#create empty vector of exit velocities 
EV_vector <- c() 

test_function <- function(model, player_data){

  #predict angles (from Sarah)
  pred_angles <- tibble(gam.preds = predict(model, newdata = player_data))
  
  #find number of angles there are to predict 
  length_pred_angles <- length(pred_angles)
  
  #interate over angles
  for(i in 1:length_pred_angles){
    #filter for the player's launch angles plus or minus 3 degrees above the predicted LA
    hits_at_angle <- player_data %>% 
      filter(launch_angle <= pred_angles[i]+3 & launch_angle >= pred_angles[i]-3)
    #sample those hits, 10 for each predicted angle and take mean launch speed of those
    EV_sample_index <- sample(1:nrow(hits_at_angle), 10, replace = TRUE)
    pred_EV <- player_data[EV_sample_index,] %>% summarize(the_EV = mean(launch_speed))
    #add that launch speed to vector as the predicted launch speed 
    EV_vector <- c(EV_vector, pred_EV)
  }
  return(EV_vector)
}
jhey <- batter_all_2019hp %>%
  filter(player_name == "Heyward, Jason")
#FOR some reason this is only returning 1 value, not the entire vector
test_function(predicted_LA, jhey)


