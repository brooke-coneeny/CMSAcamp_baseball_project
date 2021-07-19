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

# for(atbat in 1:length(batter_all_2019hp$pitch_type)){
#   batter_all_2019hp[atbat,]$attack_angle = median((batter_all_2019hp %>% 
#                                                     filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>% 
#                                                     filter(player_name == batter_all_2019hp[atbat,]$player_name) %>% 
#                                                     filter(launch_speed >= quantile(launch_speed, .9)))$launch_angle)
# }

# Need to calculate the attack angle for every at bat (needs to be the same for each individual player)
batter_all_2019hp %>% group_by(player_name) %>% 
  filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>% 
  filter(launch_speed >= quantile(launch_speed, .9)) %>%
  mutate(attack_angle = median(launch_angle)) %>%
  ungroup()

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


