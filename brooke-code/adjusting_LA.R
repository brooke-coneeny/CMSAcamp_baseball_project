library(tidyverse)

### Load in data 
batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2019 <- batter_all_2019 %>%
  filter(description == "hit_into_play")

### Our GAM Model for reference:
woba_model_interaction <- gam(woba_value ~ s(launch_speed, launch_angle, k = 200), 
                              data = batter_all_2019, method = "REML")

### Step One: create a linear model of launch angle ~ pitch height + attack angle 

# Create a column in data with the corresponding attack angle for each player
batter_all_2019 <- batter_all_2019 %>% 
  group_by(player_name) %>% 
  filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>%
  filter(launch_speed >= quantile(launch_speed, .9, na.rm = TRUE)) %>%
  summarize(attack_angle = median(launch_angle)) %>%
  right_join(batter_all_2019, by = c("player_name")) 

# Create linear model with the variables 
predicted_LA <- glm(launch_angle ~ attack_angle.x + plate_z, data = batter_all_2019)

### Step Two: Create Function: Run all observations of height and attack angle through this linear model 
###                 to produce a new column with predicted launch angles
batter_all_2019 <- batter_all_2019 %>%
  mutate(
    pred_la = predict(predicted_LA, newdata = batter_all_2019)
  )

### Step Three: Use sampling to find predicted exit velocity of each observation 

#create empty vector of exit velocities 
EV_vector <- c() 

#function to find predicted exit velocities 
pred_ev_function <- function(player_data, EV_vector){
  
  #interate over every observation in player data 
  for(i in 1:nrow(player_data)){
    #filter for the player's launch angles plus or minus 3 degrees above the predicted LA
    current_la <- player_data[i,2]
    sample_la <- player_data %>% 
      filter(launch_angle <= current_la+3 & launch_angle >= current_la-3)
    #sample those hits, 10 for each predicted angle and take mean launch speed of those
    random_sample <- sample(1:nrow(sample_la), 10, replace = TRUE)
    predicted_exit_velocity <- mean(random_sample)
    player_data %>% mutate(pred_ev[i,2] = predicted_exit_velocity)
    EV_vector.append(predicted_exit_velocity)
  }
  return(NULL)
}

#test on joey gallo
joey_gallo <- batter_all_2019 %>%
  filter(player_name == "Gallo, Joey") %>%
  select(player_name, launch_angle, launch_speed, attack_angle.x, plate_z)

pred_ev_function(joey_gallo)



 





  
