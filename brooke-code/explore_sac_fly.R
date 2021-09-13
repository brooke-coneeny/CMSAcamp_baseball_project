# Purpose: using the equation we found in the blog, holding walks and hit by pitch constant
# The equation we want to use
# expected_woba = woba_con / at_bats + sac_flies

# Library
library(tidyverse)
library(glmnet)
library(mgcv)


# Load in data 
trout_data <- read_rds("public_data/mtrout_sample_hits.rds")
heyward_data <- read_rds("public_data/jhey_sample_hits.rds")
kemp_data <- read_rds("public_data/tkemp_sample_hits.rds")

batter_all_2019 <- read_rds("private_data/all2019data.rds")

# Load in the models
woba_model <- read_rds("public_data/woba_model.rds")
linear_model <- read_rds("public_data/LA_model.rds")

# Choose player to work with 
batter_data <- trout_data

####################################################################################################################################

# Calculating sacs per hit into play 

sacs_per_hit_into_play <- trout_data %>%
  mutate(event_sac_fly = case_when(events == "sac_fly" ~ 1, TRUE ~ 0)) %>%
  group_by(attack_angle, event_sac_fly) %>%
  count() %>%
  pivot_wider(id_cols = attack_angle, names_from = event_sac_fly, values_from = n) %>%
  rename('sac' = '1', 'other' = '0') %>%
  replace(is.na(.), 0) %>%
  mutate(sacs_per_hip = sac/(sac+other)) %>%
  select(attack_angle, sacs_per_hip)

# Add this column to the data set

batter_data <- batter_data %>%
  left_join(sacs_per_hit_into_play, by = c("attack_angle"))

####################################################################################################################################

clean_edges <- function (data){
  for(i in 1:length(data$launch_angle)){
    if(data$launch_angle[i] < (mean(data$launch_angle)-2*sd(data$launch_angle))){
      data$cleaned_launch_angle[i] <- (mean(data$launch_angle)-2*sd(data$launch_angle))
    }
    else if(data$launch_angle[i] > (mean(data$launch_angle)+2*sd(data$launch_angle))){
      data$cleaned_launch_angle[i] <- (mean(data$launch_angle)+2*sd(data$launch_angle))
    }
    else{
      data$cleaned_launch_angle[i]<-data$launch_angle[i]
    }
  }
  return (data)
}

####################################################################################################################################

predicted_angles <- function(pred_angles, player_data){
  # Check all predicted launch angles and if it outside of 2 sd of their mean - replace with the cap (plus or minus)
  for(i in 1:length(pred_angles$launch_angle)){
    if(pred_angles$launch_angle[i] < (mean(player_data$launch_angle)-2*sd(player_data$launch_angle))){
      pred_angles$launch_angle[i] <- (mean(player_data$launch_angle)-2*sd(player_data$launch_angle))
    }
    else if(pred_angles$launch_angle[i] > (mean(player_data$launch_angle)+2*sd(player_data$launch_angle))){
      pred_angles$launch_angle[i] <- (mean(player_data$launch_angle)+2*sd(player_data$launch_angle))
    }
    else{
      pred_angles$launch_angle[i] <- pred_angles$launch_angle[i]
    }
  }
}

####################################################################################################################################

sampling <- function(batter_data, EV_vector){
  
  # Repeat for each swing since attack angle differs
  for(i in 1:nrow(batter_data)) {
    # Need to sample the data for each predicted angle to find what exit velocity we would give it 
    # Filter for the player's launch angles plus or minus 3 degrees around the attack angle 
    hits_at_angle <- batter_data %>% 
      filter(cleaned_launch_angle <= batter_data$attack_angle[i] +3 & 
               cleaned_launch_angle >= batter_data$attack_angle[i] -3 & 
               !is.na(launch_speed))
    # Randomly sample 1 exit velocity form similar hits
    EV_sample_index <- sample(1:nrow(hits_at_angle), 1, replace = TRUE)
    pred_EV <- hits_at_angle[EV_sample_index,] 
    # Add that launch speed to vector as the predicted launch speed 
    EV_vector <- c(EV_vector, pred_EV$launch_speed)
  }

  return(EV_vector)
}

####################################################################################################################################

# Create empty vectors of exit velocities 
EV_vector <- vector() 

predicted_woba_function<- function(woba_model, linear_model, batter_data){
  
  # Model the predicted angles given the original attack angle
  pred_angles <- tibble(lm.preds = predict(linear_model, newdata = batter_data))
  
  # Creating an rnorm with the standard deviation of the residuals to create noise
  pred_angles <- pred_angles %>% mutate(noise = rnorm(n = length(pred_angles$lm.preds), mean = 0, sd = sigma(linear_model)),
                                        launch_angle = lm.preds + noise)
  
  # Clean up outside angles that are unrealistic 
  predicted_angles(pred_angles, batter_data)
    
  # Use sampling to find a corresponding exit velocity for predicted launch angles 
  EV_vector <- sampling(batter_data, EV_vector)
  
  # Combine  predicted launch angles with predicted exit velocities into one tibble
  modeled_data <- tibble(launch_angle = pred_angles$launch_angle, launch_speed = EV_vector)
  
  # Predicted woba values from predicted launch angles and exit velocities
  preds <- tibble(gam.preds = predict(woba_model, newdata = modeled_data))  
  
  # Return expected wobas 
  return (preds)
  
}

####################################################################################################################################

# Cleaning the data
batter_data <- batter_data %>%
  filter(!is.na(plate_z), !is.na(launch_angle), !is.na(launch_speed), !is.na(attack_angle)) %>%
  clean_edges()

# Finding predicted woba values for each in play ball, make it a column in dataset
predicted_values <- predicted_woba_function(woba_model, linear_model, batter_data)

batter_data <- batter_data %>%
  mutate(pred_woba = predicted_values)

# Looking at distribution of these woba values
# Result for heyward: mostly pulled to the left between 0 and 0.5 
# Result for trout: mostly pulled to the left between 0 and 0.25 
woba_predictions <- predicted_values %>%
  ggplot(aes(x = gam.preds)) +
  geom_density() + 
  theme_bw()

woba_predictions_box_plot <- predicted_values %>%
  ggplot(aes(x = gam.preds)) +
  geom_boxplot() +
  theme_bw()


####################################################################################################################################

# Let us try and group the predicted woba values into single, double, triple, home runs 

# Need these numbers for woba equation 
# singles = 0.888
# doubles = 1.271
# triples = 1.616
# home runs = 2.101

# Used this to find what value was given to each event
# single = 0.9, double = 1.25, triple = 1.6, home run = 2
woba_variables <- batter_all_2019 %>%
  mutate(
    events_group = case_when(
      events %in% c("field_out", "other_out", "grounded_into_double_play", "double_play", 
                    "fielders_choice_out", "force_out", "sac_fly", "sac_fly_double_play", "sac_bunt_double_play", 
                    "field_error", "sac_fly", "fielders_choice", "triple_play") ~ "out",
      events == "single" ~ "single",
      events == "double" ~ "double", 
      events == "triple" ~ "triple", 
      events == "home_run" ~ "home run", 
      TRUE ~ "other")
  ) %>%
  select(events_group, woba_value) %>%
  group_by(events_group, woba_value) %>%
  count()

# Looking at woba predictions and seeing how they fall in line with woba value of each event 
woba_predictions <- predicted_values %>%
  ggplot(aes(x = gam.preds)) +
  geom_density() + 
  geom_vline(xintercept = 0.9, color = "red") +
  geom_vline(xintercept = 1.25, color = "red") +
  geom_vline(xintercept = 1.6, color = "red") +
  geom_vline(xintercept = 2, color = "red") +
  theme_bw()

# Idea for categorizing predicted values 
# The predicted wobas which fall into these categories will be assigned an event

# Singles:  0.9
    # difference between 1.25 - 0.9 = 0.35 / 2 = 0.175
    # singles will range from 0.9-0.175 to 0.9+0.175
    # 0.725 - 1.075

# Doubles:  1.25
    # difference between 1.6 - 1.25 = 0.35 / 2 = 0.175
    # double will range from 1.25-0.175 to 1.25+0.175
    # 1.075 - 1.425

# Triples:  1.6
    # difference between 2 - 1.6 = 0.4 / 2 = 0.2
    # triples will range from 1.6-0.175 to 1.6+0.2
    # 1.425 - 1.8

# Home Runs: 2
    # will be 1.8 and up 


# Categorize predicted woba values into events
# Find out how many of each event occured 
categorize_woba_values <- batter_data %>%
  mutate(
    pred_event_from_woba = case_when(
      pred_woba < 0.725 ~ "out",
      pred_woba >= 0.725 & pred_woba < 1.075 ~ "single",
      pred_woba >= 1.075 & pred_woba < 1.425 ~ "double",
      pred_woba >= 1.425 & pred_woba < 1.8 ~ "triple",
      pred_woba >= 1.8 ~ "home run"
    )
  ) %>%
  group_by(pred_event_from_woba) %>%
  summarise(num = n())

# Result: 36 doubles, 88 home runs, 293 singles, 29 triples, 1655 outs 
eq_num <- (0.888 * 293) + (1.271 * 36) + (1.616 * 29) + (2.101 * 88)
eq_den <- (293 + 36 + 29 + 88 + 1655)

