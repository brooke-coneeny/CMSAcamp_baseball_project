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

# Load in the models
woba_model <- read_rds("public_data/woba_model.rds")
linear_model <- read_rds("public_data/LA_model.rds")

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
batter_data <- kemp_data %>%
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

# singles = 0.888
# doubles = 1.271
# triples = 1.616
# home runs = 2.101

woba_values <- batter_data %>%
  filter(!is.na(events)) %>%
  group_by(woba_value, events) %>%
  count()

quantile(predicted_values$gam.preds, probs = c(0.74, 0.76, 0.78, 0.80, 0.82, 0.84, 0.86, 0.88, 0.90, 0.92, 0.94, 0.96, 0.98, 1))

# 86th percentile is 0.8767641 (so 84th-88th is single) (0.86-0.91)

# 92nd percentile is 1.2753117 (so 88th-92nd is double) (0.87-1.3)

# 95th percentile is 1.7594451 (so 92nd-96th is triple) (1.3-1.9)

# 99th percentile is 1.9924783 (so 96th-100th is hr) (1.9-2.1)

# Categorizing the hits by percentiles listed above 
batter_data <- batter_data %>%
  mutate(pred_outcome = case_when(
    pred_woba < quantile(predicted_values$gam.preds, probs = c(0.84)) ~ "Out",
    pred_woba >= quantile(predicted_values$gam.preds, probs = c(0.84)) & 
      pred_woba < quantile(predicted_values$gam.preds, probs = c(0.88)) ~ "single",
    pred_woba >= quantile(predicted_values$gam.preds, probs = c(0.88)) &
      pred_woba < quantile(predicted_values$gam.preds, probs = c(0.92)) ~ "double",
    pred_woba >= quantile(predicted_values$gam.preds, probs = c(0.92)) &
      pred_woba < quantile(predicted_values$gam.preds, probs = c(0.96)) ~ "triple",
    pred_woba >= quantile(predicted_values$gam.preds, probs = c(0.96)) ~ "home run"
  )) 

# Find number of each events
num_events <- batter_data %>%
  group_by(pred_outcome) %>%
  summarise(num = n())

# 56 doubles, 56 home runs, 1170 outs, 55 singles , 56 triples
eq_num <- (0.888 * 55) + (1.271 * 112) + (1.616 * 56) + (2.101 * 56)
eq_den <- (56 + 56 + 1170 + 55 + 56)
woba_calc <- eq_num / eq_den

trout_woba <- mean(batter_data$woba_value)
  







