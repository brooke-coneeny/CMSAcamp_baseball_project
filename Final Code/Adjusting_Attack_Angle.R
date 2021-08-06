####################################################################################################################################
#This file shows our final method for adjusting a player's attack angle (plane of swing) to find their 
#best wOBA. Please note this describes the last function as the others would cut off without changing
#the attack angle much due to some variation in calculating the wOBA.
#Brooke Coneeny, Sarah Sult, and Erin Franke 
#CMSAcamp 2021
####################################################################################################################################

#Loading Libraries
library(tidyverse)
library(glmnet)
library(mgcv)

#Loading in data 
batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2019hp <- batter_all_2019 %>%
  filter(description == "hit_into_play",
         !is.na(launch_angle), !is.na(launch_angle), !is.na(plate_z))

batter_all_2019hp <- batter_all_2019hp %>% #creating column with corresponding attack angle per player
  group_by(player_name) %>% 
  filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>%
  filter(launch_speed >= quantile(launch_speed, .9, na.rm = TRUE)) %>%
  summarize(attack_angle = median(launch_angle)) %>%
  right_join(batter_all_2019, by = c("player_name"))

#Loading in our GAM model
woba_model<- read_rds("public_data/woba_model.rds")

####################################################################################################################################

#Graph relationships between attack angle and launch angle to see if linear 
batter_all_2019hp %>%
  ggplot(aes(x=attack_angle, y=launch_angle))+
  geom_point(alpha=.5)+
  geom_smooth()

#Graph relationships between pitch height and launch angle to see if linear 
batter_all_2019hp %>%
  ggplot(aes(x=plate_z, y=launch_angle))+
  geom_smooth()+
  geom_point(alpha=.1)

#Creating a linear model which predicts launch angle based off of the attack angle the batter swings with and the height of the pitch
predicted_LA <- glm(launch_angle ~ attack_angle + plate_z, data = batter_all_2019hp)
#Be careful, this may end up being too large to push to git
write_rds(predicted_LA, "public_data/LA_model.rds")

#Now you can read it in as needed
predicted_LA <- read_rds("public_data/LA_model.rds")

####################################################################################################################################

### Purpose: cleans up the edges if a launch angle is outside of the edges then replace it with the +- 2 sd value
### Parameters: 
##    data: player data that needs to be cleaned 
### Return: new cleaned up data 

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

### Purpose: Sample the player data for each predicted launch angle to find an exit velocity 
### Parameters: 
##   pred_angles: vector of the launch angles predicted by linear model  
##   player_data: clean data for the player we are currently adjusting
##   EV_vector: vector which will hold the new predicted exit velocities 
### Return: the vector of new predicted exit velocities 

sampling <- function(pred_angles, player_data, EV_vector, orig_attack){
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
    
    # Need to sample the data for each predicted angle to find what exit velocity we would give it 
    # Filter for the player's launch angles plus or minus 3 degrees around the attack angle 
    hits_at_angle <- player_data %>% 
      filter(cleaned_launch_angle <= orig_attack +3 & cleaned_launch_angle >= orig_attack -3 & !is.na(launch_speed))
    # Randomly sample 1 exit velocity form similar hits
    EV_sample_index <- sample(1:nrow(hits_at_angle), 1, replace = TRUE)
    pred_EV <- hits_at_angle[EV_sample_index,] 
    # Add that launch speed to vector as the predicted launch speed 
    EV_vector <- c(EV_vector, pred_EV$launch_speed)
  }
  return(EV_vector)
}

####################################################################################################################################

set.seed(2021)

# Create empty vectors of exit velocities 
EV_vector1 <- vector()    #for no change in attack angle
EV_vector2 <- vector()    #for plus one attack angle
EV_vector3 <- vector()    #for minus one attack angle

# global variable to monitor change in attack angle 
change_in_attack <- 0

### Purpose: It begins by predicted woba values from expected launch angles and exit velocities, it then compares these 
##          woba values as the attack angle changes, it chooses the ideal launch angle, exit velocities, and attack angle
##          that produce the highest possible woba 
### Parameters: 
##   woba_model: our gam model which predicted woba values given launch angles and exit velocities   
##   LA_model: our linear model which predicted launch angles given an attack angle and pitch height 
##   player_data: clean data for the player we are currently adjusting
##   orig_woba: the player's woba before any adjustments in attack angle are made
##   orig_attack: the player's original attack angle before adjustment 
##   attack: the new attack angle which produces highest woba 
##   change_in_attack: how much the attack angle has changed 
### Return: returns a tibble with the original woba, final predicted woba, original attack angle, 
##         final attack angle, and change in angle

predicted_LA_adjust_attack <- function(woba_model, LA_model, player_data, orig_woba, orig_attack, attack, change_in_attack){
  # Model the predicted angles given the original attack angle
  pred_angles <- tibble(lm.preds = predict(LA_model, newdata = player_data))
  
  # Creating an rnorm with the standard deviation of the residuals to create noise
  pred_angles <- pred_angles %>% mutate(noise = rnorm(n = length(pred_angles$lm.preds), mean = 0, sd = sigma(LA_model)),
                                        launch_angle = lm.preds + noise)
  
  # Use sampling to find a corresponding exit velocity for predicted launch angles 
  EV_vector1 <- sampling(pred_angles, player_data, EV_vector1, orig_attack)
  
  # Combine  predicted launch angles with predicted exit velocities into one tibble
  modeled_data <- tibble(launch_angle = pred_angles$launch_angle, launch_speed = EV_vector1)
  
  # Predicted woba values from predicted launch angles and exit velocities
  preds1 <- tibble(gam.preds = predict(woba_model, newdata = modeled_data))  
  
  # Find the mean expected woba value for the batter 
  xwOBA1 <- mean(preds1$gam.preds, na.rm = TRUE)
  
  ##################################################
  # Repeat with a one degree increase in attack angle
  plus_one_attack <- player_data
  plus_one_attack$attack_angle <- plus_one_attack$attack_angle + 1
  
  # Model the predicted launch angles given the new increased attack angle
  pred_angles2 <- tibble(lm.preds = predict(LA_model, newdata = plus_one_attack))
  
  # Creating an rnorm with the standard deviation of the residuals to create noise
  pred_angles2 <- pred_angles2 %>% mutate(noise = rnorm(n = length(pred_angles2$lm.preds), mean = 0, sd = sigma(LA_model)),
                                          launch_angle = lm.preds + noise)
  
  # Use sampling to find a corresponding exit velocity for predicted launch angles 
  EV_vector2 <- sampling(pred_angles2, player_data, EV_vector2, orig_attack)
  
  # Combine  predicted launch angles with predicted exit velocities into one tibble
  modeled_data_plus_one <- tibble(launch_angle = pred_angles2$launch_angle, launch_speed = EV_vector2)
  
  # Predicted woba values from predicted launch angles and exit velocities
  preds2 <- tibble(gam.preds = predict(woba_model, newdata = modeled_data_plus_one))  
  
  # Find the mean expected woba value for the batter 
  xwOBA2 <- mean(preds2$gam.preds, na.rm = TRUE)
  
  ##################################################
  # Repeat with a -1 attack angle
  minus_one_attack <- player_data
  minus_one_attack$attack_angle <- minus_one_attack$attack_angle - 1
  
  # Model the predicted angles given the new decreased attack angle
  pred_angles3 <- tibble(lm.preds = predict(LA_model, newdata = minus_one_attack))
  
  # Creating an rnorm with the standard deviation of the residuals to create noise
  pred_angles3 <- pred_angles3 %>% mutate(noise = rnorm(n = length(pred_angles3$lm.preds), 
                                                        mean = 0, sd = sigma(LA_model)),launch_angle = lm.preds + noise)
  
  # Use sampling to find a corresponding exit velocity for predicted launch angles
  EV_vector3 <- sampling(pred_angles3, player_data, EV_vector3, orig_attack)
  
  # Combine  predicted launch angles with predicted exit velocities into one tibble
  modeled_data_minus_one <- tibble(launch_angle = pred_angles3$launch_angle, launch_speed = EV_vector3)
  
  # Predicted woba values from predicted launch angles and exit velocities
  preds3 <- tibble(gam.preds = predict(woba_model, newdata = modeled_data_minus_one)) 
  
  # Find the mean expected woba value for the batter 
  xwOBA3 <- mean(preds3$gam.preds, na.rm = TRUE)
  
  ##################################################
  
  # Comparing the different expected woba values 
  # If original < +1 and the +! woba is greater than the -1 woba
  if(xwOBA1 < xwOBA2 & xwOBA2 > xwOBA3){
    # Recursively call with +1 attack angle data
    change_in_attack <- change_in_attack + 1
    predicted_LA_adjust_attack(woba_model, LA_model, plus_one_attack, orig_woba, orig_attack, attack+1, change_in_attack)
  }
  # Else if original < -1
  else if (xwOBA1 < xwOBA3){
    change_in_attack <- change_in_attack - 1
    # Recursively call with -1 attack angle data
    predicted_LA_adjust_attack(woba_model, LA_model, minus_one_attack, orig_woba, orig_attack, attack-1, change_in_attack)
  }
  # Else
  else{
    change_in_attack <- change_in_attack
    # Return the orig_woba, xwOBA, orig_attack, and attack angles
    return (tibble(original_woba = orig_woba, predicted_woba = xwOBA1, original_attack = orig_attack, 
                   reccomended_attack = attack, change_in_attack = change_in_attack)[1,])
  }
}

####################################################################################################################################

### Purpose: Repeat calling the predicted woba function so that we can find an average of the results after many runs 
### Parameters: 
##   player_woba: original woba for the player 
##   player_data: clean data for the player we are currently adjusting
### Return: returns a tibble with the original woba, final predicted woba, original attack angle, 
##         final attack angle, and change in angle

repeat_adjust_attack <- function(player_data, player_woba){
  final_results <-tibble(predicted_LA_adjust_attack(woba_model, predicted_LA, player_data, player_woba, 
                                                    player_data$attack_angle, player_data$attack_angle, 0))
  for(i in 1:5){
    results <- predicted_LA_adjust_attack(woba_model, predicted_LA, player_data, player_woba, 
                                          player_data$attack_angle, player_data$attack_angle, 0)
    final_results <- bind_rows(final_results, results)
  }
  averages <- colMeans(final_results)
  final_results <- bind_rows(final_results, averages)
  return(tail(final_results))
}

####################################################################################################################################

# Using Mike Trout to test b/c it should be short
mtrout <- batter_all_2019hp %>%
  filter(player_name == "Trout, Mike" & !is.na(plate_z) & !is.na(launch_angle), !is.na(launch_speed)) %>%
  clean_edges()
mtrout_woba <- mean(mtrout$woba_value, na.rm = TRUE)
predicted_LA_adjust_attack(woba_model, predicted_LA, mtrout, mtrout_woba, mtrout$attack_angle, 
                           mtrout$attack_angle, 0)

repeat_adjust_attack(mtrout, mtrout_woba)

####################################################################################################################################

# Using Jason Heyward to test
jhey <- batter_all_2019hp %>%
  filter(player_name == "Heyward, Jason"& !is.na(plate_z) & !is.na(launch_angle), !is.na(launch_speed))%>%
  clean_edges()
jhey_woba <- mean(jhey$woba_value, na.rm = TRUE)
predicted_LA_adjust_attack(woba_model, predicted_LA, jhey, jhey_woba, jhey$attack_angle, 
                           jhey$attack_angle, 0)

repeat_adjust_attack(jhey, jhey_woba)

####################################################################################################################################
### Purpose: Compare woba values for multiple different attack angles
### Parameters: 
##   woba_model: the gam model which predicts the woba values based off launch angle and exit velocities
##   LA_model: the linear model which predicts launch angles from attack angle and pitch height 
##   player_data: clean data for the player we are currently adjusting
##   orig_attack: the original attack angle the batter is swinging at 
### Return: returns a tibble with the different attack angles and their corresponding woba values 

test_all_attack <- function(woba_model, LA_model, player_data, orig_attack){
  
  # Initialize vectors for results
  original_attack <- c(rep(orig_attack[1], times=31))
  original_woba <- c(rep(mean(player_data$woba_value, na.rm = TRUE), times = 31))
  possible_attack_vec <- c(0:30)
  predicted_woba <- c()
  avg_predicted_woba <- c()
  
  for(possible_attack in 0:30){
    # Repeat 10 times
    for(n in 1:10){
      EV_vector4 <- vector()    # To hold launch speeds for this function
      
      # Find the possible launch angle for this attack angle
      player_data$attack_angle <- possible_attack
      pred_angles <- tibble(lm.preds = predict(LA_model, newdata = player_data))
      pred_angles <- pred_angles %>% mutate(noise = rnorm(n = length(pred_angles$lm.preds), mean = 0, 
                                                          sd = sigma(LA_model)), 
                                            launch_angle = lm.preds + noise)
      
      for(i in 1:length(pred_angles$launch_angle)){
        # Sample a launch speed around their actual attack angle
        hits_at_angle <- player_data %>% 
          filter(cleaned_launch_angle <= orig_attack+3 & launch_angle >= 
                   orig_attack-3 & !is.na(launch_speed))
        # Randomly sample 1 exit velocity form similar hits
        EV_sample_index <- sample(1:nrow(hits_at_angle), 1, replace = TRUE)
        pred_EV <- hits_at_angle[EV_sample_index,] 
        # Add that launch speed to vector as the predicted launch speed 
        EV_vector4 <- c(EV_vector4, pred_EV$launch_speed)
      }
      
      # Create modeled data for this attack angle
      modeled_data <- tibble(launch_angle = pred_angles$launch_angle, launch_speed = EV_vector4)
      preds <- tibble(gam.preds = predict(woba_model, newdata = modeled_data))  
      xwOBA <- mean(preds$gam.preds, na.rm = TRUE)
      
      predicted_woba <- c(predicted_woba, xwOBA)
    }
    avg_predicted_woba <- c(avg_predicted_woba, mean(predicted_woba))
  }
  return (tibble(original_attack = original_attack, possible_attack = possible_attack_vec, 
                 original_woba = original_woba, predicted_woba = avg_predicted_woba))
  
}

####################################################################################################################################

# Testing out the plot on Mike Trout 
mtrout_woba_values <- test_all_attack(woba_model, predicted_LA, mtrout, mtrout$attack_angle)

mtrout_attack_angles_plot <- mtrout_woba_values %>%
  ggplot(aes(x = possible_attack, y = predicted_woba)) +
  geom_line()+
  geom_smooth()+
  theme_bw()+
  geom_vline(xintercept = mtrout$attack_angle, color="red", linetype = "dashed")+
  labs(x = "Possible Attack Angles",
       y = "Predicted wOBA",
       title = "Mike Trout")

# Testing out the plot on Jason Heyward
jheyward_woba_values <- test_all_attack(woba_model, predicted_LA, jheyward, jheyward$attack_angle)

jheyward_attack_angles_plot <- jheyward_woba_values %>%
  ggplot(aes(x = possible_attack, y = predicted_woba)) +
  geom_line()+
  geom_smooth()+
  theme_bw()+
  geom_vline(xintercept = jheyward$attack_angle, color="red", linetype = "dashed")+
  labs(x = "Possible Attack Angles",
       y = "Predicted wOBA",
       title = "Jason Heyward")

#We didn't expect the graph to continue trending upwards. We expected it to peak in the 20s. This is likely
#because we are assuming they successfully hit the same set of balls, but as attack angle gets very high,
#it becomes very difficult to successfully make contact with the ball (as we will investigate in future
#files). Therefore, we probably overestimated wOBA for very high attack angles. In the future we hope to
#make a logistic model that determins whether a ball will be hit into play and THEN continue with this 
#process.

