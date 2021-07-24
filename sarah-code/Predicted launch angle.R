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

# Function that cleans up the edges (if a launch angle is outside of the edges then replace it with the
  #+- 2sd value)
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


# Create model for attack angle and height predicting LA ------------------

# Need to calculate the attack angle for every at bat (needs to be the same for each individual player)
batter_all_2019hp <- batter_all_2019hp %>% group_by(player_name) %>% 
  filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>%
  filter(launch_speed >= quantile(launch_speed, .9, na.rm = TRUE)) %>%
  summarize(attack_angle = median(launch_angle)) %>%
  right_join(batter_all_2019hp, by = c("player_name"))

# Graph relationships between variables and LA
batter_all_2019hp %>%
  ggplot(aes(x=attack_angle, y=launch_angle))+
  geom_point(alpha=.5)+
  geom_smooth()

batter_all_2019hp %>%
  ggplot(aes(x=plate_z, y=launch_angle))+
  geom_smooth()+
  geom_point(alpha=.1)
hist(batter_all_2019hp$plate_z)

# The model we are sticking with
predicted_LA <- lm(launch_angle ~ attack_angle + plate_z, data=batter_all_2019hp)
summary(predicted_LA)
library(ggfortify)
autoplot(predicted_LA)

predicted_LA2 <- gam(launch_angle ~ s(attack_angle) + s(plate_z), data = batter_all_2019hp)
gam.check(predicted_LA2)

predicted_LA3 <- gam(launch_angle ~ s(attack_angle, plate_z, k=75), data = batter_all_2019hp)
gam.check(predicted_LA3) #worse but might need a higher k?

# Final wOBA Model -------------------------------------------------------------

final_woba_model2 <- gam(woba_value ~ s(launch_angle, launch_speed, k=200), data = batter_all_2019hp, 
                         method = "REML")


# Update attack angle -----------------------------------------------------

set.seed(2021)
# Create empty vectors of exit velocities 
EV_vector1 <- vector()    #for no change in attack angle
EV_vector2 <- vector()    #for plus one attack angle
EV_vector3 <- vector()    #for minus one attack angle


# Pass in the woba model, the LA model, the player's data for the year (the attack angle will be changed 
#in the recursive calls), the players original woba for output purposes, the original attack angle for 
#output purposes, and the attack angle which will be modified in the recursive call
predicted_LA_adjust_attack <- function(woba_model, LA_model, player_data, orig_woba, orig_attack, attack)
  {
  
  # Model the predicted angles given the original attack angle
  pred_angles <- tibble(lm.preds = predict(LA_model, newdata = player_data))
  pred_angles <- pred_angles %>% mutate(noise = rnorm(n = length(pred_angles$lm.preds), mean = 0, 
                                                      sd = sigma(LA_model)), 
                                        launch_angle = lm.preds + noise)
  
  # Need to sample the data for each predicted angle to find what exit velocity we would give it
  for(i in 1:length(pred_angles$launch_angle)){
    
    # Check all predicted launch angles and if it outside of 2 sd of their mean - replace with 
            #the cap (plus or minus)
    if(pred_angles$launch_angle[i] < (mean(player_data$launch_angle)-2*sd(player_data$launch_angle))){
      pred_angles$launch_angle[i] <- (mean(player_data$launch_angle)-2*sd(player_data$launch_angle))
    }
    else if(pred_angles$launch_angle[i] > (mean(player_data$launch_angle)+2*sd(player_data$launch_angle))){
      pred_angles$launch_angle[i] <- (mean(player_data$launch_angle)+2*sd(player_data$launch_angle))
    }
    
    # Filter for the player's launch angles plus or minus 3 degrees above the ACTUAL LA
    hits_at_angle <- player_data %>% 
      filter(launch_angle <= player_data$attack_angle[i]+3 & launch_angle >= 
               player_data$attack_angle[i]-3 & !is.na(launch_speed))
    # Randomly sample 1 exit velocity form similar hits
    EV_sample_index <- sample(1:nrow(hits_at_angle), 1, replace = TRUE)
    pred_EV <- hits_at_angle[EV_sample_index,] 
    # Add that launch speed to vector as the predicted launch speed 
    EV_vector1 <- c(EV_vector1, pred_EV$launch_speed)
  }
  
  # Merge predicted launch angle and sampled exit velocity
  modeled_data <- tibble(launch_angle = pred_angles$launch_angle, launch_speed = EV_vector1)
  preds1 <- tibble(gam.preds = predict(woba_model, newdata = modeled_data))  
  xwOBA1 <- mean(preds1$gam.preds, na.rm = TRUE)

  # Repeat with a +3 attack angle
  plus_one_attack <- player_data
  plus_one_attack$attack_angle <- plus_one_attack$attack_angle + 3
  
  pred_angles2 <- tibble(lm.preds = predict(LA_model, newdata = plus_one_attack))
  pred_angles2 <- pred_angles2 %>% mutate(noise = rnorm(n = length(pred_angles2), mean = 0, 
                                                       sd = sigma(LA_model)), 
                                         launch_angle = lm.preds + noise)
  
  # Need to sample the data for each predicted angle to find what exit velocity we would give it
  for(i in 1:length(pred_angles2$launch_angle)){
    
    # Check all predicted launch angles and if it outside of 2 sd of their mean - replace with 
    #the cap (plus or minus)
    if(pred_angles2$launch_angle[i] < (mean(player_data$launch_angle)-2*sd(player_data$launch_angle))){
      pred_angles2$launch_angle[i] <- (mean(player_data$launch_angle)-2*sd(player_data$launch_angle))
    }
    else if(pred_angles2$launch_angle[i] > (mean(player_data$launch_angle)+2*sd(player_data$launch_angle))){
      pred_angles2$launch_angle[i] <- (mean(player_data$launch_angle)+2*sd(player_data$launch_angle))
    }
    
    # Filter for the player's launch angles plus or minus 3 degrees above the ACTUAL LA
    hits_at_angle <- player_data %>% 
      filter(launch_angle <= player_data$attack_angle[i]+3 & launch_angle >= 
               player_data$attack_angle[i]-3 & !is.na(launch_speed))
    # Randomly sample 1 exit velocity form similar hits
    EV_sample_index <- sample(1:nrow(hits_at_angle), 1, replace = TRUE)
    pred_EV <- hits_at_angle[EV_sample_index,] 
    # Add that launch speed to vector as the predicted launch speed 
    EV_vector2 <- c(EV_vector2, pred_EV$launch_speed)
  }
  
  modeled_data_plus_one <- tibble(launch_angle = pred_angles2$launch_angle, launch_speed = EV_vector2)
  preds2 <- tibble(gam.preds = predict(woba_model, newdata = modeled_data_plus_one))  
  xwOBA2 <- mean(preds2$gam.preds, na.rm = TRUE)

  # Repeat with a -3 attack angle
  minus_one_attack <- player_data
  minus_one_attack$attack_angle <- minus_one_attack$attack_angle - 3
  
  pred_angles3 <- tibble(lm.preds = predict(LA_model, newdata = minus_one_attack))
  pred_angles3 <- pred_angles3 %>% mutate(noise = rnorm(n = length(pred_angles3), mean = 0, 
                                                        sd = sigma(LA_model)), 
                                          launch_angle = lm.preds + noise)
  
  # Need to sample the data for each predicted angle to find what exit velocity we would give it
  for(i in 1:length(pred_angles3$launch_angle)){
    
    # Check all predicted launch angles and if it outside of 2 sd of their mean - replace with 
    #the cap (plus or minus)
    if(pred_angles3$launch_angle[i] < (mean(player_data$launch_angle)-2*sd(player_data$launch_angle))){
      pred_angles3$launch_angle[i] <- (mean(player_data$launch_angle)-2*sd(player_data$launch_angle))
    }
    else if(pred_angles3$launch_angle[i] > (mean(player_data$launch_angle)+2*sd(player_data$launch_angle))){
      pred_angles3$launch_angle[i] <- (mean(player_data$launch_angle)+2*sd(player_data$launch_angle))
    }
    
    # Filter for the player's launch angles plus or minus 3 degrees above the ACTUAL LA
    hits_at_angle <- player_data %>% 
      filter(launch_angle <= player_data$attack_angle[i]+3 & launch_angle >= 
               player_data$attack_angle[i]-3 & !is.na(launch_speed))
    # Randomly sample 1 exit velocity form similar hits
    EV_sample_index <- sample(1:nrow(hits_at_angle), 1, replace = TRUE)
    pred_EV <- hits_at_angle[EV_sample_index,] 
    # Add that launch speed to vector as the predicted launch speed 
    EV_vector3 <- c(EV_vector3, pred_EV$launch_speed)
  }
  
  modeled_data_minus_one <- tibble(launch_angle = pred_angles3$launch_angle, launch_speed = EV_vector3)
  preds3 <- tibble(gam.preds = predict(woba_model, newdata = modeled_data_minus_one))  
  xwOBA3 <- mean(preds3$gam.preds, na.rm = TRUE)
  
  # print(player_data$attack_angle[1])
  # print(modeled_data)
  # print(min(modeled_data$launch_angle))
  # print(max(modeled_data$launch_angle))
  # print(plus_one_attack$attack_angle[1])
  # print(modeled_data_plus_one)
  # print(min(modeled_data_plus_one$launch_angle))
  # print(max(modeled_data_plus_one$launch_angle))
  # print(minus_one_attack$attack_angle[1])
  # print(modeled_data_minus_one)
  # print(min(modeled_data_minus_one$launch_angle))
  # print(max(modeled_data_minus_one$launch_angle))
  
  
  # If original < +1
  if(xwOBA1 < xwOBA2){
     # Recursively call with +1 attack angle data
    predicted_LA_adjust_attack(woba_model, LA_model, plus_one_attack, orig_woba, orig_attack, attack+3)
  }
  # Else if original < -1
  else if (xwOBA1 < xwOBA3){
    # Recursively call with -1 attack angle data
    predicted_LA_adjust_attack(woba_model, LA_model, minus_one_attack, orig_woba, orig_attack, attack-3)
  }
  # Else
  else{
    # Return the orig_woba, xwOBA, orig_attack, and attack angles
    return (tibble(original_woba = orig_woba, predicted_woba = xwOBA1, original_attack = orig_attack, 
                   reccomended_attack = attack)[1,])
  }
  
}


# Test update attack angle function ---------------------------------------

# Using Jason Heyward to test
jhey <- batter_all_2019hp %>%
  filter(player_name == "Heyward, Jason"& !is.na(plate_z) & !is.na(launch_angle), !is.na(launch_speed))%>%
  clean_edges()
jhey_woba <- mean(jhey$woba_value, na.rm = TRUE)
predicted_LA_adjust_attack(final_woba_model2, predicted_LA, jhey, jhey_woba, jhey$attack_angle, 
                           jhey$attack_angle)

# Using Mike Trout to test b/c it should be short
mtrout <- batter_all_2019hp %>%
  filter(player_name == "Trout, Mike" & !is.na(plate_z) & !is.na(launch_angle), !is.na(launch_speed)) %>%
  clean_edges()
mtrout_woba <- mean(mtrout$woba_value, na.rm = TRUE)
predicted_LA_adjust_attack(final_woba_model2, predicted_LA, mtrout, mtrout_woba, mtrout$attack_angle, 
                           mtrout$attack_angle)

# Using Joey Gallo to test 
jgallo <- batter_all_2019hp %>%
  filter(player_name == "Gallo, Joey"& !is.na(plate_z) & !is.na(launch_angle), !is.na(launch_speed))%>%
  clean_edges()
jgallo_woba <- mean(jgallo$woba_value, na.rm = TRUE)
predicted_LA_adjust_attack(final_woba_model2, predicted_LA, jgallo, jgallo_woba, jgallo$attack_angle, 
                           jgallo$attack_angle)

# Using Davis, Khris to test - only increases by 1 degree but MASSIVE difference in wOBA (making us think
#the glm for predicting launch angle isn't doing that well so maybe switch to GAM?) DIDN'T HELP
kdavis <- batter_all_2019hp %>%
  filter(player_name == "Davis, Khris"& !is.na(plate_z) & !is.na(launch_angle), !is.na(launch_speed))%>%
  clean_edges()
kdavis_woba <- mean(kdavis$woba_value, na.rm = TRUE)
predicted_LA_adjust_attack(final_woba_model2, predicted_LA, kdavis, kdavis_woba, kdavis$attack_angle, 
                           kdavis$attack_angle)

# Using Ramos, Wilson to test 
wramos <- batter_all_2019hp %>%
  filter(player_name == "Ramos, Wilson"& !is.na(plate_z) & !is.na(launch_angle), !is.na(launch_speed))%>%
  clean_edges()
wramos_woba <- mean(wramos$woba_value, na.rm = TRUE)
predicted_LA_adjust_attack(final_woba_model2, predicted_LA, wramos, wramos_woba, wramos$attack_angle, 
                           wramos$attack_angle)

# Using Kemp, Tony to test 
tkemp <- batter_all_2019hp %>%
  filter(player_name == "Kemp, Tony"& !is.na(plate_z) & !is.na(launch_angle), !is.na(launch_speed))%>%
  clean_edges()
tkemp_woba <- mean(tkemp$woba_value, na.rm = TRUE)
predicted_LA_adjust_attack(final_woba_model2, predicted_LA, tkemp, tkemp_woba, tkemp$attack_angle, 
                           tkemp$attack_angle)


# Function to call adjust attack multiple times ---------------------------

repeat_adjust_attack <- function(player_data, player_woba){
  final_results <-tibble(predicted_LA_adjust_attack(final_woba_model2, predicted_LA, player_data, player_woba, 
                                                    player_data$attack_angle, player_data$attack_angle))
  for(i in 1:99){
    results <- predicted_LA_adjust_attack(final_woba_model2, predicted_LA, player_data, player_woba, 
                             player_data$attack_angle, player_data$attack_angle)
    final_results <- bind_rows(final_results, results)
  }
  averages <- colMeans(final_results)
  final_results <- bind_rows(final_results, averages)
  return(tail(final_results))
}

repeat_adjust_attack(mtrout, mtrout_woba)
repeat_adjust_attack(tkemp, tkemp_woba)
repeat_adjust_attack(jhey, jhey_woba)
repeat_adjust_attack(jgallo, jgallo_woba) 
