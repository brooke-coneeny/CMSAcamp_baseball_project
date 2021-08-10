#Creating a model to determine if a ball will be hit into play based on the swing plane and how the ball
#comes in

library(tidyverse)
library(mgcv)
library(broom)

# Loading Data ------------------------------------------------------------

batter_all_2016 <- read_rds("private_data/all2016data.rds")
batter_all_2017 <- read_rds("private_data/all2017data.rds")
batter_all_2018 <- read_rds("private_data/all2018data.rds")
batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2020 <- read_rds("private_data/all2020data.rds")
batter_all_2021 <- read_rds("private_data/all2021data.rds")
woba_model<- read_rds("public_data/woba_model.rds")
predicted_LA <- read_rds("public_data/LA_model.rds")

'%!in%' <- Negate('%in%')

batter_all_2016 <- batter_all_2016 %>%
  mutate(year = "2016")
batter_all_2017 <- batter_all_2017 %>%
  mutate(year = "2017")
batter_all_2018 <- batter_all_2018 %>%
  mutate(year = "2018")
batter_all_2019 <- batter_all_2019 %>%
  mutate(year = "2019")
batter_all_2020 <- batter_all_2020 %>%
  mutate(year = "2020")
batter_all_2021 <- batter_all_2021 %>%
  mutate(year = "2021")

batter_all_1621 <- bind_rows(batter_all_2016, batter_all_2017, batter_all_2018, 
                             batter_all_2019, batter_all_2020, batter_all_2021)


#Creating a column with corresponding attack angle per player then filtering so that the variables we 
#use in the model do not have NA 
batter_all_2019_logit <- batter_all_2019 %>% 
  group_by(player_name) %>% 
  filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>%
  filter(launch_speed >= quantile(launch_speed, .9, na.rm = TRUE)) %>%
  summarize(attack_angle = median(launch_angle)) %>%
  right_join(batter_all_2019, by = c("player_name")) %>%
  filter(!is.na(plate_z), !is.na(attack_angle), !is.na(description), !is.na(release_speed),
         !is.na(pitch_type)) #MIGHT NEED TO ADD/CHANGE VARS DEPENDING ON WHAT WE USE IN THE MODEL

batter_all_1621_logit <- batter_all_1621 %>% 
  group_by(player_name) %>% 
  filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>%
  filter(launch_speed >= quantile(launch_speed, .9, na.rm = TRUE)) %>%
  summarize(attack_angle = median(launch_angle)) %>%
  right_join(batter_all_1621, by = c("player_name")) %>%
  filter(!is.na(plate_z), !is.na(attack_angle), !is.na(description), !is.na(release_speed),
         !is.na(pitch_type)) 

#Adding a yes/no column for if a ball was hit into play
batter_all_2019_logit <- batter_all_2019_logit %>%
  mutate(is_hit_into_play = ifelse(description == "hit_into_play", 1, 0))
batter_all_1621_logit <- batter_all_1621_logit %>%
  mutate(is_hit_into_play = ifelse(description == "hit_into_play", 1, 0))

# Create another training and test dataset from the batter_all dataset. This time, group by player and 
#year so that all the pitches that a player swung at in a season are in either the test or train dataset. 
player_year <- batter_all_1621_logit %>%
  group_by(player_name, year) %>%
  count()

set.seed(211)

nrow(player_year)*0.75
sample_rows2 <- sample_rows <- sample(nrow(player_year), 3900)

player_year_train <- player_year[sample_rows2,]
player_year_test <- player_year[-sample_rows2,]

hit_py_train <- batter_all_1621_logit %>%
  right_join(player_year_train, by = c("player_name", "year")) 
hit_py_test <- batter_all_1621_logit %>%
  right_join(player_year_test, by = c("player_name", "year"))


#Read in the stats and add the player's attack angle (for batting average model)
stats_2019 <- read_csv("public_data/expected_stats2019.csv")
stats_2019 <- stats_2019 %>% mutate(player_name = paste(last_name, first_name, sep=", "))
name_and_attack <- batter_all_2019_logit %>% select(player_name, attack_angle)

avg_and_attack_modeling <- stats_2019 %>% left_join(name_and_attack, by = c("player_name")) %>% distinct()

clean_edges <- function (data){
  for(i in 1:length(data$launch_angle)){
    if(!is.na(data$launch_angle[i])){
      if(data$launch_angle[i] < (mean(data$launch_angle, na.rm = TRUE)-2*sd(data$launch_angle, na.rm = TRUE))){
        data$cleaned_launch_angle[i] <- (mean(data$launch_angle, na.rm = TRUE)-2*sd(data$launch_angle, na.rm = TRUE))
      }
      else if(data$launch_angle[i] > (mean(data$launch_angle, na.rm = TRUE)+2*sd(data$launch_angle, na.rm = TRUE))){
        data$cleaned_launch_angle[i] <- (mean(data$launch_angle, na.rm = TRUE)+2*sd(data$launch_angle, na.rm = TRUE))
      }
      else{
        data$cleaned_launch_angle[i]<-data$launch_angle[i]
      }
    }
    else{
      data$cleaned_launch_angle[i]<-data$launch_angle[i]
    }
  }
  return (data)
}

########################################################################################################

#Creating a logistic model to predict whether a hit is in play or not based on attack angle and height
init_logit <- glm(is_hit_into_play ~ attack_angle + plate_z, 
                  data = hit_py_train, family = "binomial")
summary(init_logit)

#pred_hit_outcomes <- ifelse(init_logit$fitted.values > 0.5, "hit", "no hit")

#table("Predictions" = pred_hit_outcomes, "Observed" = batter_all_2019_logit$is_hit_into_play)
max(init_logit$fitted.values)
min(init_logit$fitted.values)

# Test the model on the test dataset. I played around with the threshold to split at and found that 
#0.385(same accuracy as .4) seemed to maximize the overall accuracy of the model (0.8253515). The average
#rate of hit into play is 0.1746485 in the hit_py_test dataset. 
hit_py_test$prob <- predict(init_logit, hit_py_test, type = "response")
hit_py_test$pred[hit_py_test$prob >= .385] = 1
hit_py_test$pred[hit_py_test$prob < .385] = 0
hit_py_test$pred[is.na(hit_py_test$prob)] = 0

# Compute the overall accuracy of the simpler tree
mean(hit_py_test$pred == hit_py_test$is_hit_into_play) 

# Create the confusion matrix and compute the accuracy of both predicting swings and misses 
#and also hit into play. With the threshold that maximized of the overall accuracy (0.36), the accuracy
#of predicting the "rare" event of swing and miss is very low at 0.197. 
threshold <- 0.385
init_logit %>%
  augment(type.predict = "response") %>%
  mutate(predict_hit = as.numeric(.fitted >= threshold)) %>%
  count(is_hit_into_play, predict_hit)      #SOMETHIGN REALLY WEIRD IS HAPPENING HERE

# Therefore, we should further lower the threshold in order to increase the probability of predicting the 
#rare event correctly. With a threshold of 0.285, the model predicts 84.18% of contact correctly and 
#41.7% of swing and misses correctly. The overall accuracy of the model is a bit lower at 74.55%. 
threshold <- 0.285
init_logit %>%
  augment(type.predict = "response") %>%
  mutate(predict_hit = as.numeric(.fitted >= threshold)) %>%
  count(is_hit_into_play, predict_hit)

#I think there are so few hit into play that when you have a a similar attack angle/plate height 
#combination the not hit into play lowers the prob of a hit

#If we create a model that predicts batting average based on attack angle (coming from the idea that 
#strikeouts increase with steaper attack angles) then we could use that batting average and total pitches
#the saw as the number of samples to take and use the probabilities as the sampling probability

avg_and_attack_modeling %>%
  ggplot(aes(x=attack_angle, y=ba))+
  geom_line() + geom_smooth()             #Tells me we need a GAM

#Creating this initial gam for predicting batting average
init_avg_model <- gam(ba ~ s(attack_angle, k=125), data=avg_and_attack_modeling)
summary(init_avg_model)
gam.check(init_avg_model, k.sample = 50000, k.rep = 250)    #Used this to guess the K value (can do cross
                                              #validation later to check)

########################################################################################################

#Function that will get the sample of balls they could have hit into play for each attack angle
get_sample_hits <- function(ba_model, hit_model, player_data){
  possible_attack_vec <- c()
  launch_angles <- c()
  launch_speeds <- c()
  pitch_heights <- c()
  cleaned_angles <- c()
  woba_values <- c()
  for(possible_attack in 0:30){
    player_data$attack_angle <- possible_attack
    #Get their predicted batting average for that attack angle
    pred_ba <- predict(ba_model, newdata = player_data)
    #Get the probability they hit any of the balls they were pitched with that attack angle
    probs_of_hit <- predict(hit_model, newdata = player_data, type = "response")
    player_data$prob_of_hit <- probs_of_hit
    #Find the number of hits to sample from the overall data
    num_to_sample <- round((pred_ba[1]*nrow(player_data)))
    #Sample based on the number and probabilities
    sample <- sample(1:nrow(player_data), num_to_sample[1], replace = TRUE, prob = player_data$prob_of_hit)
    las <- player_data[sample,] %>% select(launch_angle)
    lss <- player_data[sample,] %>% select(launch_speed)
    heights <- player_data[sample,] %>% select(plate_z)
    cleaned <- player_data[sample,] %>% select(cleaned_launch_angle)
    wobas <- player_data[sample,] %>% select(woba_value)
    
    possible_attack_vec <- c(possible_attack_vec, rep(possible_attack, times = num_to_sample))
    launch_angles <- c(launch_angles, las$launch_angle)
    launch_speeds <- c(launch_speeds, lss$launch_speed)
    pitch_heights <- c(pitch_heights, heights$plate_z)
    cleaned_angles <- c(cleaned_angles, cleaned$cleaned_launch_angle)
    woba_values <- c(woba_values, wobas$woba_value)
  }
  return (tibble(attack_angle = possible_attack_vec, launch_angle = launch_angles, 
                launch_speed = launch_speeds, plate_z = pitch_heights, 
                cleaned_launch_angle = cleaned_angles, woba_value = woba_values)) 
}


########################################################################################################

#Modified function from previous presentation to get predicted wobas for each attack angle
test_all_attack_sample <- function(woba_model, LA_model, player_data, year_data, orig_attack, orig_woba){
  
  # Initialize vectors for results
  original_attack <- c(rep(orig_attack, times=31))
  original_woba <- c(rep(orig_woba, times = 31))
  possible_attack_vec <- c(0:30)
  predicted_woba <- c()
  avg_predicted_woba <- c()
  
  for(possible_attack in 0:30){
    current_attack <- player_data %>% filter(attack_angle == possible_attack)
    # Repeat 10 times
    for(n in 1:10){
      EV_vector4 <- vector()    # To hold launch speeds for this function
      
      # Find the possible launch angle for this attack angle
      #current_attack$attack_angle <- possible_attack
      pred_angles <- tibble(lm.preds = predict(LA_model, newdata = current_attack))
      pred_angles <- pred_angles %>% mutate(noise = rnorm(n = length(pred_angles$lm.preds), mean = 0, 
                                                          sd = sigma(LA_model)), 
                                            launch_angle = lm.preds + noise)

      for(i in 1:length(pred_angles$launch_angle)){
        # Sample a launch speed around their actual attack angle
        hits_at_angle <- mtrout %>%     #we want to sample exit velocities from his actual data
                                  #not just the ones we sampled as potential hit into play
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


########################################################################################################

#Test for Trout
mtrout <- batter_all_2019_logit %>%
  filter(player_name == "Trout, Mike") %>% clean_edges()
mtrout_woba <- mean(mtrout$woba_value, na.rm = TRUE)

mtrout_sample_hits <- get_sample_hits(init_avg_model, init_logit, mtrout) 
mtrout_woba_values <- test_all_attack_sample(woba_model, predicted_LA, mtrout_sample_hits, mtrout,
                                             mtrout$attack_angle[1], mtrout_woba)
mtrout_attack_angles_plot <- mtrout_woba_values %>%
  ggplot(aes(x = possible_attack, y = predicted_woba)) +
  geom_line()+
  geom_smooth()+
  theme_bw()+
  geom_vline(xintercept = mtrout$attack_angle, color="red", linetype = "dashed")+
  labs(x = "Possible Attack Angles",
       y = "Predicted wOBA",
       title = "Mike Trout")

#Test for Heyward
jhey <- batter_all_2019_logit %>%
  filter(player_name == "Heyward, Jason") %>% clean_edges()
jhey_woba <- mean(jhey$woba_value, na.rm = TRUE)

jhey_sample_hits <- get_sample_hits(init_avg_model, init_logit, jhey) 
jhey_woba_values <- test_all_attack_sample(woba_model, predicted_LA, jhey_sample_hits, jhey,
                                             jhey$attack_angle[1], jhey_woba)
jhey_attack_angles_plot <- jhey_woba_values %>%
  ggplot(aes(x = possible_attack, y = predicted_woba)) +
  geom_line()+
  geom_smooth()+                                #Didn't plateau like Trout for some reason
  theme_bw()+
  geom_vline(xintercept = jhey$attack_angle, color="red", linetype = "dashed")+
  labs(x = "Possible Attack Angles",
       y = "Predicted wOBA",
       title = "Jason Heyward")

#Test for Kemp
tkemp <- batter_all_1621_logit %>%
  filter(player_name == "Kemp, Tony") %>% clean_edges()
tkemp_woba <- mean(tkemp$woba_value, na.rm = TRUE)

tkemp_sample_hits <- get_sample_hits(init_avg_model, init_logit, tkemp)
tkemp_woba_values <- test_all_attack_sample(woba_model, predicted_LA, tkemp_sample_hits, tkemp,
                                             tkemp$attack_angle[1], tkemp_woba)

tkemp_attack_angles_plot <- tkemp_woba_values %>%
  ggplot(aes(x = possible_attack, y = predicted_woba)) +
  geom_line()+
  geom_smooth()+
  theme_bw()+
  geom_vline(xintercept = tkemp$attack_angle, color="red", linetype = "dashed")+
  labs(x = "Possible Attack Angles",
       y = "Predicted wOBA",
       title = "Tony Kemp")





