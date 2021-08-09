#Creating a model to determine if a ball will be hit into play based on the swing plane and how the ball
#comes in

library(tidyverse)
library(mgcv)


# Loading Data ------------------------------------------------------------

batter_all_2016 <- read_rds("private_data/all2016data.rds")
batter_all_2017 <- read_rds("private_data/all2017data.rds")
batter_all_2018 <- read_rds("private_data/all2018data.rds")
batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2020 <- read_rds("private_data/all2020data.rds")
batter_all_2021 <- read_rds("private_data/all2021data.rds")

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

#GET THE STRIKE PERCENTAGE FOR EACH PLAYER FOR EACH YEAR

#find each player's attack angle in each season
attack_angles <- batter_all_1621 %>%
  filter(description == "hit_into_play") %>%
  group_by(player_name, year) %>% 
  filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>%
  filter(launch_speed >= quantile(launch_speed, .9, na.rm = TRUE)) %>%
  summarize(attack_angle = median(launch_angle))

#find each player's launch angle in each season
launch_angles <- batter_all_1621 %>%
  filter(description == "hit_into_play") %>%
  group_by(player_name, year) %>% 
  filter(launch_speed <= 120)%>%
  summarize(avg_launch_angle = mean(launch_angle, na.rm = TRUE))

#find each player's number of plate appearances
plate_appearances <- batter_all_1621 %>%
  mutate(PA_id = paste(game_pk, at_bat_number, sep = "-")) %>%
  group_by(player_name, year) %>%
  summarise(n_pa = length(unique(PA_id)))

#find number of batted balls for each player
batted_balls <- batter_all_1621 %>%
  group_by(player_name, year) %>%
  filter(description == "hit_into_play") %>%
  count() %>%
  rename(balls_in_play = n)

#find each player's number of strikes
strikes <- batter_all_1621 %>%
  group_by(player_name, year) %>%
  filter(description %!in% c("hit_into_play", "ball", "hit_by_pitch")) %>%
  count() %>%
  rename(strikes=n)     
  

#find each player's wOBA each season
wOBAs <-  batter_all_1621 %>%
  group_by(player_name, year) %>%
  summarize(woba = mean(woba_value, na.rm = TRUE))

#Get total number of pitches seen by a player during a year
total_pitches <- batter_all_1621 %>%
  group_by(player_name, year) %>%
  count() %>%
  rename(total_pitches = n)

#create joined data set
strike_modeling <- plate_appearances %>%
  left_join(strikes, by = c("player_name", "year")) %>%
  left_join(attack_angles, by=c("player_name", "year")) %>%
  left_join(launch_angles, by=c("player_name", "year")) %>%
  left_join(wOBAs, by = c("player_name", "year")) %>%
  left_join(batted_balls, by = c("player_name", "year")) %>%
  left_join(total_pitches, by = c("player_name", "year")) %>%
  mutate(strike_percent = strikes/total_pitches) %>%
  filter(attack_angle <= 30 & attack_angle >=0)

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
         !is.na(pitch_type), year != 2019) #remove the year you test it on

#Adding a yes/no column for if a ball was hit into play
batter_all_2019_logit <- batter_all_2019_logit %>%
  mutate(is_hit_into_play = ifelse(description == "hit_into_play", 1, 0))
batter_all_1621_logit <- batter_all_1621_logit %>%
  mutate(is_hit_into_play = ifelse(description == "hit_into_play", 1, 0))

#read in the stats and add the player's attack angle
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

# Creating initial glm ----------------------------------------------------

init_logit <- glm(is_hit_into_play ~ attack_angle, #+ plate_z + release_speed, 
                  data = batter_all_1621_logit, family = "binomial")
summary(init_logit)

#pred_hit_outcomes <- ifelse(init_logit$fitted.values > 0.5, "hit", "no hit")

#table("Predictions" = pred_hit_outcomes, "Observed" = batter_all_2019_logit$is_hit_into_play)
max(init_logit$fitted.values)
min(init_logit$fitted.values)

#I think there are so few hit into play that when you have a a similar attack angle/plate height 
#combination the not hit into play lowers the prob of a hit

#could we use the low probabilities as the prob of selection and then select the number of balls hit into
#play based on a modeled batting average for that launch angle?????

avg_and_attack_modeling %>%
  ggplot(aes(x=attack_angle, y=ba))+
  geom_line() + geom_smooth() 

init_avg_model <- gam(ba ~ s(attack_angle, k=125), data=avg_and_attack_modeling)
summary(init_avg_model)
gam.check(init_avg_model, k.sample = 50000, k.rep = 250)


# Model strike percentage  ---------------------------------------------
strike_modeling %>%
  ggplot(aes(x=attack_angle, y=strike_percent))+
  geom_line() + geom_smooth() #practically no relationship...so the model is useless

strike_percent_init_model <- lm(strike_percent ~ attack_angle, data=strike_modeling)
summary(strike_percent_init_model)







# Function to use pred avg,  probs of hit to find wOBA --------------------

#Function that will get the sample of balls in play
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

#Test for Trout
mtrout <- batter_all_2019_logit %>%
  filter(player_name == "Trout, Mike") %>% clean_edges()
mtrout_woba <- mean(mtrout$woba_value, na.rm = TRUE)

mtrout_sample_hits <- get_sample_hits(init_avg_model, init_logit, mtrout) 

#Test for Heyward
jhey <- batter_all_2019_logit %>%
  filter(player_name == "Heyward, Jason") %>% clean_edges()
jhey_woba <- mean(jhey$woba_value, na.rm = TRUE)

jhey_sample_hits <- get_sample_hits(init_avg_model, init_logit, jhey) 

#Test for Kemp
tkemp <- batter_all_1621_logit %>%
  filter(player_name == "Kemp, Tony") %>% clean_edges()
tkemp_woba <- mean(tkemp$woba_value, na.rm = TRUE)

tkemp_sample_hits <- get_sample_hits(init_avg_model, init_logit, tkemp)

#Function from previous presentation
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

woba_model<- read_rds("public_data/woba_model.rds")
predicted_LA <- read_rds("public_data/LA_model.rds")

#Test for Trout
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


# Writing a similar function for Erin's model -----------------------------



