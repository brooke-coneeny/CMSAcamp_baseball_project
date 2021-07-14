#Exploring GAM Model
library(glmnet)
library(mgcv)
library(tidyverse)
library(gratia)

#data we are exploring 
batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2021 <- read_rds("private_data/all2021data.rds")

#relationship between launch angle and exit velocity with wOBA
#for a GAM model these relationships should not be linear 

batter_all_2019 %>%
  ggplot(aes(x = launch_angle, y = woba_value)) +
  geom_smooth()

batter_all_2019 %>%
  ggplot(aes(x = launch_speed, y = woba_value)) +
  geom_smooth()

#gam models
woba_model <- gam(woba_value ~ s(launch_angle) + s(launch_speed), 
                 data = batter_all_2019, method = "REML") 

woba_model_interaction <- gam(woba_value ~ s(launch_speed, launch_angle, k = 200), 
                  data = batter_all_2019, method = "REML")

woba_model_interaction_intercepts <- gam(woba_value ~ s(launch_speed) + 
                  s(launch_angle) + ti(launch_speed, launch_angle),
                  data = batter_all_2019, method = "REML")

summary(woba_model)
summary(woba_model_interaction)
summary(woba_model_interaction_intercepts)

#visualize partial response variables
draw(woba_model)
draw(woba_model_interaction)
draw(woba_model_interaction_intercepts)

#check for number of basis functions
gam.check(woba_model)
gam.check(woba_model_interaction)
gam.check(woba_model_interaction_interacpts) #gets a different type of output 

#function purpose: given the predicted average wOBA, increase the all the launch 
#angles by 1 degree, if the wOBA increases after doing this, repeat process
#if it decreases, stop and go back to previous launch angles 

changing_launch_angle <- function(player_data, woba_model, net_change) {
  #create tibble of predicted using model
  tibbletest <- tibble(gam.preds = predict(woba_model, newdata = player_data))
  #find mean woba
  woba_mean <- mean(tibbletest$gam.preds, na.rm = TRUE)
  
  #begin by increasing launch angles by 1 to see what happens to mean 
  add_player_data <- player_data
  add_player_data$launch_angle <- player_data$launch_angle + 1
  tibbletest <- tibble(gam.preds = predict(woba_model, newdata = add_player_data))
  add_woba_mean <- mean(tibbletest$gam.preds, na.rm = TRUE)
  
  #begin by decreasing launch angles by 1 to see what happens to mean 
  subtract_player_data <- player_data
  subtract_player_data$launch_angle <- player_data$launch_angle - 1
  tibbletest <- tibble(gam.preds = predict(woba_model, newdata = subtract_player_data))
  subtract_woba_mean <- mean(tibbletest$gam.preds, na.rm = TRUE)
  
  #if increasing the angles leads to a greater woba
  if (add_woba_mean > woba_mean) {
    #increase the angles again
    net_change <- net_change + 1
    changing_launch_angle(add_player_data, woba_model, net_change)
  } else if (subtract_woba_mean > woba_mean) {
    #decrease the angles again 
    net_change <- net_change -1
    changing_launch_angle(subtract_player_data, woba_model, net_change)
  } else {
    #no change 
    return (tibble(predicted_wOBA = woba_mean, recommended_LA = mean(player_data$launch_angle, na.rm = TRUE), 
            chng_LA = net_change))
  }
}

#test on real players
mike_trout <- batter_all_2019 %>%
  filter(player_name == "Trout, Mike", description == "hit_into_play",
         !is.na(launch_angle), !is.na(launch_speed)) 

result_trout <- changing_launch_angle(mike_trout, woba_model_interaction, 0) 
result_trout <- result_trout %>% 
  add_column(true_wOBA = mean(mike_trout$woba_value)) %>%
  add_column(true_LA = mean(mike_trout$launch_angle))
result_trout <- result_trout %>% relocate(true_wOBA) %>% relocate(true_LA, .after = predicted_wOBA)

jason_heyward <- batter_all_2019 %>%
  filter(player_name == "Heyward, Jason", description == "hit_into_play", 
         !is.na(launch_angle), !is.na(launch_speed)) 

result_heyward <- changing_launch_angle(jason_heyward, woba_model_interaction, 0)
result_heyward <- result_heyward %>% 
  add_column(true_wOBA = mean(jason_heyward$woba_value)) %>%
  add_column(true_LA = mean(jason_heyward$launch_angle))
result_heyward <- result_heyward %>% relocate(true_wOBA) %>% relocate(true_LA, .after = predicted_wOBA)


#train and test model with cross validation and rmse 
set.seed(2001)
batter_all_2019 <- batter_all_2019 %>% 
  filter(description == "hit_into_play") %>%
  mutate(test_fold = sample(rep(1:5, length.out = n())))

holdout_predictions <-
  map_dfr(unique(batter_all_2019$test_fold),
          function(holdout){
            # Separate test and training data:
            test_data <- batter_all_2019 %>% filter(test_fold == holdout)
            train_data <- batter_all_2019 %>% filter(test_fold != holdout)
            
            # Train models:
            woba_model_interaction <- gam(woba_value ~ s(launch_angle, launch_speed, k=45), data = train_data, 
                                     method = "REML")
            woba_model <- gam(woba_value ~ s(launch_angle) + s(launch_speed), data = train_data, 
                                          method = "REML")
            
            # Return tibble of holdout results:
            tibble(woba_model_interaction_preds = predict(woba_model_interaction, newdata = test_data),
                   woba_model_preds = predict(woba_model, newdata = test_data),
                   test_actual = test_data$woba_value, test_fold = holdout)
          }
  )

holdout_predictions %>%
  pivot_longer(
    woba_model_interaction_preds:woba_model_preds,
    names_to = "type", values_to = "test_preds"
  ) %>%
  group_by(type, test_fold) %>%
  summarize(rmse = sqrt(mean((test_actual - test_preds)^2, na.rm = TRUE))) %>%
  ggplot(aes(x = type, y = rmse)) +
    geom_point() +
    theme_bw() +
    stat_summary(fun = mean, geom = "point", color = "red") +
    stat_summary(fun.data = mean_se, geom = "errorbar", color = "red")

#testing different values of k 
set.seed(2001)
batter_all_2019 <- batter_all_2019 %>% 
  filter(description == "hit_into_play") %>%
  mutate(test_fold = sample(rep(1:3, length.out = n())))

holdout_predictions_k <-
  map_dfr(unique(batter_all_2019$test_fold),
          function(holdout){
            # Separate test and training data:
            test_data <- batter_all_2019 %>% filter(test_fold == holdout)
            train_data <- batter_all_2019 %>% filter(test_fold != holdout)
            
            # Train models:
            woba_model_interaction_85 <- gam(woba_value ~ s(launch_angle, launch_speed, k=85), data = train_data, 
                                          method = "REML")
            woba_model_interaction_300 <- gam(woba_value ~ s(launch_angle, launch_speed, k=300), data = train_data, 
                                          method = "REML")
            
            # Return tibble of holdout results:
            tibble(preds_85 = predict(woba_model_interaction_85, newdata = test_data),
                   preds_300 = predict(woba_model_interaction_300, newdata = test_data),
                   test_actual = test_data$woba_value, test_fold = holdout)
          }
  )

holdout_predictions_k %>%
  pivot_longer(
    preds_85:preds_300,
    names_to = "type", values_to = "test_preds"
  ) %>%
  group_by(type, test_fold) %>%
  summarize(rmse = sqrt(mean((test_actual - test_preds)^2, na.rm = TRUE))) %>%
  ggplot(aes(x = type, y = rmse)) +
  geom_point() +
  theme_bw() +
  stat_summary(fun = mean, geom = "point", color = "red") +
  stat_summary(fun.data = mean_se, geom = "errorbar", color = "red")

#now that we have determined that the minimum k is very large, we want to find a k that has a consistent p value above 0.05

woba_model_interaction_200 <- gam(woba_value ~ s(launch_angle, launch_speed, k=200), data = batter_all_2019, 
                                 method = "REML")
gam.check(woba_model_interaction_200, k.sample = 50,000, k.rep = 250)
