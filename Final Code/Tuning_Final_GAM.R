####################################################################################################################################
#This file tunes our final GAM by testing different values of k using 5 fold cross validation. Expect 
#these to take a LONG time to run (sometimes upward of an hour)
#Brooke Coneeny, Sarah Sult, and Erin Franke 
#CMSAcamp 2021
####################################################################################################################################

#Loading Libraries
library(tidyverse)
library(mgcv)

#Loading Data
batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2019hp <- batter_all_2019 %>%
  filter(description == "hit_into_play")

#Creating the test fold
set.seed(2020)
batter_all_2019hp <- batter_all_2019hp %>% mutate(test_fold = sample(rep(1:5, length.out = n())))

#Loading in GAM 
woba_model <- read_rds("public_data/woba_model.rds")

####################################################################################################################################

# Calculates predictions for 5 fold cross validation for a variety of models ks 40-65
holdout_predictions_k <-
  map_dfr(unique(batter_all_2019hp$test_fold),
          function(holdout){
            # Separate test and training data:
            test_data <- batter_all_2019hp %>% filter(test_fold == holdout)
            train_data <- batter_all_2019hp %>% filter(test_fold != holdout)
            
            # Train models:
            model_40 <- gam(woba_value ~ s(launch_angle, launch_speed, k=40), data = train_data, 
                            method = "REML")
            model_45 <- gam(woba_value ~ s(launch_angle, launch_speed, k=45), data = train_data, 
                            method = "REML")
            model_50 <- gam(woba_value ~ s(launch_angle, launch_speed, k=50), data = train_data, 
                            method = "REML")
            model_55 <- gam(woba_value ~ s(launch_angle, launch_speed, k=55), data = train_data, 
                            method = "REML")
            model_60 <- gam(woba_value ~ s(launch_angle, launch_speed, k=60), data = train_data, 
                            method = "REML")
            model_65 <- gam(woba_value ~ s(launch_angle, launch_speed, k=65), data = train_data, 
                            method = "REML")
            
            # Return tibble of holdout results:
            tibble(model_40_preds = predict(model_40, newdata = test_data),
                   model_45_preds = predict(model_45, newdata = test_data),
                   model_50_preds = predict(model_50, newdata = test_data),
                   model_55_preds = predict(model_55, newdata = test_data),
                   model_60_preds = predict(model_60, newdata = test_data),
                   model_65_preds = predict(model_65, newdata = test_data),
                   test_actual = test_data$woba_value, test_fold = holdout)
          })

# Graphs RMSEs for each model tested
holdout_predictions_k %>%
  pivot_longer(model_40_preds:model_65_preds,
               names_to = "type", values_to = "test_preds") %>%
  group_by(type, test_fold) %>%
  summarize(rmse = sqrt(mean((test_actual - test_preds)^2, na.rm = TRUE))) %>%
  ggplot(aes(x=type, y = rmse)) +
  geom_point() +
  theme_bw() +
  stat_summary(fun = mean, geom = "point",
               color = "red") +
  stat_summary(fun.data = mean_se,
               geom = "errorbar", color = "red")

####################################################################################################################################

# Calculates predictions for 5 fold cross validation for a variety of models ks 60-85
holdout_predictions_k2 <-
  map_dfr(unique(batter_all_2019hp$test_fold),
          function(holdout){
            # Separate test and training data:
            test_data <- batter_all_2019hp %>% filter(test_fold == holdout)
            train_data <- batter_all_2019hp %>% filter(test_fold != holdout)
            
            # Train models:
            model_60 <- gam(woba_value ~ s(launch_angle, launch_speed, k=60), data = train_data, 
                            method = "REML")
            model_65 <- gam(woba_value ~ s(launch_angle, launch_speed, k=65), data = train_data, 
                            method = "REML")
            model_70 <- gam(woba_value ~ s(launch_angle, launch_speed, k=70), data = train_data, 
                            method = "REML")
            model_75 <- gam(woba_value ~ s(launch_angle, launch_speed, k=75), data = train_data, 
                            method = "REML")
            model_80 <- gam(woba_value ~ s(launch_angle, launch_speed, k=80), data = train_data, 
                            method = "REML")
            model_85 <- gam(woba_value ~ s(launch_angle, launch_speed, k=85), data = train_data, 
                            method = "REML")
            
            # Return tibble of holdout results:
            tibble(model_60_preds = predict(model_60, newdata = test_data),
                   model_65_preds = predict(model_65, newdata = test_data),
                   model_70_preds = predict(model_70, newdata = test_data),
                   model_75_preds = predict(model_75, newdata = test_data),
                   model_80_preds = predict(model_80, newdata = test_data),
                   model_85_preds = predict(model_85, newdata = test_data),
                   test_actual = test_data$woba_value, test_fold = holdout)
          })

# Graphs RMSEs for each model tested ks 
holdout_predictions_k2 %>%
  pivot_longer(model_60_preds:model_85_preds,
               names_to = "type", values_to = "test_preds") %>%
  group_by(type, test_fold) %>%
  summarize(rmse = sqrt(mean((test_actual - test_preds)^2, na.rm = TRUE))) %>%
  ggplot(aes(x=type, y = rmse)) +
  geom_point() +
  theme_bw() +
  stat_summary(fun = mean, geom = "point",
               color = "red") +
  stat_summary(fun.data = mean_se,
               geom = "errorbar", color = "red")

####################################################################################################################################

# Calculates predictions for 5 fold cross validation for a variety of models ks 80-95
holdout_predictions_k3 <-
  map_dfr(unique(batter_all_2019hp$test_fold),
          function(holdout){
            # Separate test and training data:
            test_data <- batter_all_2019hp %>% filter(test_fold == holdout)
            train_data <- batter_all_2019hp %>% filter(test_fold != holdout)
            
            # Train models:
            model_80 <- gam(woba_value ~ s(launch_angle, launch_speed, k=80), data = train_data, 
                            method = "REML")
            model_85 <- gam(woba_value ~ s(launch_angle, launch_speed, k=85), data = train_data, 
                            method = "REML")
            model_90 <- gam(woba_value ~ s(launch_angle, launch_speed, k=90), data = train_data, 
                            method = "REML")
            model_95 <- gam(woba_value ~ s(launch_angle, launch_speed, k=95), data = train_data, 
                            method = "REML")
            
            # Return tibble of holdout results:
            tibble(model_80_preds = predict(model_80, newdata = test_data),
                   model_85_preds = predict(model_85, newdata = test_data),
                   model_90_preds = predict(model_90, newdata = test_data),
                   model_95_preds = predict(model_95, newdata = test_data),
                   test_actual = test_data$woba_value, test_fold = holdout)
          })

# Graphs RMSEs for each model tested ks 
holdout_predictions_k3 %>%
  pivot_longer(model_80_preds:model_95_preds,
               names_to = "type", values_to = "test_preds") %>%
  group_by(type, test_fold) %>%
  summarize(rmse = sqrt(mean((test_actual - test_preds)^2, na.rm = TRUE))) %>%
  ggplot(aes(x=type, y = rmse)) +
  geom_point() +
  theme_bw() +
  stat_summary(fun = mean, geom = "point",
               color = "red") +
  stat_summary(fun.data = mean_se,
               geom = "errorbar", color = "red")

####################################################################################################################################

#After lots more cross validation we came to the conclusion that a k of 200 was a good place to stop 
#We also used a gam.check with this new k value to ensure it adequately represents the complexity of the relationships btwn variables

# This shows the calculations for 100, 200, and 300 to show how the RMSEs plateaued
holdout_predictions_k4 <-
  map_dfr(unique(batter_all_2019hp$test_fold),
          function(holdout){
            # Separate test and training data:
            test_data <- batter_all_2019hp %>% filter(test_fold == holdout)
            train_data <- batter_all_2019hp %>% filter(test_fold != holdout)
            
            # Train models:
            model_100 <- gam(woba_value ~ s(launch_angle, launch_speed, k=100), data = train_data, 
                            method = "REML")
            model_200 <- gam(woba_value ~ s(launch_angle, launch_speed, k=200), data = train_data, 
                            method = "REML")
            model_300 <- gam(woba_value ~ s(launch_angle, launch_speed, k=300), data = train_data, 
                            method = "REML")
            
            # Return tibble of holdout results:
            tibble(model_100_preds = predict(model_100, newdata = test_data),
                   model_200_preds = predict(model_200, newdata = test_data),
                   model_300_preds = predict(model_300, newdata = test_data),
                   test_actual = test_data$woba_value, test_fold = holdout)
          })

# Graphs RMSEs for each model tested ks 
holdout_predictions_k4 %>%
  pivot_longer(model_100_preds:model_300_preds,
               names_to = "type", values_to = "test_preds") %>%
  group_by(type, test_fold) %>%
  summarize(rmse = sqrt(mean((test_actual - test_preds)^2, na.rm = TRUE))) %>%
  ggplot(aes(x=type, y = rmse)) +
  geom_point() +
  theme_bw() +
  stat_summary(fun = mean, geom = "point",
               color = "red") +
  stat_summary(fun.data = mean_se,
               geom = "errorbar", color = "red")

woba_model <- gam(woba_value ~ s(launch_angle, launch_speed, k=200), data = batter_all_2019hp, 
                         method = "REML")
gam.check(woba_model, k.sample = 50000, k.rep = 250)

#Update the public code with this new woba_model
write_rds(woba_model, "public_data/woba_model.rds")

