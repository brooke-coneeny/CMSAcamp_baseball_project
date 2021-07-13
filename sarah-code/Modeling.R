library(tidyverse)
library(mgcv)

# Loading Data ------------------------------------------------------------

batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2019hp <- batter_all_2019 %>%
  filter(description == "hit_into_play")
batter_all_2020 <- read_rds("private_data/all2020data.rds")
batter_all_2021 <- read_rds("private_data/all2021data.rds")

batter_all_1921 <- bind_rows(batter_all_2019, batter_all_2020, batter_all_2021)
batter_all_1921hp <- batter_all_1921 %>%
  filter(description == "hit_into_play")


# Erin's first go at GAM model --------------------------------------------

woba_model <- gam(woba_value ~ s(launch_angle) + s(launch_speed), 
                  data = batter_all_2019, method = "REML")

summary(woba_model)
gam.check(woba_model)

#creates all possible combinations to see where it places value
made_up_data <- expand.grid(launch_angle = seq(-50,70,2), launch_speed = seq(50,110,2))
made_up_preds <- tibble(gam.preds = predict(woba_model, newdata = made_up_data)) 
test <- bind_cols(made_up_data, made_up_preds)

test %>%
  ggplot(aes(x=launch_angle, y = launch_speed, fill = gam.preds)) +
  geom_raster() + theme_bw()

#GAM model with 1 interaction term -------------------------------------

woba_model2 <- gam(woba_value ~ s(launch_angle, launch_speed, k=45), data = batter_all_2019, 
                   method = "REML")

summary(woba_model2)
gam.check(woba_model2)

#creates all possible combinations to see where it places value
made_up_preds2 <- tibble(gam.preds = predict(woba_model2, newdata = made_up_data)) 
test2 <- bind_cols(made_up_data, made_up_preds2)

test2 %>%
  ggplot(aes(x=launch_angle, y = launch_speed, fill = gam.preds)) +
  geom_raster() + theme_bw()


# GAM model with 3 terms --------------------------------------------------

#the ti term could use a larger k but then it never finished running
woba_model3 <- gam(woba_value ~ s(launch_speed) + s(launch_angle) + ti(launch_speed, launch_angle),
                                         data = batter_all_2019, method = "REML")
summary(woba_model3)
gam.check(woba_model3)

#creates all possible combinations to see where it places value
made_up_preds3 <- tibble(gam.preds = predict(woba_model3, newdata = made_up_data)) 
test3 <- bind_cols(made_up_data, made_up_preds3)

#DEFINITELY DOES NOT LOOK GOOD
test3 %>%
  ggplot(aes(x=launch_angle, y = launch_speed, fill = gam.preds)) +
  geom_raster() + theme_bw()


# Holdout Predictions -----------------------------------------------------

#creates the test fold
set.seed(2020)
batter_all_1921hp <- batter_all_1921hp %>% mutate(test_fold = sample(rep(1:5, length.out = n())))
batter_all_2019hp <- batter_all_2019hp %>% mutate(test_fold = sample(rep(1:5, length.out = n())))

#calculates predictions for 5 fold cross validation for a variety of models
holdout_predictions <-
  map_dfr(unique(batter_all_2019hp$test_fold),
          function(holdout){
            # Separate test and training data:
            test_data <- batter_all_2019hp %>% filter(test_fold == holdout)
            train_data <- batter_all_2019hp %>% filter(test_fold != holdout)
            
            # Train models:
            no_interaction_model <- gam(woba_value ~ s(launch_angle) + s(launch_speed), 
                                        data = train_data, method = "REML")
            interaction_model <- gam(woba_value ~ s(launch_angle, launch_speed, k=45), data = train_data, 
                                     method = "REML")
            three_term_model <- gam(woba_value ~ s(launch_speed) + s(launch_angle) + ti(launch_speed, launch_angle),
                                    data = train_data, method = "REML")
            
            # Return tibble of holdout results:
            tibble(no_interaction_preds = predict(no_interaction_model, newdata = test_data),
                   interaction_preds = predict(interaction_model, newdata = test_data),
                   three_terms_preds = predict(three_term_model, newdata = test_data),
                   test_actual = test_data$woba_value, test_fold = holdout)
          })

# Graphs RMSEs for each model tested
# Tells us we should DEF use the model 2 (one interaction term) which is what I expected
holdout_predictions %>%
  pivot_longer(no_interaction_preds:three_terms_preds,
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


# Tuning k by cross validation --------------------------------------------

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

# Calculates predictions for 5 fold cross validation for a variety of models trying to find lowest k
  #s.t. it is still within 1se of the minimum
holdout_predictions_k4 <-
  map_dfr(unique(batter_all_2019hp$test_fold),
          function(holdout){
            # Separate test and training data:
            test_data <- batter_all_2019hp %>% filter(test_fold == holdout)
            train_data <- batter_all_2019hp %>% filter(test_fold != holdout)
            
            # Train models:
            model_75 <- gam(woba_value ~ s(launch_angle, launch_speed, k=65), data = train_data, 
                            method = "REML")
            model_155 <- gam(woba_value ~ s(launch_angle, launch_speed, k=135), data = train_data, 
                            method = "REML")
            
            # Return tibble of holdout results:
            tibble(model_75_preds = predict(model_75, newdata = test_data),
                   model_155_preds = predict(model_155, newdata = test_data),
                   test_actual = test_data$woba_value, test_fold = holdout)
          })

# Graphs RMSEs for each model tested ks 
holdout_predictions_k4 %>%
  pivot_longer(model_75_preds:model_155_preds,
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
  
# Function to manually adjust launch angles -------------------------------

plus_one_LA <- function (model, data){
  #calculate starting preds
  preds1 <- tibble(gam.preds = predict(model, newdata = data))  
  #mean should give pred xwOBA for season
  xwOBA1 <- mean(preds1$gam.preds, na.rm = TRUE)   
  
  #add one to every launch angle in data
  data2 <- data
  data2$launch_angle <- data$launch_angle + 1   
  
  #redo the predictions with the updated launch angles
  preds2 <- tibble(gam.preds = predict(model, newdata = data2))
  xwOBA2 <- mean(preds2$gam.preds, na.rm = TRUE)
  
  #subtract one to every launch angle in data
  data3 <- data
  data3$launch_angle <- data$launch_angle - 1   
  
  preds3 <- tibble(gam.preds = predict(model, newdata = data3))
  xwOBA3 <- mean(preds3$gam.preds, na.rm = TRUE)
  
  #if the +1 xwOBA is larger
  if(xwOBA1 < xwOBA2){
    #do it again but the +1 is the starting data
    print(xwOBA2)
    plus_one_LA(model, data2)
  }
  #if decreasing LA is actually better
  else if(xwOBA1 < xwOBA3){
    #do it again but the -1 is the starting data
    print(xwOBA3)
    plus_one_LA(model, data3)
  }
  #if no change makes it better
  else{
    
    #return the larger xwOBA and the average launch angle that goes with it 
           #can adjust what info about the launch angle we want to return later (i.e. all values instead)
    return (tibble(xwOBA = xwOBA1, avg_launch_angle = mean(data$launch_angle, na.rm = TRUE)))
  }
}

#testing
mike_trout <- batter_all_2021 %>%
  filter(player_name == "Trout, Mike", description == "hit_into_play") 
jason_heyward <- batter_all_2021 %>%
  filter(player_name == "Heyward, Jason", description == "hit_into_play") 


plus_one_LA(woba_model, mike_trout)
plus_one_LA(woba_model, jason_heyward)

