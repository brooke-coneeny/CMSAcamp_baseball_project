library(tidyverse)
library(ranger)

# Data --------------------------------------------------------------------

batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2019hp <- batter_all_2019 %>%
  filter(description == "hit_into_play")
batter_all_2020 <- read_rds("private_data/all2020data.rds")
batter_all_2021 <- read_rds("private_data/all2021data.rds")

batter_all_1921 <- bind_rows(batter_all_2019, batter_all_2020, batter_all_2021)
batter_all_1921hp <- batter_all_1921 %>%
  filter(description == "hit_into_play")

# Tuning num tress by cross validation ------------------------------------

#creates the test fold
set.seed(2020)
batter_all_1921hp <- batter_all_1921hp %>% mutate(test_fold = sample(rep(1:5, length.out = n())))
batter_all_2019hp <- batter_all_2019hp %>% mutate(test_fold = sample(rep(1:5, length.out = n())))

batter_all_2019hp <- batter_all_2019hp %>% filter(!is.na(launch_speed), !is.na(launch_angle))

#calculates predictions for 5 fold cross validation for a variety of models
holdout_predictions_find_tree_num <-
  map_dfr(unique(batter_all_2019hp$test_fold),
          function(holdout){
            # Separate test and training data:
            test_data <- batter_all_2019hp %>% filter(test_fold == holdout)
            train_data <- batter_all_2019hp %>% filter(test_fold != holdout)
            
            # Train models:
            model_25 <- ranger(woba_value ~ launch_angle + launch_speed, data = train_data, 
                               num.trees = 25, importance = "impurity")
            model_50 <- ranger(woba_value ~ launch_angle + launch_speed, data = train_data, 
                               num.trees = 50, importance = "impurity")
            model_75 <- ranger(woba_value ~ launch_angle + launch_speed, data = train_data, 
                               num.trees = 75, importance = "impurity")
            model_100 <- ranger(woba_value ~ launch_angle + launch_speed, data = train_data, 
                               num.trees = 100, importance = "impurity")
            model_150 <- ranger(woba_value ~ launch_angle + launch_speed, data = train_data, 
                                num.trees = 150, importance = "impurity")
            model_500 <- ranger(woba_value ~ launch_angle + launch_speed, data = train_data, 
                                num.trees = 500, importance = "impurity")
            
            # Get predictions
            model_25_preds <- predict(model_25, data = test_data)
            model_50_preds <- predict(model_50, data = test_data)
            model_75_preds <- predict(model_75, data = test_data)
            model_100_preds <- predict(model_100, data = test_data)
            model_150_preds <- predict(model_150, data = test_data)
            model_500_preds <- predict(model_500, data = test_data)
            
            # Return tibble of holdout results:
            tibble(preds_25 = model_25_preds$predictions,
                   preds_50 = model_50_preds$predictions,
                   preds_75 = model_75_preds$predictions,
                   preds_100 = model_100_preds$predictions,
                   preds_150 = model_150_preds$predictions,
                   preds_500 = model_500_preds$predictions,
                   test_actual = test_data$woba_value, test_fold = holdout)
          })

# Graphs RMSEs for each model tested
# Tells us we should DEF use the model 2 (one interaction term) which is what I expected
holdout_predictions_find_tree_num %>%
  pivot_longer(preds_25:preds_500,
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


# Compare random forest and GAM ------------------------------------------

# The final GAM model
final_woba_model2 <- gam(woba_value ~ s(launch_angle, launch_speed, k=200), data = batter_all_2019hp, 
                         method = "REML")

# The comparison 
forest_vs_gam <-
  map_dfr(unique(batter_all_2019hp$test_fold),
          function(holdout){
            # Separate test and training data:
            test_data <- batter_all_2019hp %>% filter(test_fold == holdout)
            train_data <- batter_all_2019hp %>% filter(test_fold != holdout)
            
            # Train models:
            gam_model <- gam(woba_value ~ s(launch_angle, launch_speed, k=200), data = train_data, 
                       method = "REML")
            model_100 <- ranger(woba_value ~ launch_angle + launch_speed, data = train_data, 
                                num.trees = 100, importance = "impurity")
            
            # Get predictions for forest
            model_100_preds <- predict(model_100, data = test_data)
            
            # Return tibble of holdout results:
            tibble(gam_preds = predict(gam_model, newdata = test_data),
                   preds_100 = model_100_preds$predictions,
                   test_actual = test_data$woba_value, test_fold = holdout)
          })

# Graphs RMSEs for each model tested
# Tells us we should DEF use the model 2 (one interaction term) which is what I expected
forest_vs_gam %>%
  pivot_longer(gam_preds:preds_100,
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
