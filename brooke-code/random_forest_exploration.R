library(tidyverse)
library(ranger)

#load in data
batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2019 <- batter_all_2019 %>% 
  filter(description == "hit_into_play")

#creating test folds 
set.seed(2001)
batter_all_test_fold <- batter_all_2019 %>%
  mutate(test_fold = sample(rep(1:5, length.out = n()))) %>%
  filter(!is.na(launch_speed), !is.na(launch_angle))

#determining the optimal number of trees with cross validation 
holdout_predictions <-
  map_dfr(unique(batter_all_test_fold$test_fold),
          function(holdout){
            # Separate test and training data:
            test_data <- batter_all_test_fold %>% filter(test_fold == holdout)
            train_data <- batter_all_test_fold %>% filter(test_fold != holdout)
            
            # Train models:
            rf_75 <- ranger(woba_value ~ launch_angle + launch_speed, data = train_data, 
                               num.trees = 75, importance = "impurity")
            rf_100 <- ranger(woba_value ~ launch_angle + launch_speed, data = train_data, 
                               num.trees = 100, importance = "impurity")
            rf_125  <- ranger(woba_value ~ launch_angle + launch_speed, data = train_data, 
                               num.trees = 125, importance = "impurity")
            rf_150  <- ranger(woba_value ~ launch_angle + launch_speed, data = train_data, 
                                num.trees = 150, importance = "impurity")
            
            # Return predictions
            rf_75_preds <- predict(model_75, data = test_data)
            rf_100_preds <- predict(model_100, data = test_data)
            rf_125_preds <- predict(model_125, data = test_data)
            rf_150_preds <- predict(model_150, data = test_data)
            
            
            # Return tibble of holdout results:
            tibble(rf_75_preds = rf_75_preds$predictions,
                   rf_100_preds = rf_100_preds$predictions,
                   rf_125_preds = rf_125_preds$predictions,
                   rf_150_preds = rf_150_preds$predictions,
                   test_actual = test_data$woba_value, test_fold = holdout)
          }
  )

holdout_predictions %>%
  pivot_longer(
    rf_75_preds:rf_150_preds,
    names_to = "type", values_to = "test_preds"
  ) %>%
  group_by(type, test_fold) %>%
  summarize(rmse = sqrt(mean((test_actual - test_preds)^2, na.rm = TRUE))) %>%
  ggplot(aes(x = type, y = rmse)) +
  geom_point() +
  theme_bw() +
  stat_summary(fun = mean, geom = "point", color = "red") +
  stat_summary(fun.data = mean_se, geom = "errorbar", color = "red")
