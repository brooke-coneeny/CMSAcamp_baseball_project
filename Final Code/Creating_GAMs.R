####################################################################################################################################
#This file explores  the three different GAMs we created and how we came about choosing our final model
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

#Created a data set of all possible combinations of launch angle and exit velocity to see how our model predicts woba values 
made_up_data <- expand.grid(launch_angle = seq(-50,70,2), launch_speed = seq(50,110,2))

####################################################################################################################################

#Checking to make sure there is not linear relationships between response and explanatory variables
batter_all_2019hp %>%
  ggplot(aes(x = launch_angle, y = woba_value)) +
  geom_smooth()

batter_all_2019hp %>%
  ggplot(aes(x = launch_speed, y = woba_value)) +
  geom_smooth()

####################################################################################################################################

#Our three different GAMs
woba_model_splines <- gam(woba_value ~ s(launch_angle) + s(launch_speed), 
                  data = batter_all_2019, method = "REML") 

woba_model <- gam(woba_value ~ s(launch_speed, launch_angle), 
                  data = batter_all_2019, method = "REML")

woba_model_interaction_intercepts <- gam(woba_value ~ s(launch_speed) + 
                                           s(launch_angle) + ti(launch_speed, launch_angle),
                                         data = batter_all_2019, method = "REML")

#We created rds objects for each model so that we wouldn't have to re-run them and would save time
write_rds(woba_model_splines, "public_data/woba_model_splines.rds")
write_rds(woba_model, "public_data/woba_model.rds")
write_rds(woba_model_interaction_intercepts, "public_data/woba_model_interaction_intercepts.rds")

#Loading in models from rds 
woba_model_splines <- read_rds("public_data/woba_model_splines.rds")
woba_model <- read_rds("public_data/woba_model.rds")
woba_model_interaction_intercepts <- read_rds("public_data/woba_model_interaction_intercepts.rds")

####################################################################################################################################

#We explored the raster plot each of the models would create using the made up data set and found that the wOBA model with
#only the interaction term captured the relationship the best 

#Checking the first woba model with splines 
made_up_preds1 <- tibble(gam.preds = predict(woba_model_splines, newdata = made_up_data)) 
test_data1 <- bind_cols(made_up_data, made_up_preds1)

summary(woba_model_splines)
gam.check(woba_model_splines)

raster_check1 <- test_data1 %>%
  ggplot(aes(x=launch_angle, y = launch_speed, fill = gam.preds)) +
  geom_raster() + theme_bw()


#Checking wOBA model with just an interaction term 
made_up_preds2 <- tibble(gam.preds = predict(woba_model, newdata = made_up_data)) 
test_data2 <- bind_cols(made_up_data, made_up_preds2)

summary(woba_model)
gam.check(woba_model)

#Do note this isn't as great as the raster in the presentations because we have not yet tuned k
raster_check2 <- test_data2 %>%
  ggplot(aes(x=launch_angle, y = launch_speed, fill = gam.preds)) +
  geom_raster() + theme_bw()


#Checking wOBA model with both splines and interaction term 
made_up_preds3 <- tibble(gam.preds = predict(woba_model_interaction_intercepts, newdata = made_up_data)) 
test_data3 <- bind_cols(made_up_data, made_up_preds3)

summary(woba_model_interaction_intercepts)
gam.check(woba_model_interaction_intercepts)

raster_check3 <- test_data3 %>%
  ggplot(aes(x=launch_angle, y = launch_speed, fill = gam.preds)) +
  geom_raster() + theme_bw()

####################################################################################################################################

#We wanted to compare the three models using 5-fold cross validation 
#The result: the wOBA model with the singular interaction term had the best RMSE out of the three, and since it also created the
#best raster plot in the step above, we chose this model 

#Creating the test fold
set.seed(2020)
batter_all_2019hp <- batter_all_2019hp %>% mutate(test_fold = sample(rep(1:5, length.out = n())))

#Calculating predictions
holdout_predictions <-
  map_dfr(unique(batter_all_2019hp$test_fold),
          function(holdout){
            # Separate test and training data:
            test_data <- batter_all_2019hp %>% filter(test_fold == holdout)
            train_data <- batter_all_2019hp %>% filter(test_fold != holdout)
            
            # Train models:
            woba_model_splines <- gam(woba_value ~ s(launch_angle) + s(launch_speed), 
                                        data = train_data, method = "REML")
            woba_model <- gam(woba_value ~ s(launch_angle, launch_speed, k=45), data = train_data, 
                                     method = "REML")
            woba_model_interaction_intercepts <- gam(woba_value ~ s(launch_speed) + s(launch_angle) + ti(launch_speed, launch_angle),
                                    data = train_data, method = "REML")
            
            # Return tibble of holdout results:
            tibble(woba_model_splines_preds = predict(woba_model_splines, newdata = test_data),
                   woba_model_preds = predict(woba_model, newdata = test_data),
                   woba_model_interaction_intercepts_preds = predict(woba_model_interaction_intercepts, newdata = test_data),
                   test_actual = test_data$woba_value, test_fold = holdout)
          })

# Graphs RMSEs for each model tested
RMSE_plot <- holdout_predictions %>%
  pivot_longer(woba_model_splines_preds:woba_model_interaction_intercepts_preds,
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



