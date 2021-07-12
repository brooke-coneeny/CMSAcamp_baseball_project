library(tidyverse)
library(mgcv)

# Loading Data ------------------------------------------------------------

batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2020 <- read_rds("private_data/all2020data.rds")
batter_all_2021 <- read_rds("private_data/all2021data.rds")

batter_all_1921 <- bind_rows(batter_all_2019, batter_all_2020, batter_all_2021)
batter_all_1921hp <- batter_all_1921 %>%
  filter(description == "hit_into_play")


# Fitting First LM for wOBA/LA/EV (def don't use)-----------------------------------------


init_lm_LA_EV <- lm(woba_value ~ launch_angle + launch_speed, data = batter_all_2019)
summary(init_lm_LA_EV)
library(ggfortify)
autoplot(init_lm_LA_EV, ncol = 4) + theme_bw() #a HOT mess... (residualas a mess, QQ looks kinda decent,
                                        # lots of high leverage points?)


# GAM / Spline Model (not using)------------------------------------------------------

gam_model1 <- gam(woba_value ~ s(launch_angle) + s(launch_speed), data = batter_all_2019)
summary(gam_model1)


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

set.seed(2020)
batter_all_1921hp <- batter_all_1921hp %>% mutate(test_fold = sample(rep(1:5, length.out = n())))
holdout_predictions <-
  map_dfr(unique(batter_all_1921hp$test_fold),
          function(holdout){
            # Separate test and training data:
            test_data <- batter_all_1921hp %>% filter(test_fold == holdout)
            train_data <- batter_all_1921hp %>% filter(test_fold != holdout)
            
            # Train models:
            no_interaction_model <- gam(woba_value ~ s(launch_angle) + s(launch_speed), 
                                        data = batter_all_2019, method = "REML")
            interaction_model <- gam(woba_value ~ s(launch_angle, launch_speed, k=45), data = batter_all_2019, 
                                     method = "REML")
            three_term_model <- gam(woba_value ~ s(launch_speed) + s(launch_angle) + ti(launch_speed, launch_angle),
                                    data = batter_all_2019, method = "REML")
            
            # Return tibble of holdout results:
            tibble(no_interaction_preds = predict(no_interaction_model, newdata = test_data),
                   interaction_preds = predict(interaction_model, newdata = test_data),
                   three_terms_preds = predict(three_term_model, newdata = test_data),
                   test_actual = test_data$woba_value, test_fold = holdout)
          })

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

#COULD CONSIDER DOING THE SAME THING WITH DIFFERENT K'S TO FIND THE BEST MODEL
  
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

