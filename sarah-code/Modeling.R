library(tidyverse)
library(mgcv)

# Loading Data ------------------------------------------------------------

batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2021 <- read_rds("private_data/all2021data.rds")


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

