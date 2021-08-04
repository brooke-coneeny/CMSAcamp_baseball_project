# PURPOSE: to begin the modeling process for trying to maximize a player's wOBA
# using launch angle and exit velocity

#load packages and data ---------------------------------------------------
library(tidyverse)
library(glmnet)
library(mgcv)
library(data.table)
library(caret)
library(gratia)

batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2021 <- read_rds("private_data/all2021data.rds")
batter_all_2020 <- read_rds("private_data/all2020data.rds")

# create a hit into play dataset for the 2019-2021 season. 
batter_all_1921 <- bind_rows(batter_all_2019, batter_all_2020, batter_all_2021)
batter_all_1921hp <- batter_all_1921 %>%
  filter(description == "hit_into_play")

# First attempt at a wOBA model ------------------------------------------
# We are using a GAM because it is able to model the complex, non-linear relationship
# between launch angle, exit velocity, and wOBA. 
woba_model <- gam(woba_value ~ s(launch_angle) + s(launch_speed), 
                  data = batter_all_2019, method = "REML")
summary(woba_model)

#cannot draw horizontal line through either plot indicating significance. 
#histogram looks okay but too tall in middle, qq plot looks decent
#residual plots look kind of frightening...due to some variable not sure why yet
plot(woba_model, pages =1, all.terms = TRUE, shade = TRUE, shade.col = "lightblue")

#check model adequacy - the pvalues are not too low which is good
gam.check(woba_model, pages = 1)

#check concurvity... looks good because there are no high values 
concurvity(woba_model, full = TRUE)

#create a raster plot to see how well the GAM shows the relationship 
# we see that this GAM does not get at the relationship between launch angle and 
#exit velocity, as it does not capture the J shape of singles or the clump of outs
# when there is a launch angle between 20-40 but a lower exit velocity. 
made_up_data <- expand.grid(launch_angle = seq(-50,70,2), launch_speed = seq(50,110,2))
made_up_preds <- tibble(gam.preds = predict(woba_model, newdata = made_up_data)) 
test <- bind_cols(made_up_data, made_up_preds)

test %>%
  ggplot(aes(x=launch_angle, y = launch_speed, fill = gam.preds)) +
  geom_raster() + theme_bw()

#when we plot the relationship between launch angle and wOBA and exit velocity and 
# wOBA, the plots look very similar to the output of the plot GAM command
batter_all_2019 %>%
  ggplot(aes(x=launch_angle, y=woba_value))+
  geom_smooth()

batter_all_2019 %>%
  ggplot(aes(x=launch_speed, y=woba_value))+
  geom_smooth()

# Modeling: Interactions -----------------------------------------------------
# begin to experiment with interactions in order to capture that unique J shape 
# curve of singles 

#create model training and test data
batter_all_1921hp <- data.table(batter_all_1921hp)

set.seed(112)
train_i <- createDataPartition(y = batter_all_1921hp$woba_value, p = 0.7, list = FALSE)
train_mlb_data <- batter_all_1921hp[train_i,]
test_mlb_data <- batter_all_1921hp[-train_i,]

# create the wOBA model with an interaction between launch angle and exit velocity
woba_model2 <- gam(woba_value ~ s(launch_angle, launch_speed), 
                   data = training_data, method = "REML")

summary(woba_model2)
coef(woba_model2)

# create a few plots to model this GAM
plot(woba_model2, scheme = 1)
plot(woba_model2, scheme = 2)

vis.gam(x=woba_model2, 
        view = c("launch_angle", "launch_speed"), #variables
        plot.type = "contour", #kind of plot
        too.far = 0.05,  
        contour.col = "black", 
        nlevels = 20) #number of lines
