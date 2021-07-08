library(tidyverse)
library(glmnet)
library(mgcv)
library(data.table)

batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2021 <- read_rds("private_data/all2021data.rds")
batter_all_2020 <- read_rds("private_data/all2020data.rds")

batter_all_1921 <- bind_rows(batter_all_2019, batter_all_2020, batter_all_2021)
batter_all_1921hp <- batter_all_1921 %>%
  filter(description == "hit_into_play")

woba_model <- gam(woba_value ~ s(launch_angle) + s(launch_speed), 
                  data = batter_all_2019, method = "REML")

#coefficents are both close to 9 effective degrees of freedom 
#meaning complex and wiggly relationship with woba. 
summary(woba_model)

#cannot draw horizontal line through either plot indicating significance. 
#histogram looks okay but too tall in middle, qq plot looks decent
#residual plots look kind of frightening...due to some variable not sure why yet
plot(woba_model, pages =1, all.terms = TRUE, shade = TRUE, shade.col = "lightblue")

#check model adequacy - the pvalues are not too low which is good
gam.check(woba_model, pages = 1)

#check concurvity... looks good because there are no high values 
concurvity(woba_model, full = TRUE)

#plotting launch angle and exit velocity versus woba, the plots look 
#very similar to the output of the plot gam command
batter_all_2019 %>%
  ggplot(aes(x=launch_angle, y=woba_value))+
  geom_smooth()

batter_all_2019 %>%
  ggplot(aes(x=launch_speed, y=woba_value))+
  geom_smooth()

#trying out the model with a few players 
mike_trout <- batter_all_2021 %>%
  filter(player_name == "Trout, Mike", description == "hit_into_play") 
jason_heyward <- batter_all_2021 %>%
  filter(player_name == "Heyward, Jason", description == "hit_into_play") 

#this is about 0.1 above what we would expect 
tibbletest <- tibble(gam.preds = predict(woba_model, newdata = jason_heyward))
mean(tibbletest$gam.preds, na.rm = TRUE)

# modeling..interactions?-------------------------------------------------

#create model training data
set.seed(7821)
batter_all_1921hp <- data.table(batter_all_1921hp)
training_data <- batter_all_1921hp[sample(.N, 112042)]

woba_model2 <- gam(woba_value ~ s(launch_angle, launch_speed), data = training_data, method = "REML")

summary(woba_model2)
coef(woba_model2)

plot(woba_model2, scheme = 1)
plot(woba_model2, scheme = 2)

vis.gam(x=woba_model2, 
        view = c("launch_angle", "launch_speed"), #variables
        plot.type = "contour", #kind of plot
        too.far = 0.05,  
        contour.col = "black", 
        nlevels = 20) #number of lines


