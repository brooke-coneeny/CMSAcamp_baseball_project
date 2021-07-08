library(tidyverse)
library(glmnet)
library(mgcv)
library(data.table)
library(caret)

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
batter_all_1921hp <- data.table(batter_all_1921hp)

set.seed(112)
train_i <- createDataPartition(y = batter_all_1921hp$woba_value, p = 0.7, list = FALSE)
train_mlb_data <- batter_all_1921hp[train_i,]
test_mlb_data <- batter_all_1921hp[-train_i,]

woba_model2 <- gam(woba_value ~ s(launch_angle, launch_speed), data = training_data, method = "REML")

summary(woba_model2)
coef(woba_model2)

#visuals
plot(woba_model2, scheme = 1)
plot(woba_model2, scheme = 2)

vis.gam(x=woba_model2, 
        view = c("launch_angle", "launch_speed"), #variables
        plot.type = "contour", #kind of plot
        too.far = 0.05,  
        contour.col = "black", 
        nlevels = 20) #number of lines

#logistic model for predicting whether a player hit a homerun---------------

#add a binary homerun variable to training and test data 
train_mlb_data <- train_mlb_data %>%
  mutate(homerun = events == "home_run")
test_mlb_data <- test_mlb_data %>%
  mutate(homerun = events == "home_run")

#fit model 
homerun_mod <- gam(homerun ~ s(launch_speed, launch_angle), data = train_mlb_data, 
               family = binomial, 
               method = "REML")
summary(homerun_mod)

#test model on jason heyward
jason_heyward2 <- test_mlb_data %>%
  filter(player_name == "Heyward, Jason", description == "hit_into_play") 

jhey <- tibble(hr.preds = predict(homerun_mod, newdata = jason_heyward2))
jhey_preds <- plogis(jhey$hr.preds)
mean(jhey_preds, na.rm = TRUE) #heyward has a 2.53% likelihood of hitting homerun 
#when he puts the ball in play

#test model on mike trout
mike_trout2 <- test_mlb_data %>%
  filter(player_name == "Trout, Mike", description == "hit_into_play") 

mtrout <- tibble(hr.preds = predict(homerun_mod, newdata = mike_trout2))
mtrout_preds <- plogis(mtrout$hr.preds)
mean(mtrout_preds, na.rm = TRUE) #trout has a 15% likelihood of hitting homerun 
#when he puts the ball in play

#test model on joey gallo 
joey_gallo <- test_mlb_data %>%
  filter(player_name == "Gallo, Joey", description == "hit_into_play") 
joeyg <- tibble(hr.preds = predict(homerun_mod, newdata = joey_gallo))
joeyg_preds <- plogis(joeyg$hr.preds)
mean(joeyg_preds, na.rm = TRUE) #gallo 15.8% likelihood

#test model on trea turner
trea_turner <- test_mlb_data %>%
  filter(player_name == "Turner, Trea", description == "hit_into_play") 
tt <- tibble(hr.preds = predict(homerun_mod, newdata = trea_turner))
tt_preds <- plogis(tt$hr.preds)
mean(tt_preds, na.rm = TRUE) #turner with 5.4% probability 

#see their actual percentages 
hrs_by_players <- test_mlb_data %>%
  filter(player_name %in% c("Trout, Mike", "Turner, Trea", "Heyward, Jason", "Gallo, Joey")) %>%
  group_by(player_name) %>%
  summarize(hrs = sum(homerun))

into_play <- test_mlb_data %>%
  filter(player_name %in% c("Trout, Mike", "Turner, Trea", "Heyward, Jason", "Gallo, Joey")) %>%
  group_by(player_name) %>%
  count()

hrs_by_players %>%
  inner_join(into_play, by = "player_name") %>%
  mutate(hr_percent = hrs/n)

