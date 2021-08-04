## PURPOSE: create a logistic model for predicting whether a batted ball will be a hit
# we did not end up using this model

library(baseballr)
library(tidyverse)
library(mgcv)
library(gratia)

#load data ------------------------------------------------------------
batter_all_2016 <- read_rds("private_data/all2016data.rds")
batter_all_2017 <- read_rds("private_data/all2017data.rds")
batter_all_2018 <- read_rds("private_data/all2018data.rds")
batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2020 <- read_rds("private_data/all2020data.rds")
batter_all_2021 <- read_rds("private_data/all2021data.rds")

#create dataset of all hit into play balls for the 2016-2021 season. 
#create a column indicated whether or not the batted ball was a hit 
batter_all_1621 <- bind_rows(batter_all_2016, batter_all_2017, batter_all_2018, 
                             batter_all_2019, batter_all_2020, batter_all_2021)
batter_all_1621hp <- batter_all_1921 %>%
  filter(description == "hit_into_play") %>%
  mutate(is_hit = as.numeric(events %in% c("home_run", "single", "double", "triple"))) %>%
  filter(!is.na(launch_angle), !is.na(launch_speed),
         !is.na(is_hit))

#determine how many of all the batted balls were hits and how many were outs 
table(batter_all_1621hp$is_hit)

#create a scatterplot of launch angle on the x axis, exit velocity on the y. 
# color by whether or not the batted ball was a hit. 
batter_all_1621hp %>%
  ggplot(aes(x = launch_angle, 
             y = launch_speed,
             color = as.factor(is_hit))) +
  geom_point(alpha = 0.5) +
  ggthemes::scale_color_colorblind(labels = c("No", "Yes")) +
  labs(x = "Exit velocity", 
       y = "Launch angle", 
       color = "Hit?") +
  theme_bw() +
  theme(legend.position = "bottom")

## Fit an initial GAM ----------------------------------------------------

#set up training data
set.seed(2016)
batter_all_1621hp <-batter_all_1621hp %>%
  mutate(is_train = sample(rep(0:1, length.out = nrow(batter_all_1621hp))))

#initial function with smoothing. Does not include an interaction. 
init_hit_gam <- gam(is_hit ~ s(launch_speed) + s(launch_angle), 
                      data = filter(batter_all_1621hp, is_train == 1), 
                      family = binomial, method = "REML")
summary(init_hit_gam) #not a great R^2 

## visualizing the GAM 
draw(init_hit_gam, fun = plogis, constant = coef(init_hit_gam)[1])

## use gam.check() to see if we need more basis functions -------------
gam.check(init_hit_gam)

## check predictions 
batter_all_1621hp <- batter_all_1621hp %>%
  mutate(init_hit_gam_prob = 
           as.numeric(predict(init_hit_gam,
                              newdata = batter_all_1621hp,
                              type = "response")),
         init_hit_gam_class = as.numeric(init_hit_gam_prob >= 0.5))
batter_all_1621hp %>%
  group_by(is_train) %>%
  summarize(correct = mean(is_hit == init_hit_gam_class))

## GAM model with an interaction  ------------------------------------
hit_gam_interact <- gam(is_hit ~ s(launch_angle, launch_speed), 
                       data = filter(batter_all_1621hp, is_train == 1), 
                       family = binomial)
summary(hit_gam_interact) #R^2 is higher than before at 0.357
draw(hit_gam_interact)

#check predictions-----------------------------------------------------
batter_all_1621hp <- batter_all_1621hp %>%
  mutate(gam_interact_prob = 
           as.numeric(predict(hit_gam_interact,
                              newdata = batter_all_1621hp,
                              type = "response")),
         gam_interact_class = as.numeric(gam_interact_prob >= 0.5))
batter_all_1621hp %>%
  group_by(is_train) %>%
  summarize(correct = mean(is_hit == gam_interact_class))
