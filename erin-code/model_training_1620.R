library(tidyverse)
library(mgcv)
library(data.table)

#PURPOSE: train model on data from more years

#load data
batter_all_2016 <- read_rds("private_data/all2016data.rds")
batter_all_2017 <- read_rds("private_data/all2017data.rds")
batter_all_2018 <- read_rds("private_data/all2018data.rds")
batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2021 <- read_rds("private_data/all2021data.rds")
batter_all_2020 <- read_rds("private_data/all2020data.rds")

batter_all_1620 <- bind_rows(batter_all_2016, batter_all_2017, batter_all_2018, 
                             batter_all_2019, batter_all_2020)

batter_all_1620hp <- batter_all_1620 %>%
  filter(description == "hit_into_play") %>%
  filter(!is.na(woba_value))

#create training data --> take 50% of balls hit in play from 2016-2020
set.seed(719)
train_i <- createDataPartition(y = batter_all_1620hp$woba_value, p = 0.5, list = FALSE)
train_mlb_1620 <- batter_all_1620hp[train_i,]

#load model
woba_model2_1620 <-  gam(woba_value ~ s(launch_angle, launch_speed, k=200), data = train_mlb_1620, 
                         method = "REML")
gam.check(woba_model2_1620)

#load function -------------------------------------------------------------
changing_launch_angle <- function(player_data, woba_model, net_change) {
  #create tibble of predicted using model
  tibbletest <- tibble(gam.preds = predict(woba_model, newdata = player_data))
  #find mean woba
  woba_mean <- round(mean(tibbletest$gam.preds, na.rm = TRUE), 3)
  
  #begin by increasing launch angles by 1 to see what happens to mean 
  add_player_data <- player_data
  add_player_data$launch_angle <- player_data$launch_angle + 1
  tibbletest <- tibble(gam.preds = predict(woba_model, newdata = add_player_data))
  add_woba_mean <- mean(tibbletest$gam.preds, na.rm = TRUE)
  
  #begin by decreasing launch angles by 1 to see what happens to mean 
  subtract_player_data <- player_data
  subtract_player_data$launch_angle <- player_data$launch_angle - 1
  tibbletest <- tibble(gam.preds = predict(woba_model, newdata = subtract_player_data))
  subtract_woba_mean <- mean(tibbletest$gam.preds, na.rm = TRUE)
  
  #if increasing the angles leads to a greater woba
  if (add_woba_mean > woba_mean) {
    #increase the angles again
    net_change <- net_change + 1
    changing_launch_angle(add_player_data, woba_model, net_change)
  } else if (subtract_woba_mean > woba_mean) {
    #decrease the angles again 
    net_change <- net_change -1
    changing_launch_angle(subtract_player_data, woba_model, net_change)
  } else {
    #no change 
    return (tibble(`predicted wOBA` = woba_mean, `recommended LA` = round(mean(player_data$launch_angle, na.rm = TRUE), 0), 
                   `degrees change` = net_change))
  }
}

#first tests on players --------------------------------------------------
#test on joey gallo
joey_gallo <- batter_all_2021 %>%
  filter(player_name == "Gallo, Joey", description == "hit_into_play", 
         !is.na(launch_angle), !is.na(launch_speed)) 

result_gallo <- changing_launch_angle(joey_gallo, woba_model2_1620, 0)
result_gallo <- result_gallo %>% 
  add_column(`true wOBA` = round(mean(joey_gallo$woba_value), 3)) %>%
  add_column(`true LA` = round(mean(joey_gallo$launch_angle), 0))
result_gallo <- result_gallo %>% 
  select(`true wOBA`, `predicted wOBA`, `true LA`, `recommended LA`, `degrees change`)

#test on jason heyward
jhey <- batter_all_2021 %>%
  filter(player_name == "Heyward, Jason", description == "hit_into_play", 
         !is.na(launch_angle), !is.na(launch_speed)) 

result_jhey <- changing_launch_angle(jhey, woba_model2_1620, 0)
result_jhey <- result_jhey %>% 
  add_column(`true wOBA` = round(mean(jhey$woba_value), 3)) %>%
  add_column(`true LA` = round(mean(jhey$launch_angle), 0))
result_jhey <- result_jhey %>% 
  select(`true wOBA`, `predicted wOBA`, `true LA`, `recommended LA`, `degrees change`)
