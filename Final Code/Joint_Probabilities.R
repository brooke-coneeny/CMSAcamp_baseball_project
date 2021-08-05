####################################################################################################################################
#This file explores the joint probabilities of launch angles and exit velocities and introduces the idea
#of attack angles
#Brooke Coneeny, Sarah Sult, and Erin Franke 
#CMSAcamp 2021
####################################################################################################################################

#Loading Libraries
library(tidyverse)
library(sn)

#Loading Data
batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2019hp <- batter_all_2019 %>%
  filter(description == "hit_into_play")

#Loading GAM model
woba_model<- read_rds("public_data/woba_model.rds")

#Setting up variables
var_range <- expand.grid(launch_speed = 30:125,
                         launch_angle = -75:75)

player = 'Trout, Mike'

####################################################################################################################################

#Getting rid of all unrealistic measurements 
batter_all_2019_good <- batter_all_2019hp %>% 
  filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>% 
  filter(player_name == player)

#Creating a skew normal distribution of launch speed
launch_speed_skew_norm <- sn::selm(launch_speed ~ 1, data = batter_all_2019hp %>% 
                                     filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>% 
                                     filter(player_name == player), method = "MPLE")

#Calculating the presumed attack angle of the batter (median angle of the top 10% hardest hit balls)
attack_angle <- median((batter_all_2019 %>% 
                          filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>% 
                          filter(player_name == player) %>% 
                          filter(launch_speed >= quantile(launch_speed, .9)))$launch_angle)

#Uses the above to add an attack angle column
batter_all_2019hp <- batter_all_2019hp %>% group_by(player_name) %>% 
  filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>%
  filter(launch_speed >= quantile(launch_speed, .9, na.rm = TRUE)) %>%
  summarize(attack_angle = median(launch_angle)) %>%
  right_join(batter_all_2019hp, by = c("player_name"))

#Finding the conditional distribution of LA given EV and attack angle 
la_dist <- function(ev, la, attack, n = 100000) {
  w <- sn::psn(ev, launch_speed_skew_norm@param$dp[1],launch_speed_skew_norm@param$dp[2],
               launch_speed_skew_norm@param$dp[3])^2
  w = (exp(w)/(100 + exp(w)))^2.2
  dbeta((la + 75)/150, 1 + w * n * (attack + 75)/150, 1 + w * n - w * (n * (attack + 75)/150))
}

####################################################################################################################################

#Visualizing the density plot of combinations for the given player
##d is p(ev, la) ~ skew_normal(ev, empirically det.) * beta distribution
ggplot() + 
  geom_raster(data = var_range %>% 
                mutate(d = sn::dsn(launch_speed,launch_speed_skew_norm@param$dp[1],
                                   launch_speed_skew_norm@param$dp[2],launch_speed_skew_norm@param$dp[3]) *
                         la_dist(launch_speed, launch_angle, attack_angle)), aes(launch_speed, launch_angle, fill = d)) +
  scale_fill_gradientn(colours = rainbow(7)) +
  geom_hline(yintercept = 0) + coord_cartesian(expand = F) 

#Visualizing the skew normal distribution relative to the players actual exit velocity distribution
ggplot() + stat_density(data = batter_all_2019hp %>% filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>% 
                          filter(player_name == player) #%>% 
                        , aes(launch_speed), alpha = .2, colour = 'blue') + 
  geom_line(data = data.frame(launch_speed = 30:120) %>% mutate(density = sn::dsn(launch_speed,launch_speed_skew_norm@param$dp[1],launch_speed_skew_norm@param$dp[2],launch_speed_skew_norm@param$dp[3])), 
            aes(launch_speed, density), size = 1)

###################################################################################################################################

### Purpose: Given the predicted average wOBA for a player, increase or decrease the attack angles by 1 degree, if the wOBA
##            increases after this change, repeat the process. If it decreases, stop the search and use the previous attack angle
### Parameters: 
##   model: our gam model which predicted woba values given launch angles and exit velocities   
##   all_data: clean data for the player we are currently adjusting
##   orig_woba: the woba value found as the mean of woba_value for all hit into play balls
##   orig_attack: the player's attack angle as calculated above
##   attack: the current attack angle we are investigating
### Return: returns a tibble with the original woba, final predicted woba, original attack angle, 
              #and recommended attack angle

set.seed(2021)
adjust_attack <- function(model, all_data, orig_woba, orig_attack, attack){
  original_wOBA <- mean(all_data$woba_value)
  
  var_range <- expand.grid(launch_speed = 30:125,
                           launch_angle = -75:75)
  
  launch_speed_skew_norm <- sn::selm(launch_speed ~ 1, data = all_data %>% 
                                       filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7), 
                                     method = "MPLE")
  
  # THE STARTING ATTACK ANGLE
  #Gets the probabilities of combinations of LA/EV 
  probs1 <- var_range %>% 
    mutate(d = sn::dsn(launch_speed,launch_speed_skew_norm@param$dp[1],
                       launch_speed_skew_norm@param$dp[2],launch_speed_skew_norm@param$dp[3]) *  
             la_dist(launch_speed, launch_angle, attack),
           d_div_sum = d/sum(d))
  
  #Create mock dataset 
  sample1 <- sample(1:nrow(probs1), 300, replace = TRUE, prob = probs1$d_div_sum)
  mock1 <- probs1[sample1,] %>% select(launch_angle, launch_speed)
  
  #Predicted woba values 
  preds1 <- tibble(gam.preds = predict(model, newdata = mock1))  
  
  #Predicted average woba value 
  xwOBA1 = mean(preds1$gam.preds)
  
  # PLUS ONE ATTACK ANGLE
  #Gets the probabilities of combinations of LA/EV 
  probs2 <- var_range %>% 
    mutate(d = sn::dsn(launch_speed,launch_speed_skew_norm@param$dp[1],
                       launch_speed_skew_norm@param$dp[2],launch_speed_skew_norm@param$dp[3]) *  
             la_dist(launch_speed, launch_angle, attack+1),
           d_div_sum = d/sum(d))
  
  #Create mock dataset 
  sample2 <- sample(1:nrow(probs2), 300, replace = TRUE, prob = probs2$d_div_sum)
  mock2 <- probs2[sample2,] %>% select(launch_angle, launch_speed)
  
  #Predicted woba values 
  preds2 <- tibble(gam.preds = predict(model, newdata = mock2))  
  
  #Predicted average woba value 
  xwOBA2 = mean(preds2$gam.preds)
  
  
  # PLUS TWO ATTACK ANGLE
  #Gets the probabilities of combinations of LA/EV 
  probs3 <- var_range %>% 
    mutate(d = sn::dsn(launch_speed,launch_speed_skew_norm@param$dp[1],
                       launch_speed_skew_norm@param$dp[2],launch_speed_skew_norm@param$dp[3]) *  
             la_dist(launch_speed, launch_angle, attack-1),
           d_div_sum = d/sum(d))
  
  #Create mock dataset 
  sample3 <- sample(1:nrow(probs3), 300, replace = TRUE, prob = probs3$d_div_sum)
  mock3 <- probs3[sample3,] %>% select(launch_angle, launch_speed)
  
  #Predicted woba values 
  preds3 <- tibble(gam.preds = predict(model, newdata = mock3))  
  
  #Predicted average woba value 
  xwOBA3 = mean(preds3$gam.preds)
  
  #+1 attack better
  if(xwOBA1 < xwOBA2){
    adjust_attack(model, mock2, orig_woba, orig_attack, attack+1)
  }
  #-1 attack better
  else if(xwOBA1 < xwOBA3){
    adjust_attack(model, mock3, orig_woba, orig_attack, attack-1)
  }
  #we found the best
  else{
    return(tibble(original_attack = orig_attack, best_attack = attack, original_woba = orig_woba,
                  xwOBA = xwOBA1)[1,])
  }
  
}

#Testing this function on players

gallo_data <- batter_all_2019hp %>% filter (player_name == "Gallo, Joey")
jgallo_woba <- mean(gallo_data$woba_value, na.rm = TRUE)
adjust_attack(woba_model, gallo_data, jgallo_woba, gallo_data$attack_angle, 
              gallo_data$attack_angle)

kdavis <- batter_all_2019hp %>%
  filter(player_name == "Davis, Khris")
kdavis_woba <- mean(kdavis$woba_value, na.rm = TRUE)
adjust_attack(woba_model, kdavis, kdavis_woba, kdavis$attack_angle, 
              kdavis$attack_angle)

jhey <- batter_all_2019hp %>%
  filter(player_name == "Heyward, Jason")
jhey_woba <- mean(jhey$woba_value, na.rm = TRUE)
adjust_attack(woba_model, jhey, jhey_woba, jhey$attack_angle, 
              jhey$attack_angle)

#While this method improves on the last (it introduces the idea of attack angle and keeps the relationship
#between launch angle and exit velcity), it does not include anything to do with pitch information. Our
#next method will include the idea that how the bat interacts with the ball at impact will affect how the
#ball leaves the bat.



