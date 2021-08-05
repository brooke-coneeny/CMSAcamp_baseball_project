####################################################################################################################################
#This file explores the joint porbabilities of launch angles and exit velocities
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

#Setting up variables
var_range <- expand.grid(launch_speed = 30:125,
                         launch_angle = -75:75)

player = 'Trout, Mike'

####################################################################################################################################

#Getting rid of all unrealistic measurements 
batter_all_2019_good <- batter_all_2019hp %>% 
  filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>% 
  filter(player_name == player)

#Creating a skew normal dist of launch speed
launch_speed_skew_norm <- sn::selm(launch_speed ~ 1, data = batter_all_2019hp %>% 
                                     filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>% 
                                     filter(player_name == player), method = "MPLE")

#Calculating the presumed attack angle of the batter (median angle of the top 10% hardest hit balls)
attack_angle <- median((batter_all_2019 %>% 
                          filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>% 
                          filter(player_name == player) %>% 
                          filter(launch_speed >= quantile(launch_speed, .9)))$launch_angle)

#Finding the conditional distribution of LA given EV and attack angle 
#d is p(ev, la) ~ skew_normal(ev, empricially det.) * beta distribution
la_dist <- function(ev, la, attack, n = 100000) {
  w <- sn::psn(ev, launch_speed_skew_norm@param$dp[1],launch_speed_skew_norm@param$dp[2],
               launch_speed_skew_norm@param$dp[3])^2
  w = (exp(w)/(100 + exp(w)))^2.2
  dbeta((la + 75)/150, 1 + w * n * (attack + 75)/150, 1 + w * n - w * (n * (attack + 75)/150))
}

####################################################################################################################################

#Visualizing 
ggplot() + 
  geom_raster(data = var_range %>% 
                mutate(d = sn::dsn(launch_speed,launch_speed_skew_norm@param$dp[1],
                                   launch_speed_skew_norm@param$dp[2],launch_speed_skew_norm@param$dp[3]) *
                         la_dist(launch_speed, launch_angle, attack_angle)), aes(launch_speed, launch_angle, fill = d)) +
  scale_fill_gradientn(colours = rainbow(7)) +
  geom_hline(yintercept = 0) + coord_cartesian(expand = F) 

#Visualizing
ggplot() + stat_density(data = batter_all_2019hp %>% filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>% 
                          filter(player_name == player) #%>% 
                        , aes(launch_speed), alpha = .2, colour = 'blue') + 
  geom_line(data = data.frame(launch_speed = 30:120) %>% mutate(density = sn::dsn(launch_speed,launch_speed_skew_norm@param$dp[1],launch_speed_skew_norm@param$dp[2],launch_speed_skew_norm@param$dp[3])), 
            aes(launch_speed, density), size = 1)


