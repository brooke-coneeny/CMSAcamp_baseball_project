library(tidyverse)
library(sn)


# Data --------------------------------------------------------------------

batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2019hp <- batter_all_2019 %>%
  filter(description == "hit_into_play")
batter_all_2020 <- read_rds("private_data/all2020data.rds")
batter_all_2021 <- read_rds("private_data/all2021data.rds")

batter_all_1921 <- bind_rows(batter_all_2019, batter_all_2020, batter_all_2021)
batter_all_1921hp <- batter_all_1921 %>%
  filter(description == "hit_into_play")

# Parametric form for adjusting LA based on LA/EV distribution ----------------------------

var_range <- expand.grid(launch_speed = 30:125,
                 launch_angle = -75:75)

player = 'Trout, Mike'

# Gets rid of what we assume to be bad measurements
batter_all_2019hp_good <- batter_all_2019hp %>% 
  filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>% 
  filter(player_name == player)

# Creating a skew normal dist of launch speed
launch_speed_skew_norm <- sn::selm(launch_speed ~ 1, data = batter_all_2019hp %>% 
                                     filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>% 
                                     filter(player_name == player), method = "MPLE")

# Calculates the presumed attack angle of the batter (median angle of the top 10% hardest hit balls)
attack_angle <- median((batter_all_2019hp %>% 
                          filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>% 
                          filter(player_name == player) %>% 
                          filter(launch_speed >= quantile(launch_speed, .9)))$launch_angle)

# Conditional distribution of LA given EV and attack angle 
la_dist <- function(ev, la, attack, n = 100000) {
  w <- sn::psn(ev, launch_speed_skew_norm@param$dp[1],launch_speed_skew_norm@param$dp[2],
               launch_speed_skew_norm@param$dp[3])^2
  w = (exp(w)/(100 + exp(w)))^2.2
  #(la+75)/150 puts all launch angles between 0 and 1
  dbeta((la + 75)/150, 1 + w * n * (attack + 75)/150, 1 + w * n - w * (n * (attack + 75)/150))
}

ggplot() + 
  geom_raster(data = var_range %>% 
                mutate(d = sn::dsn(launch_speed,launch_speed_skew_norm@param$dp[1],
                                   launch_speed_skew_norm@param$dp[2],launch_speed_skew_norm@param$dp[3]) *
                         la_dist(launch_speed, launch_angle, attack_angle)), 
              #d is p(ev, la) ~ skew_normal(ev, empricially det.) * beta(la between -75 and +75, ad hockery to decay with lower EVs, and mode determined by attack angle)
              aes(launch_speed, launch_angle, fill = d)) +
  scale_fill_gradientn(colours = rainbow(7)) +
  geom_hline(yintercept = 0) + coord_cartesian(expand = F) #+
  #geom_point(data = batter_all_2019hp_good, aes(x = launch_speed, y = launch_angle)) +
  #stat_density_2d(data = batter_all_2019hp_good, aes(x = launch_speed, y = launch_angle), colour = 'black')

# Gets the probabilities Gallo hits any combination of LA/EV and turns it into a mock data set
probs_Gallo <- var_range %>% 
  mutate(d = sn::dsn(launch_speed,launch_speed_skew_norm@param$dp[1],
                     launch_speed_skew_norm@param$dp[2],launch_speed_skew_norm@param$dp[3]) *
           la_dist(launch_speed, launch_angle, attack_angle),
         d_div_sum = d/sum(d),
         num_hits = round(d_div_sum*100000, 0))

mock_Gallo <- probs_Gallo %>% filter (num_hits>=1) %>% select(launch_angle, launch_speed, num_hits) %>% uncount(num_hits)
# Sample based on those probabilities rather than that multiplication
set.seed(2021)
Gallo_sample <- sample(1:nrow(probs_Gallo), 300, replace = TRUE, prob = probs_Gallo$d_div_sum)
mock_Gallo1 <- probs_Gallo[Gallo_sample,] %>% select(launch_angle, launch_speed)

#first way
preds_Gallo <- tibble(gam.preds = predict(final_woba_model2, newdata = mock_Gallo))  
wOBA_Gallo <- mean(preds_Gallo$gam.preds, na.rm = TRUE)  
#sampling
preds_Gallo1 <- tibble(gam.preds = predict(final_woba_model2, newdata = mock_Gallo1))  
wOBA_Gallo1 <- mean(preds_Gallo1$gam.preds, na.rm = TRUE)
#actual 2019 value
mean((batter_all_2019hp %>% filter(player_name == player))$woba_value)

# Gets the probabilities Trout hits any combination of LA/EV and turns it into a mock data set
probs_Trout <- var_range %>% 
  mutate(d = sn::dsn(launch_speed,launch_speed_skew_norm@param$dp[1],
                     launch_speed_skew_norm@param$dp[2],launch_speed_skew_norm@param$dp[3]) *
           la_dist(launch_speed, launch_angle, attack_angle),
         d_div_sum = d/sum(d),
         num_hits = round(d_div_sum*100000, 0))

#somehow need to replicate the combinations the number of times of num_hits aka if one combo has 
#num_hits=78... there better be 78 of that LA/EV combo in the mock data set...
mock_Trout <- probs_Trout %>% filter (num_hits>=1) %>% select(launch_angle, launch_speed, num_hits) %>% uncount(num_hits)


preds_Trout <- tibble(gam.preds = predict(final_woba_model2, newdata = mock_Trout))  
wOBA_Trout <- mean(preds_Trout$gam.preds, na.rm = TRUE)  
mean((batter_all_2019hp %>% filter(player_name == player))$woba_value)

# Final Model -------------------------------------------------------------

final_woba_model2 <- gam(woba_value ~ s(launch_angle, launch_speed, k=200), data = batter_all_2019hp, 
                         method = "REML")


# Adjusting attack angle to find best xwOBA -------------------------------

la_dist <- function(ev, la, attack, n = 100000) {
  w <- sn::psn(ev, 
               launch_speed_skew_norm@param$dp[1],launch_speed_skew_norm@param$dp[2],  
               launch_speed_skew_norm@param$dp[3])^2
  w = (exp(w)/(100 + exp(w)))^2.2
  dbeta((la + 75)/150, 1 + w * n * (attack + 75)/150, 1 + w * n - w * (n * (attack + 75)/150))
}

player = "Gallo, Joey"

attack_angle <- median((batter_all_2019hp %>% 
                                   filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>% 
                                   filter(player_name == player) %>% 
                                   filter(launch_speed >= quantile(launch_speed, .9)))$launch_angle)

adjust_attack <- function(model, all_data, attack){
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
           d_div_sum = d/sum(d),
           num_hits = round(d_div_sum*100000, 0))
  
  #Create mock dataset 
  mock1 <- probs1 %>% filter (num_hits>=1) %>% select(launch_angle, launch_speed, 
                                                    num_hits) %>% uncount(num_hits)
  
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
           d_div_sum = d/sum(d),
           num_hits = round(d_div_sum*100000, 0))
  
  #Create mock dataset 
  mock2 <- probs2 %>% filter (num_hits>=1) %>% select(launch_angle, launch_speed, 
                                                     num_hits) %>% uncount(num_hits)
  
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
           d_div_sum = d/sum(d),
           num_hits = round(d_div_sum*100000, 0))
  
  #Create mock dataset 
  mock3 <- probs3 %>% filter (num_hits>=1) %>% select(launch_angle, launch_speed, 
                                                     num_hits) %>% uncount(num_hits)
  
  #Predicted woba values 
  preds3 <- tibble(gam.preds = predict(model, newdata = mock3))  
  
  #Predicted average woba value 
  xwOBA3 = mean(preds3$gam.preds)
  
  #+1 attack better
  if(xwOBA1 < xwOBA2){
    adjust_attack(model, mock2, attack+1)
  }
  #-1 attack better
  else if(xwOBA1 < xwOBA3){
    adjust_attack(model, mock3, attack-1)
  }
  #we found the best
  else{
    return(tibble(best_attack = attack, xwOBA = xwOBA1))
  }

}

gallo_data <- batter_all_2019hp %>% filter (player_name == "Gallo, Joey")
adjust_attack(final_woba_model2, gallo_data, attack_angle)

hist(gallo_data$launch_angle)
