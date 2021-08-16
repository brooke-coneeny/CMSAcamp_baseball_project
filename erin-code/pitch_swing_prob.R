# This file intends to begin to explore how the probability of a batter swinging at a
#pitch changes by location. 
#Brooke Coneeny, Sarah Sult, and Erin Franke 
#CMSAcamp 2021
####################################################################################################################################

#Loading Libraries
library(tidyverse)
library(plotly)
library(broom)
library(mgcv)
'%!in%' <- Negate('%in%')

#Loading Data
batter_all_2016 <- read_rds("private_data/all2016data.rds")
batter_all_2017 <- read_rds("private_data/all2017data.rds")
batter_all_2018 <- read_rds("private_data/all2018data.rds")
batter_all_2019 <- read_rds("private_data/all2019data.rds")
batter_all_2020 <- read_rds("private_data/all2020data.rds")
batter_all_2021 <- read_rds("private_data/all2021data.rds")
####################################################################################################################################
# EDA TO EXPLORE WHAT PITCHES ARE SWUNG AT 

# First, denote whether a pitch was swung at, create a varaible called swung_at that denotes 
#with a 1 if the pitch was swung at and a 0 if the pitch was not swung at. 
swing_prob_data <- batter_all_2019 %>%
  mutate(swung_at = case_when(description %in% c("ball", "blocked_ball", "called_strike", "hit_by_pitch") ~ 0, 
                              description %in% c("foul", "foul_tip", "hit_into_play", "swinging_strike", "swinging_strike_blocked") ~ 1, 
                              TRUE ~ 2), 
         swung_at = as.factor(swung_at)) %>%
  filter(swung_at %in% c(0, 1)) %>%
  select(swung_at, everything())
swing_prob_data %>%
  ggplot(aes(x=plate_x, y=plate_z, color=swung_at)) +
  geom_point(alpha=0.7)+
  theme_minimal()


  
