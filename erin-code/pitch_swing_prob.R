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
library(ggtext)
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

# First, denote whether a pitch was swung at, create a variable called swung_at that denotes 
#with a 1 if the pitch was swung at and a 0 if the pitch was not swung at. 
batter_all_2019_lefty<- batter_all_2019 %>%
  filter(stand == "L") %>%
  mutate(plate_x = -1*plate_x)
batter_all_2019_rest <- batter_all_2019 %>%
  filter(stand != "L")
batter_all_2019_reflect <- bind_rows(batter_all_2019_lefty, batter_all_2019_rest)

swing_prob_data <- batter_all_2019_reflect %>%
  mutate(swung_at = case_when(description %in% c("ball", "blocked_ball", "called_strike", "hit_by_pitch") ~ 0, 
                              description %in% c("foul", "foul_tip", "hit_into_play", "swinging_strike", "swinging_strike_blocked") ~ 1, 
                              TRUE ~ 2), 
         swung_at = as.factor(swung_at), 
         pitch_type = case_when(pitch_type %in% c("CH", "EP") ~ "Offspeed", 
                                       pitch_type %in% c("CS", "CU", "KC", "KN", "SC", "SL") ~ "Breaking", 
                                       pitch_type %in% c("FA", "FO", "FS", "FT", "SI", "FC", "FF") ~ "Fastball", 
                                       TRUE ~ pitch_type), 
         in_horizontal_zone = case_when(abs(plate_x) > 11/12 ~ 0, 
                                        TRUE ~ 1), 
         in_horizontal_zone = as.factor(in_horizontal_zone)) %>%
  filter(swung_at %in% c(0, 1)) %>%
  select(swung_at, pitch_type, in_horizontal_zone, everything())

# Visualization that selects 2500 pitches and creates a scatterplot of zone coordinates showing what 
#was swung at and what was taken
sample_n(swing_prob_data, 2500) %>%
  ggplot(aes(x=plate_x, y=plate_z, color=swung_at)) +
  geom_point(alpha=0.7)+
  geom_vline(xintercept = 11/12, linetype = "dashed")+
  geom_vline(xintercept = -11/12, linetype = "dashed")+
  theme_minimal()+
  scale_color_manual(values = c("firebrick", "cadetblue3"))+
  labs(x="Horizontal pitch location (ft)", y="Pitch height (ft)", 
       title = "MLB Hitter Pitch Selection: <strong><span style='color:firebrick'>Takes</span></strong></b> versus <strong><span style='color:cadetblue3'>swings</span></strong></b>", 
       subtitle = "With positive numbers representing outside pitches and negative representing inside pitches")+
  theme(plot.title.position = "plot",
        plot.title = element_markdown(size = 10), 
        axis.title = element_markdown(size=9),
        plot.subtitle = element_markdown(size=8),
        legend.position = "none")

#For pitches swung at outside of the horizontal strike zone what are more often swung at? 
fastballs <- swing_prob_data %>%
  filter(pitch_type == "Fastball") %>%
  group_by(in_horizontal_zone, swung_at) %>%
  count() %>%
  ungroup() %>%
  group_by(in_horizontal_zone) %>%
  mutate(freq = n / sum(n), 
         pitch_type = "Fastball")

offspeeds <- swing_prob_data %>%
  filter(pitch_type == "Offspeed") %>%
  group_by(in_horizontal_zone, swung_at) %>%
  count() %>%
  ungroup() %>%
  group_by(in_horizontal_zone) %>%
  mutate(freq = n / sum(n), 
         pitch_type = "Offspeed")

breakings <- swing_prob_data %>%
  filter(pitch_type == "Breaking") %>%
  group_by(in_horizontal_zone, swung_at) %>%
  count() %>%
  ungroup() %>%
  group_by(in_horizontal_zone) %>%
  mutate(freq = n / sum(n), 
         pitch_type = "Breaking")

pitch_selection <- bind_rows(fastballs, offspeeds, breakings)
pitch_selection %>%
  filter(in_horizontal_zone == 0) %>%
  ggplot(aes(x=pitch_type, y=freq, fill=swung_at))+
  geom_col(position = "fill")+
  theme_minimal()+
  labs(x = "", y="",
       title = "Offspeed pitches outside the horizontal strike zone are more often <strong><span style='color:cadetblue3'>swung at</span></strong></b> than fastballs and breaking balls")+
  scale_y_discrete(expand = c(0,0))+
  scale_fill_manual(values = c("firebrick", "cadetblue3"))+
  theme(plot.title = element_markdown(size=8), 
        legend.position = "none")


  
