library(baseballr)
library(Lahman)
library(tidyverse)

may_week1 <- scrape_statcast_savant_batter_all(start_date = "2021-05-01",
                                           end_date = "2021-05-07")
may_week2 <- scrape_statcast_savant_batter_all(start_date = "2021-05-08",
                                               end_date = "2021-05-14")
may_week3 <- scrape_statcast_savant_batter_all(start_date = "2021-05-15",
                                               end_date = "2021-05-22")
may_week4 <- scrape_statcast_savant_batter_all(start_date = "2021-05-23",
                                               end_date = "2021-05-31")

may2021 <- bind_rows(may_week1, may_week2, may_week3, may_week4)

batter_all_2021 <- bind_rows(april, may2021, june_2021)

#wOBA vs. launch angle by exit velos -------------------------------------------------------------------

#need to find each hitters max EV and the corresponding launch angle and their wOBA for the season and 
#each data point is a person

# batter_all_2021 %>%
#   filter(description == "hit_into_play", !is.na(events), launch_angle %in% c(-40:60))%>%
#   mutate(events_group = case_when(
#     events %in% c("field_out", "other_out", "grounded_into_double_play", "double_play", 
#                   "fielders_choice_out", "force_out", "sac_bunt", "sac_fly_double_play", 
#                   "sac_bunt_double_play", "field_error", "sac_fly", "fielders_choice", "triple_play") ~ "out",
#     TRUE ~ "hit"
#   ), wOBA = case_when(
#     events == "single" ~ .882,
#     events == "double" ~ 1.242,
#     events == "triple" ~ 1.586,
#     events == "home_run" ~ 2.039,
#     events_group == "out" ~ 0
#   ), launch_speed_group = case_when(
#     launch_speed < 106 ~ "low",
#     launch_speed >= 106 & launch_speed < 112 ~ "medium",
#     launch_speed >= 112 ~ "high"
#   )
#   )%>%
#   dplyr::select(wOBA, launch_speed, launch_angle, launch_speed_group)%>%
#   ggplot(aes(x=launch_angle, y=wOBA, color = launch_speed_group))+
#   geom_smooth(method="loess", se=FALSE)+
#   theme_bw()

#gives us totals for all variables but singles, could calculate singles though???????????
#how do you merge players on different teams? the playerID is confusing???????????
batting_Lahman <- Lahman::Batting
batting_Lahman_2019 <- batting_Lahman %>%     #doens't have 2021 data yet! also doesnt count singles?!?!
  filter(yearID == 2019)

max_exit_velos <- batter_all_2021 %>%
#this gets us each player's max EV and the associated angle
  group_by(player_name) %>%
  summarize(max_EV = max(launch_speed, na.rm = TRUE)) %>%
  filter(max_EV < 130 & max_EV>0) %>%
  left_join(batter_all_2021, by = c("player_name", "max_EV" = "launch_speed")) %>%
  select(player_name, max_EV, launch_angle) %>%
  #still need to somehow calculate wOBA??????????
  #mutate(wOBA = woba_plus())
  mutate(EV_group = cut(max_EV, breaks = c(0, 105.99, 111.99, 150), labels = c("<106", "106+", "112+"))) %>%
  #ggplot(aes(x=launch_angle, y=wOBA, color = velo_group))+
  #geom_smooth(method="loess", se=FALSE)+
  #theme_bw()
  
  


