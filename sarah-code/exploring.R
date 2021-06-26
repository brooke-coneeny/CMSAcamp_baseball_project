library(baseballr)
library(Lahman)
library(tidyverse)


# 2019 data ---------------------------------------------------------------


march <- scrape_statcast_savant_batter_all(start_date = "2019-03-28",
                                          end_date = "2019-03-31")
april1 <- scrape_statcast_savant_batter_all(start_date = "2019-04-01",
                                            end_date = "2019-04-07")
april2 <- scrape_statcast_savant_batter_all(start_date = "2019-04-08",
                                  end_date = "2019-04-14")
april3 <- scrape_statcast_savant_batter_all(start_date = "2019-04-15",
                                            end_date = "2019-04-22")
april4 <- scrape_statcast_savant_batter_all(start_date = "2019-04-23",
                                            end_date = "2019-04-30")
may1 <- scrape_statcast_savant_batter_all(start_date = "2019-05-01",
                                            end_date = "2019-05-07")
may2 <- scrape_statcast_savant_batter_all(start_date = "2019-05-08",
                                          end_date = "2019-05-14")
may3 <- scrape_statcast_savant_batter_all(start_date = "2019-05-15",
                                          end_date = "2019-05-22")
may4 <- scrape_statcast_savant_batter_all(start_date = "2019-05-23",
                                          end_date = "2019-05-31")
june1 <- scrape_statcast_savant_batter_all(start_date = "2019-06-01",
                                          end_date = "2019-06-07")
june2 <- scrape_statcast_savant_batter_all(start_date = "2019-06-08",
                                           end_date = "2019-06-14")
june3 <- scrape_statcast_savant_batter_all(start_date = "2019-06-15",
                                           end_date = "2019-06-22")
june4 <- scrape_statcast_savant_batter_all(start_date = "2019-06-23",
                                           end_date = "2019-06-30")
july1 <- scrape_statcast_savant_batter_all(start_date = "2019-07-01",
                                           end_date = "2019-07-07")
july2 <- scrape_statcast_savant_batter_all(start_date = "2019-07-08",
                                           end_date = "2019-07-14")
july3 <- scrape_statcast_savant_batter_all(start_date = "2019-07-15",
                                           end_date = "2019-07-22")
july4 <- scrape_statcast_savant_batter_all(start_date = "2019-07-23",
                                           end_date = "2019-07-31")
aug1 <- scrape_statcast_savant_batter_all(start_date = "2019-08-01",
                                           end_date = "2019-08-07")
aug2 <- scrape_statcast_savant_batter_all(start_date = "2019-08-08",
                                          end_date = "2019-08-14")
aug3 <- scrape_statcast_savant_batter_all(start_date = "2019-08-15",
                                          end_date = "2019-08-22")
aug4 <- scrape_statcast_savant_batter_all(start_date = "2019-08-23",
                                          end_date = "2019-08-31")
sep1 <- scrape_statcast_savant_batter_all(start_date = "2019-09-01",
                                          end_date = "2019-09-07")
sep2 <- scrape_statcast_savant_batter_all(start_date = "2019-09-08",
                                          end_date = "2019-09-14")
sep3 <- scrape_statcast_savant_batter_all(start_date = "2019-09-15",
                                          end_date = "2019-09-23")

batter_all_2019 <- rbind(march, april1, april2, april3, april4, may1, may2, may3, may4, june1, june2,
                         june3, june4, july1, july2, july3, july4, aug1, aug2, aug3, aug4, sep1, sep2, 
                         sep3)

#read in this from baseball savant to get the season wOBA value instead of calculating it
expected_stats_2019 <- read_csv("expected_stats2019.csv")

#combine first and last name so you can join
expected_stats_2019 <- expected_stats_2019 %>%
  mutate(name = paste(last_name, first_name, sep = ", "))

#2021 data ----------------------------------------------------------------------------------------------


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

wOBA_angle_velo_graph <- batter_all_2019 %>%
#this gets us each player's max EV and the associated angle
  group_by(player_name) %>%
  summarize(max_EV = max(launch_speed, na.rm = TRUE)) %>%
  filter(max_EV < 130 & max_EV>0) %>%
  left_join(batter_all_2021, by = c("player_name", "max_EV" = "launch_speed")) %>%
#this brings in expected stats data
  left_join(expected_stats_2019, by = c("player_name" = "name")) %>%
  select(player_name, max_EV, launch_angle, woba) %>%
#then we prepare the graph
  mutate(EV_group = cut(max_EV, breaks = c(0, 105.99, 111.99, 150), labels = c("<106", "106+", "112+"))) %>%
  ggplot(aes(x=launch_angle, y = woba, color = EV_group)) +
  geom_smooth(se = FALSE)+
  scale_x_continuous(limits = c(-40, 60))+
  scale_y_continuous(limits = c(0, 0.8))+
  theme_bw()+
  labs(x= "Launch Angle",
       y = "wOBA")
  
  


