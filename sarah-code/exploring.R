library(baseballr)
library(tidyverse)

#baseballr package info: http://billpetti.github.io/baseballr/
#variable meanings: https://app.box.com/v/statcast-pitchfx-glossary-pett

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

# reading in 2019 data without scraping -----------------------------------------------------
#try reading in rds data
all_2019_pitches <- read_rds("data/all2019data.rds")

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

wOBA_angle_velo_graph <- batter_all_2019 %>%
#this gets us each player's max EV and the associated angle
  group_by(player_name) %>%
  summarize(max_EV = max(launch_speed, na.rm = TRUE)) %>%
  filter(max_EV < 130 & max_EV>0) %>%
  left_join(batter_all_2019, by = c("player_name", "max_EV" = "launch_speed")) %>%
#this brings in expected stats data
  left_join(expected_stats_2019, by = c("player_name" = "name")) %>%
  select(player_name, max_EV, launch_angle, woba) %>%
  filter(!is.na(woba)) %>%
#then we prepare the graph
  mutate(EV_group = cut(max_EV, breaks = c(0, 105.99, 111.99, 150), labels = c("<106", "106+", "112+"))) %>%
  ggplot(aes(x=launch_angle, y = woba, color = EV_group)) +
  geom_smooth(se = FALSE)+
  #geom_line()+
  scale_x_continuous(limits = c(-40, 60))+
  scale_y_continuous(limits = c(0, 1))+
  theme_bw()+
  labs(x= "Launch Angle",
       y = "wOBA")
  
  

# Do righties vs. lefties have a distinct ev/angle combo?  --------------------------------------------

batter_all_2019 %>%
  group_by(player_name) %>%
  summarize(max_EV = max(launch_speed, na.rm = TRUE)) %>%
  filter(max_EV < 130 & max_EV>0) %>%
  left_join(batter_all_2019, by = c("player_name", "max_EV" = "launch_speed")) %>%
  select(player_name, max_EV, launch_angle, stand)%>%
  ggplot(aes(x=launch_angle, y=max_EV, color = stand))+
  geom_point(alpha = .4)+
  theme_bw()+
  labs(x = "Launch Angle",
       y = "Max Exit Velocity")


# where the max ev hits landed -----------------------------------------------------------------------

batter_all_2019 %>%
  group_by(player_name) %>%
  summarize(max_EV = max(launch_speed, na.rm = TRUE)) %>%
  filter(max_EV < 130 & max_EV>0) %>%
  left_join(batter_all_2019, by = c("player_name", "max_EV" = "launch_speed"))%>%
  select(player_name, hc_x, hc_y, stand, events)%>%
  filter(!is.na(stand), !is.na(events))%>%
  mutate(events = case_when(
#for the purposes of this, field error and fielders choice are outs because in normal play that's what 
    #they would have been
    events %in% c("double_play", "field_out", "grounded_into_double_play", "sac_bunt", 
                  "sac_fly", "field_error", "fielders_choice") ~ "out",
    events %in% c("single", "force_out") ~ "single",
    events == "double" ~ "double",
    events == "home_run" ~ "home_run",
    events == "triple" ~ "triple"
  ))%>%
#coloring by event might allow us to see how the shift affects lefties
  ggplot(mapping = aes(x = hc_x, y = hc_y, color = events)) +
  geom_point() +
  facet_wrap(~stand) +
  scale_y_reverse()
