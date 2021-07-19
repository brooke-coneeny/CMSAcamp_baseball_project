#2021 batted balls

#2021 data ----------------------------------------------------------------------------------------------
library(baseballr)
library(tidyverse)

april_week1 <- scrape_statcast_savant_batter_all(start_date = "2021-04-01",
                                               end_date = "2021-04-07")
april_week2 <- scrape_statcast_savant_batter_all(start_date = "2021-04-08",
                                               end_date = "2021-04-14")
april_week3 <- scrape_statcast_savant_batter_all(start_date = "2021-04-15",
                                               end_date = "2021-04-22")
april_week4 <- scrape_statcast_savant_batter_all(start_date = "2021-04-23",
                                               end_date = "2021-04-30")
may_week1 <- scrape_statcast_savant_batter_all(start_date = "2021-05-01",
                                               end_date = "2021-05-07")
may_week2 <- scrape_statcast_savant_batter_all(start_date = "2021-05-08",
                                               end_date = "2021-05-14")
may_week3 <- scrape_statcast_savant_batter_all(start_date = "2021-05-15",
                                               end_date = "2021-05-22")
may_week4 <- scrape_statcast_savant_batter_all(start_date = "2021-05-23",
                                               end_date = "2021-05-31")
june_week1 <- scrape_statcast_savant_batter_all(start_date = "2021-06-01",
                                                 end_date = "2021-06-07")
june_week2 <- scrape_statcast_savant_batter_all(start_date = "2021-06-08",
                                                 end_date = "2021-06-14")
june_week3 <- scrape_statcast_savant_batter_all(start_date = "2021-06-15",
                                                 end_date = "2021-06-22")
june_week4 <- scrape_statcast_savant_batter_all(start_date = "2021-06-23",
                                                 end_date = "2021-06-30")
july_week1 <- scrape_statcast_savant_batter_all(start_date = "2021-07-01",
                                                 end_date = "2021-07-06")
july_week2 <- scrape_statcast_savant_batter_all(start_date = "2021-07-07",
                                                end_date = "2021-07-14")
july_week3 <- scrape_statcast_savant_batter_all(start_date = "2021-07-15",
                                                end_date = "2021-07-18")


batter_all_2021 <- bind_rows(april_week1, april_week2, april_week3, april_week4, 
                     may_week2, may_week3, may_week4, june_week1, june_week2,
                     june_week3, june_week4, july_week1, july_week2, july_week3)

# Write 2021 data to rds --------------------------------------------------

write_rds(batter_all_2021, "private_data/all2021data.rds")
