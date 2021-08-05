library(baseballr)
library(tidyverse)


# 2018 data ---------------------------------------------------------------

march_18 <- scrape_statcast_savant_batter_all(start_date = "2018-03-29",
                                           end_date = "2018-03-31")
april1_18 <- scrape_statcast_savant_batter_all(start_date = "2018-04-01",
                                            end_date = "2018-04-07")
april2_18 <- scrape_statcast_savant_batter_all(start_date = "2018-04-08",
                                            end_date = "2018-04-14")
april3_18 <- scrape_statcast_savant_batter_all(start_date = "2018-04-15",
                                            end_date = "2018-04-22")
april4_18 <- scrape_statcast_savant_batter_all(start_date = "2018-04-23",
                                            end_date = "2018-04-30")
may1_18 <- scrape_statcast_savant_batter_all(start_date = "2018-05-01",
                                          end_date = "2018-05-07")
may2_18 <- scrape_statcast_savant_batter_all(start_date = "2018-05-08",
                                          end_date = "2018-05-14")
may3_18 <- scrape_statcast_savant_batter_all(start_date = "2018-05-15",
                                          end_date = "2018-05-22")
may4_18 <- scrape_statcast_savant_batter_all(start_date = "2018-05-23",
                                          end_date = "2018-05-31")
june1_18 <- scrape_statcast_savant_batter_all(start_date = "2018-06-01",
                                           end_date = "2018-06-07")
june2_18 <- scrape_statcast_savant_batter_all(start_date = "2018-06-08",
                                           end_date = "2018-06-14")
june3_18 <- scrape_statcast_savant_batter_all(start_date = "2018-06-15",
                                           end_date = "2018-06-22")
june4_18 <- scrape_statcast_savant_batter_all(start_date = "2018-06-23",
                                           end_date = "2018-06-30")
july1_18 <- scrape_statcast_savant_batter_all(start_date = "2018-07-01",
                                           end_date = "2018-07-07")
july2_18 <- scrape_statcast_savant_batter_all(start_date = "2018-07-08",
                                           end_date = "2018-07-14")
july3_18 <- scrape_statcast_savant_batter_all(start_date = "2018-07-15",
                                           end_date = "2018-07-22")
july4_18 <- scrape_statcast_savant_batter_all(start_date = "2018-07-23",
                                           end_date = "2018-07-31")
aug1_18 <- scrape_statcast_savant_batter_all(start_date = "2018-08-01",
                                          end_date = "2018-08-07")
aug2_18 <- scrape_statcast_savant_batter_all(start_date = "2018-08-08",
                                          end_date = "2018-08-14")
aug3_18 <- scrape_statcast_savant_batter_all(start_date = "2018-08-15",
                                          end_date = "2018-08-22")
aug4_18 <- scrape_statcast_savant_batter_all(start_date = "2018-08-23",
                                          end_date = "2018-08-31")
sep1_18 <- scrape_statcast_savant_batter_all(start_date = "2018-09-01",
                                          end_date = "2018-09-07")
sep2_18 <- scrape_statcast_savant_batter_all(start_date = "2018-09-08",
                                          end_date = "2018-09-14")
sep3_18 <- scrape_statcast_savant_batter_all(start_date = "2018-09-15",
                                          end_date = "2018-09-23")
sep4_18_and_oct <- scrape_statcast_savant_batter_all(start_date = "2018-09-23",
                                             end_date = "2018-10-01")

batter_all_2018 <- rbind(march_18, april1_18, april2_18, april3_18, april4_18, may1_18, may2_18, may3_18,
                         may4_18, june1_18, june2_18, june3_18, june4_18, july1_18, july2_18, july3_18,
                         july4_18, aug1_18, aug2_18, aug3_18, aug4_18, sep1_18, sep2_18, sep3_18, 
                         sep4_18_and_oct)

write_rds(batter_all_2018, "private_data/all2018data.rds")


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

write_rds(batter_all_2019, "private_data/all2019data.rds")

#read in this from baseball savant to get the season wOBA value instead of calculating it
#from: https://baseballsavant.mlb.com/leaderboard/expected_statistics?type=batter&year=2019&position=&team=&min=1
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

write_rds(batter_all_2021, "private_data/all2021data.rds")
