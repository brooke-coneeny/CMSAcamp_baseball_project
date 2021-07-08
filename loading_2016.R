library(baseballr)
library(tidyverse)

# 2016 data ---------------------------------------------------------------

april1 <- scrape_statcast_savant_batter_all(start_date = "2016-04-03",
                                            end_date = "2016-04-10")
april2 <- scrape_statcast_savant_batter_all(start_date = "2016-04-11",
                                            end_date = "2016-04-17")
april3 <- scrape_statcast_savant_batter_all(start_date = "2016-04-18",
                                            end_date = "2016-04-24")
april4 <- scrape_statcast_savant_batter_all(start_date = "2016-04-25",
                                            end_date = "2016-04-30")
may1 <- scrape_statcast_savant_batter_all(start_date = "2016-05-01",
                                          end_date = "2016-05-07")
may2 <- scrape_statcast_savant_batter_all(start_date = "2016-05-08",
                                          end_date = "2016-05-14")
may3 <- scrape_statcast_savant_batter_all(start_date = "2016-05-15",
                                          end_date = "2016-05-22")
may4 <- scrape_statcast_savant_batter_all(start_date = "2016-05-23",
                                          end_date = "2016-05-31")
june1 <- scrape_statcast_savant_batter_all(start_date = "2016-06-01",
                                           end_date = "2016-06-07")
june2 <- scrape_statcast_savant_batter_all(start_date = "2016-06-08",
                                           end_date = "2016-06-14")
june3 <- scrape_statcast_savant_batter_all(start_date = "2016-06-15",
                                           end_date = "2016-06-22")
june4 <- scrape_statcast_savant_batter_all(start_date = "2016-06-23",
                                           end_date = "2016-06-30")
july1 <- scrape_statcast_savant_batter_all(start_date = "2016-07-01",
                                           end_date = "2016-07-07")
july2 <- scrape_statcast_savant_batter_all(start_date = "2016-07-08",
                                           end_date = "2016-07-14")
july3 <- scrape_statcast_savant_batter_all(start_date = "2016-07-15",
                                           end_date = "2016-07-22")
july4 <- scrape_statcast_savant_batter_all(start_date = "2016-07-23",
                                           end_date = "2016-07-31")
aug1 <- scrape_statcast_savant_batter_all(start_date = "2016-08-01",
                                          end_date = "2016-08-07")
aug2 <- scrape_statcast_savant_batter_all(start_date = "2016-08-08",
                                          end_date = "2016-08-14")
aug3 <- scrape_statcast_savant_batter_all(start_date = "2016-08-15",
                                          end_date = "2016-08-22")
aug4 <- scrape_statcast_savant_batter_all(start_date = "2016-08-23",
                                          end_date = "2016-08-31")
sep1 <- scrape_statcast_savant_batter_all(start_date = "2016-09-01",
                                          end_date = "2016-09-07")
sep2 <- scrape_statcast_savant_batter_all(start_date = "2016-09-08",
                                          end_date = "2016-09-14")
sep3 <- scrape_statcast_savant_batter_all(start_date = "2016-09-15",
                                          end_date = "2016-09-23")
sep4 <- scrape_statcast_savant_batter_all(start_date = "2016-09-24",
                                          end_date = "2016-09-30")
oct1 <- scrape_statcast_savant_batter_all(start_date = "2016-10-01",
                                          end_date = "2016-10-02")

batter_all_2016 <- rbind(april1, april2, april3, april4, may1, may2, may3, may4, june1, june2,
                         june3, june4, july1, july2, july3, july4, aug1, aug2, aug3, aug4, sep1, sep2, 
                         sep3, sep4, oct1)

# Write 2016 data to rds --------------------------------------------------

write_rds(batter_all_2016, "private_data/all2016data.rds")
