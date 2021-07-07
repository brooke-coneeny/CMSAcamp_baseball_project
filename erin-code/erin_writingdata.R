#write 2020 data 

july20 <- scrape_statcast_savant_batter_all(start_date = "2020-07-23",
                                            end_date = "2020-07-31")
aug201 <- scrape_statcast_savant_batter_all(start_date = "2020-08-01",
                                            end_date = "2020-08-07")
aug202 <- scrape_statcast_savant_batter_all(start_date = "2020-08-08",
                                            end_date = "2020-08-14")
aug203 <- scrape_statcast_savant_batter_all(start_date = "2020-08-15",
                                            end_date = "2020-08-22")
aug204 <- scrape_statcast_savant_batter_all(start_date = "2020-08-23",
                                            end_date = "2020-08-31")
sep201 <- scrape_statcast_savant_batter_all(start_date = "2020-09-01",
                                            end_date = "2020-09-07")
sep202 <- scrape_statcast_savant_batter_all(start_date = "2020-09-08",
                                            end_date = "2020-09-14")
sep203 <- scrape_statcast_savant_batter_all(start_date = "2020-09-15",
                                            end_date = "2020-09-23")
sep204 <- scrape_statcast_savant_batter_all(start_date = "2020-09-24",
                                            end_date = "2020-09-30")
batter_all_2020 <- rbind(july20, aug201, aug202, aug203, aug204, sep201, sep202, 
                         sep203, sep204)

write_rds(batter_all_2020, file ="private_data/all2020data.rds")