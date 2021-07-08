#write 2017 data 

ap171 <- scrape_statcast_savant_batter_all(start_date = "2017-04-02",
                                            end_date = "2017-04-09")
ap172 <- scrape_statcast_savant_batter_all(start_date = "2017-04-10",
                                           end_date = "2017-04-17")
ap173 <- scrape_statcast_savant_batter_all(start_date = "2017-04-18",
                                           end_date = "2017-04-25")
ap174 <- scrape_statcast_savant_batter_all(start_date = "2017-04-26",
                                           end_date = "2017-04-30")
may171 <- scrape_statcast_savant_batter_all(start_date = "2017-05-01",
                                            end_date = "2017-05-08")
may172 <- scrape_statcast_savant_batter_all(start_date = "2017-05-09",
                                            end_date = "2017-05-16")
may173 <- scrape_statcast_savant_batter_all(start_date = "2017-05-17",
                                            end_date = "2017-05-24")
may174 <- scrape_statcast_savant_batter_all(start_date = "2017-05-25",
                                            end_date = "2017-05-31")
jun171 <- scrape_statcast_savant_batter_all(start_date = "2017-06-01",
                                            end_date = "2017-06-08")
jun172 <- scrape_statcast_savant_batter_all(start_date = "2017-06-09",
                                            end_date = "2017-06-16")
jun173 <- scrape_statcast_savant_batter_all(start_date = "2017-06-17",
                                            end_date = "2017-06-24")
jun174 <- scrape_statcast_savant_batter_all(start_date = "2017-06-25",
                                            end_date = "2017-06-30")
jul171 <- scrape_statcast_savant_batter_all(start_date = "2017-07-01",
                                            end_date = "2017-07-08")
jul172 <- scrape_statcast_savant_batter_all(start_date = "2017-07-09",
                                            end_date = "2017-07-16")
jul173 <- scrape_statcast_savant_batter_all(start_date = "2017-07-17",
                                            end_date = "2017-07-24")
jul174 <- scrape_statcast_savant_batter_all(start_date = "2017-07-25",
                                            end_date = "2017-07-31")
aug171 <- scrape_statcast_savant_batter_all(start_date = "2017-08-01",
                                            end_date = "2017-08-08")
aug172 <- scrape_statcast_savant_batter_all(start_date = "2017-08-16",
                                            end_date = "2017-08-17")
aug173 <- scrape_statcast_savant_batter_all(start_date = "2017-08-18",
                                            end_date = "2017-08-24")
aug174 <- scrape_statcast_savant_batter_all(start_date = "2017-08-25",
                                            end_date = "2017-08-31")
sep171 <- scrape_statcast_savant_batter_all(start_date = "2017-09-01",
                                            end_date = "2017-09-08")
sep172 <- scrape_statcast_savant_batter_all(start_date = "2017-09-09",
                                            end_date = "2017-09-16")
sep173 <- scrape_statcast_savant_batter_all(start_date = "2017-09-17",
                                            end_date = "2017-09-24")
sep174 <- scrape_statcast_savant_batter_all(start_date = "2017-09-25",
                                            end_date = "2017-10-01")

batter_all_2017 <- rbind(ap171, ap172, ap173, ap174, may171, may172, may173, may174, 
                         jun171, jun172, jun173, jun174, jul171, jul172, jul173, jul174, 
                         aug171, aug172, aug173, aug174, sep171, sep172, sep173, sep174)

write_rds(batter_all_2017, file ="private_data/all2017data.rds")

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
