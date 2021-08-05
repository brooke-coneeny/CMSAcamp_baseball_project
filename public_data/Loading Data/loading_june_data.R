library(baseballr)
library(dplyr)
library(tidyverse)

#baseballr package info: http://billpetti.github.io/baseballr/
#variable meanings: https://app.box.com/v/statcast-pitchfx-glossary-pett


#Loading in the data from the month of June 

june_week01 <- scrape_statcast_savant_batter_all(start_date = "2021-06-01", 
                                                 end_date = "2021-06-07")
june_week02 <- scrape_statcast_savant_batter_all(start_date = "2021-06-08", 
                                                 end_date = "2021-06-14")
june_week03 <- scrape_statcast_savant_batter_all(start_date = "2021-06-15", 
                                                 end_date = "2021-06-21")
june_week04 <- scrape_statcast_savant_batter_all(start_date = "2021-06-22", 
                                                 end_date = "2021-06-23")

june_2021 <- bind_rows(june_week01, june_week02, june_week03, june_week04)
View(june_2021)

  