#erin's folder test

#baseballr package info: http://billpetti.github.io/baseballr/
#variable meanings: https://app.box.com/v/statcast-pitchfx-glossary-pett

#load packages
library(baseballr)
library(data.table)
library(tidyverse)

april_week1 <- scrape_statcast_savant_batter_all(
  start_date = "2021-04-01", 
  end_date = "2021-04-07")
april_week2 <- scrape_statcast_savant_batter_all(
  start_date = "2021-04-08", 
  end_date = "2021-04-14")
april_week3 <- scrape_statcast_savant_batter_all(
  start_date = "2021-04-15", 
  end_date = "2021-04-21")
april_week4 <- scrape_statcast_savant_batter_all(
  start_date = "2021-04-22", 
  end_date = "2021-04-28")
april_2930 <- scrape_statcast_savant_batter_all(
  start_date = "2021-04-29", 
  end_date = "2021-04-30")

april <- bind_rows(april_week1, april_week2, april_week3, april_week4, april_2930)
