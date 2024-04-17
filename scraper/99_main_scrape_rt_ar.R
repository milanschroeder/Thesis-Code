# 99_main_scrape_rt.R

# load packages #####
library(dplyr)
library(lubridate)
library(DBI)
library(rvest)

# connect DB #####
source("00_connect_DB_RT.R")

# load scraping functions #####
source("01_scrape_pages_rt.R")

# scrape #####

scrape_filter <- c("ar")  # select which scrapers to use (mainly for initial scrape)

already_scraped <-
  tbl(conn, "page_data") %>% 
  filter(version %in% scrape_filter 
         && available_online == 1) %>% 
  select(link) %>% 
  collect()

RT_DB_pointer <- dplyr::tbl(conn, "page_list")

updated_pages <- 
  RT_DB_pointer %>% 
  filter(version %in% scrape_filter
  ) %>% 
  collect() %>% 
  filter(!loc %in% already_scraped$link) %>% 
  slice_sample(prop = 1)

scrape_pages(pages_list = updated_pages, sleeptime = 0, log_file = "scrapelog.txt")
