# 99_main_scrape_RT

# load packages #####
require(dplyr)
require(lubridate)
require(DBI)
require(rvest)

# connect DB #####
source("00_connect_DB_RT.R")

# update sitemaps:
source("00_sitemap_updater.R")
updated_pages <- scrape_base_sitemaps()
page_list <- scrape_sitemaps()
#just needed to check, loaded from DB later (less data heavy)
#rm(page_list, updated_pages)

# load scraping functions #####
source("01_scrape_pages_rt.R")

# scrape #####

scrape_filter <- c("de")  # select which scrapers to use (mainly for initial scrape or preferential update)

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

scrape_pages(pages_list = updated_pages, sleeptime = 0, log_file = "ignore/scrape_log.txt")
