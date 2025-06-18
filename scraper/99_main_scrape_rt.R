# 99_main_scrape_rt.R

# load packages #####
require(dplyr)
require(lubridate)
require(DBI)
require(rvest)

# connect DB #####
source("../00_connect_DB_RT.R")



# load sitemap scraper ####
source("00_sitemap_updater_rt.R")

# update_sitemaps ####
updated_sitemaps <- scrape_base_sitemaps()
updated_sitemaps # inspect
# scrape sitemaps ####
updated_pages <- scrape_sitemaps()
try(updated_pages %>% dplyr::count(version)) # inspect

rm(updated_pages) # faster/lighter to load filtered from DB

# load content scraping functions #####
source("01_scrape_pages_rt.R")


# scrape #####

scrape_filter <- c("ru")  # select which scrapers to use (mainly for initial scrape)

already_scraped <-
  tbl(conn, "page_data") %>% 
  filter(version %in% scrape_filter 
         #date > as.Date("2023-09-30") && date < as.Date("2024-03-01") 
         & available_online == 1) %>% 
  select(link) %>% 
  collect()


updated_pages <- 
  dplyr::tbl(conn, "page_list") %>% 
  filter(version %in% scrape_filter 
    #& date > as.Date("2023-09-30") && date < as.Date("2024-03-01")
    ) %>% 
  collect() %>% 
  filter(!loc %in% already_scraped$link) %>% 
  slice_sample(prop = 1)

scrape_pages(pages_list = updated_pages, sleeptime = 0, log_file = "scrapelog_rt.txt")

DBI::dbDisconnect(conn)
