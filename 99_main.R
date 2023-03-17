# main:

# ToDo: append updated cases from sitemaps to page_list, then delete sitemaps 
#...

source("00_sitemap_updater.R")

# run base_sitemap scraper: ####
updated_sitemaps <- scrape_base_sitemaps()

# run page_list scraper: ####
# usually: 
updated_pages <- scrape_sitemaps()

# initial scrape: ####
# scrape_sitemaps(sitemaps_source = "all", sleeptime = 3)

# Scrape articles #####

source("01_scrape_pages.R")

pacman::p_load(tidyverse, DBI, RSQLite)
RT_DB <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = "ignore/Russian_Media_Database_RT.sqlite") 

# ... imagine some ifnotexist-function...
# initialize_page_tbls()


# define list of pages to scrape (better not all together) # not needed as long as only for defined functions
updated_pages <- dplyr::tbl(RT_DB, "page_list") %>% 
  filter(version %in% c("bk", "bk-lat")) %>% 
  collect()

test <- filter(updated_pages, loc == "https://rt.rs/news/57-pepa-prase-i-dve-mame/")

scrape_pages(test)
scrape_pages(pages_list = updated_pages, sleeptime = 1.5)


# create cetagory & tag dictionary:
non_article_pages_pointer <- dplyr::tbl(RT_DB, "page_list") 
non_article_pages <- non_article_pages_pointer %>% 
  filter() %>%                                              # ToDo!
  collect()

category_dict <- tibble(
  category_name = ..., # from sitemaps
  category_full_link = ..., # from sitemaps
  version = version)

tag_dict <- tibble(
  tag_name = ..., # from sitemaps
  tag_full_link = ..., # from sitemaps
  version = version)

