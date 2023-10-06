# main:

# ToDo: append updated cases from sitemaps to page_list, then delete sitemaps 
#...

source("00_sitemap_updater.R")

# run base_sitemap scraper: ####
# updated_sitemaps <- scrape_base_sitemaps()

# run page_list scraper: ####
# usually: 
# updated_pages <- scrape_sitemaps()

# initial scrape: ####
# scrape_sitemaps(sitemaps_source = "all", sleeptime = 3)

# Scrape articles #####

source("01_scrape_pages.R")

pacman::p_load(tidyverse, DBI, RSQLite, rvest)

# connect DB #####
source("00_connect_DB_RT.R") # returns conn
RT_DB <- conn # avoid changing old code

# overview of tables in DB:
DBI::dbListTables(RT_DB) 

completed_scrapers <- c("en", "bk", "bk-lat", "es", "de")

RT_DB_pointer <- dplyr::tbl(RT_DB, "page_list") 
already_scraped <- 
  tbl(RT_DB, "page_data") %>% filter(version %in% completed_scrapers 
                                     # && available_online == 1
                                     ) %>% select(link) %>% collect()


# for mySQL: datetime to compare can only be a string, not a datetime variable (which works as well for SQLite)

#en_wartime <- 
all <- 
RT_DB_pointer %>% 
  filter(version %in% completed_scrapers 
#         && lastmod_utc >= "2022-02-24 00:00:00"
         ) %>% 
  collect() %>% filter(!loc %in% already_scraped$link)

# test writing with DBI: sitemap_versions <- RT_DB %>% tbl(., "sitemap_versions") %>% select(-rowid) %>% collect()
# DBI::dbWriteTable(conn = RT_DB, name = "sitemap_versions", value = sitemap_versions, append = T)

all <- all %>% slice_sample(prop = 1)

scrape_pages(pages_list = all, sleeptime = 0)
  


RT_DB_pointer %>% 
  filter(version == "en"
         #         && lastmod_utc >= "2022-02-24 00:00:00"
  ) %>%
  tally() # DB tables are lazy, therefore nrow() doesn't work


df <- dbReadTable(RT_DB, tables[5]) 
  df2 <- dbReadTable(RT_DBnew, tables[5])
identical(df2, df)

# dbWriteTable(RT_DBnew, table[5], df, overwrite = F)

tables <- DBI::dbListTables(RT_DB)
# 4: too big
# 6: malformed
# 7: text_all needs longtext


for (table in tables[8:12]) {
  try({
    df <- dbReadTable(RT_DB, tables[7])
    dbWriteTable(RT_DBnew, tables[7], df, overwrite = T)
    cat(table, " successfully copied! \n")
  })
}


# ... imagine some ifnotexist-function...
# initialize_page_tbls()

links_to_save <- dplyr::tbl(RT_DB, "page_list") %>% filter(version %in% c("bk", "bk-lat")) %>% distinct(loc) %>% collect() %>% select(loc) %>% as_vector()
saved_data <- dplyr::tbl(RT_DB, "page_data") %>% select(link) %>% distinct(link) %>% collect() %>% as_vector()

still_to_scrape <- links_to_save[!(links_to_save %in% saved_data)]

early <- dplyr::tbl(RT_DB, "page_list") %>% filter(loc %in% still_to_scrape) %>% collect()

early2 <- dplyr::tbl(RT_DB, "page_list") %>% 
  filter(version %in% c("bk", "bk-lat")) %>% 
  filter(!(loc %in% new)) %>% 
  collect()

old <- dplyr::tbl(RT_DB, "page_data") %>% collect() %>% filter(as.Date(capture_time) < as.Date("2023-03-19")) %>% select(link) %>% as_vector()
new <- dplyr::tbl(RT_DB, "page_data") %>% collect() %>% filter(as.Date(capture_time) > as.Date("2023-03-19")) %>% select(link) %>% as_vector()

missing <- old[!old %in% new]



# write function to restart...

# define list of pages to scrape (better not all together) # not needed as long as only for defined functions
updated_pages_es <- dplyr::tbl(RT_DB, "page_list") %>% 
  filter(version %in% c("es")) %>% 
  collect()

updated_pages_de <- dplyr::tbl(RT_DB, "page_list") %>% 
  filter(version %in% c("de")) %>% 
  collect()

updated_pages_es %>% rownames_to_column() %>% filter(loc == "https://actualidad.rt.com/videoclub/166679-acrobacias-spiderman-panama")
updated_pages_es <- updated_pages_es[161416:nrow(updated_pages_es), ]

scrape_pages(pages_list = updated_pages_es, sleeptime = .05)


# scrape articles from saved: ####


################### ERROR:
  # *** in database main ***
  #   On tree page 10047256 cell 0: invalid page number 10080797
  # On tree page 10060782 cell 0: invalid page number 10080859
  # On tree page 10080709 cell 4: invalid page number 10080798
  # On tree page 10080709 cell 3: invalid page number 10080766
  # On tree page 10080709 cell 2: invalid page number 10080738
  # On tree page 10080709 cell 1: invalid page number 10080710


# scrape those from saved later:
saved_pages <- dplyr::tbl(RT_DB, "html_pages") %>% select(link) %>% collect() %>% as_vector()

# need to be saved:
non_saved_pages <- updated_pages %>% filter(!loc %in% saved_pages)
scrape_pages(pages_list = non_saved_pages[-c(1:43),], sleeptime = 1.2)



# create cetagory & tag dictionary: ####
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

