# 02 crawl sitemap_index for base_sitemaps

# should run directly on Server

# connect DB & sitemap index if necessary ####
source("create_sitemap_index_newsfront.R")

library(DBI)
library(rvest)
library(lubridate)

# loop over sitemaps: ####

lastcrawl = ifelse("base_sitemaps" %in% DBI::dbListTables(conn),
                    (tbl(conn, "base_sitemaps") %>% dplyr::arrange(desc(last_crawl)) %>% head(1) %>% collect())$last_crawl %>% lubridate::ymd_hms(., tz = "UTC"),
                    lubridate::ymd_hms("1900-01-01 00:00:01", tz = "UTC")
                    ) %>% as_datetime()

base_sitemaps <- tibble()

for(index in 1:nrow(sitemap_index)) {
  
  pointer <- rvest::read_html(sitemap_index$loc[index])
  
  # Crawl each sitemap_index
  base_sitemaps_crawl <- tibble(loc = pointer %>% html_elements("sitemap loc") %>% html_text2(),
                                 lastmod = pointer %>% html_elements("sitemap lastmod") %>% html_text2() %>% lubridate::ymd_hms(., tz = "UTC"),
                                 index_sitemap = sitemap_index$loc[index],
                                 version = sitemap_index$version[index],
                                 last_crawl = Sys.time() %>% lubridate::ymd_hms(., tz = "UTC")
                                )
  
  # add new rows for each version
  base_sitemaps <- base_sitemaps %>% bind_rows(., base_sitemaps_crawl) 
  
  print(paste(index, "/", nrow(sitemap_index), "-", sitemap_index$loc[index], "saved."))
}

# filter non-updated sitemaps: ####
  
new_sitemaps <- dplyr::filter(base_sitemaps, lastmod > lastcrawl)

#  append table in db: ####
  
  DBI::dbWriteTable(conn = conn, name = "base_sitemaps", 
                    value = new_sitemaps %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                    append = TRUE
)

