# 03 Crawl base_sitemaps for url_list

# connect DB as conn ####
source("00_connect_DB_newsfront.R")

library(DBI)
library(rvest)
library(lubridate)

source("scrape_base_sitemaps_newsfront.R") # get new_sitemaps and update base_sitemaps
# old: new_sitemaps <- DBI::dbReadTable(conn, "base_sitemaps") 

# get new pages: #####
new_sitemaps <- new_sitemaps %>% slice_sample(., prop = 1) # randomly mix to avoid DDoS-Guard

sitemaps <- tibble()

for (index in 1:nrow(new_sitemaps)) {
  
  pointer <- rvest::read_html(new_sitemaps$loc[index])

 if (stringr::str_detect(new_sitemaps$loc[index], "page-sitemap")) {
    
    # some sitemaps need modification:
     if("try-error" %in% class(
   
        sitemaps_crawl <- try(tibble(loc = pointer %>% html_elements(xpath = "//url[not(image)]/loc") %>% html_text2(),
                             lastmod = c(Sys.time() %>% lubridate::ymd_hms(., tz = "UTC"), 
                                         pointer %>% html_elements(xpath = "//url[not(image)]/lastmod") %>% html_text2() %>% lubridate::ymd_hms(., tz = "UTC")
                                         ),
                             base_sitemap = new_sitemaps$loc[index],
                             version = new_sitemaps$version[index],
                             last_crawl = Sys.time() %>% lubridate::ymd_hms(., tz = "UTC")
                      ))
    )){print(paste("Error saving", new_sitemaps$loc[index]))}
  
 } else{
  
  # Crawl each sitemap_index
# category_list df: 
  if("try-error" %in% class(
     
  sitemaps_crawl <- try(tibble(loc = pointer %>% html_elements(xpath = "//url[not(image)]/loc") %>% html_text2(),
                                 lastmod = pointer %>% html_elements(xpath = "//url[not(image)]/loc") %>% html_text2() %>% lubridate::ymd_hms(., tz = "UTC"),
                                 base_sitemap = new_sitemaps$loc[index],
                                 version = new_sitemaps$version[index],
                                 last_crawl = Sys.time() %>% lubridate::ymd_hms(., tz = "UTC")
  )
  )
	)){print(paste("Error saving", new_sitemaps$loc[index]))}

}

  # add new rows for each version
  sitemaps <- sitemaps %>% bind_rows(., sitemaps_crawl) 
  
  print(paste(index, "/", nrow(new_sitemaps), "-", new_sitemaps$loc[index], "saved."))
}

# filter for new entries: #####

#lastcrawl = ifelse("url_list" %in% DBI::dbListTables(conn),
#                   (tbl(conn, "new_sitemaps") %>% dplyr::arrange(desc(last_crawl)) %>% head(1) %>% collect())$last_crawl %>% lubridate::ymd_hms(., tz = "UTC"),
#                   lubridate::ymd_hms("1900-01-01 00:00:01", tz = "UTC") 
#) %>% as_datetime()



# new_pages <- dplyr::filter(sitemaps, lastmod > lastcrawl)
new_pages = sitemaps

#  append table in db: #####

DBI::dbWriteTable(conn = conn, name = "url_list", 
                  value = new_pages %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                  append = TRUE
                  ) 
