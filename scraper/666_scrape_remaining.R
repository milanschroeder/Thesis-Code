# scrape remaining:


# get doc_hash from html_pages not in page_data ####
source("00_connect_DB_newsfront.R")
library(tidyverse)
library(rvest)

scraped_links <- tbl(conn, "page_data") %>% select(loc) %>% collect() %>% as_vector()
# also filter out unsuccessful attempts, so page_date is the only thing needed to be scraped...
scrapelog <- read.csv("scrapelog_nf.txt", header = F) %>% mutate(loc = str_squish(V2)) %>% select(loc) %>% as_vector()
start_from = as.POSIXct("2022-01-01 00:00") # for now, bc time of interest  

non_scraped_links <- 
  tbl(conn, "url_list") %>% 
  filter(lastmod >= start_from) %>% 
  filter(str_detect(base_sitemap, "post-sitemap")) %>% 
  filter(!loc %in% scrapelog) %>% 
  filter(!loc %in% scraped_links) %>% 
  collect()


# join original doc_hashes with link ####
scraped_pages <- tbl(conn, "page_data") %>% select(doc_hash) %>% collect() %>% as_vector()
htmls_to_link_pointer <- 
  tbl(conn, "html_pages") %>% 
  filter(!doc_hash %in% scraped_pages)

html_links <- tibble()
for (i in 1:tally(htmls_to_link_pointer)) {
  
  html_query <- paste0("SELECT * FROM html_pages WHERE (NOT(`doc_hash` IN ('", 
                       str_c(scraped_pages, collapse = "', '"),
                       "'))) LIMIT 1 OFFSET"
                       )
  html_entry <- DBI::dbGetQuery(conn, paste(html_query, i-1, ";"))
  
  html_links <- 
    bind_rows(
      html_links,
      tibble_row(
        doc_hash = html_entry$doc_hash,
        html_url = html_entry$html_doc %>% read_html() %>% html_elements(xpath = "//meta[@property='og:url']") %>% html_attr("content")
      ) # end of row
    ) # end of bind
  
  print(paste(html_links$html_url[i], "read"))
  
} # end of loop
