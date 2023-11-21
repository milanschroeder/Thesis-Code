
library(tidyverse)
library(RMariaDB)
library(magrittr)
library(rvest)


# Datenbank
# connect

# server version:
source("00_connect_DB_newsfront.R")
mydb <- conn

#Link-Datensatz für 1 Jahr (24.02.2022 - 23.02.2023). Links wurden von Sitemaps davor gescraped. 

# all_links_newsfront = read.csv("news_front_df_all.csv") # adapt to get links from db

# Daten abrufen 
SQL_code = "SELECT * from page_data;"    # adapt to get already scraped links from db
done_links_df = dbGetQuery(mydb, SQL_code)

new_links <- 
(all_links <- tbl(mydb, "url_list") %>% collect()
  ) %>% filter(as.POSIXct(lastmod) > as.POSIXct("2022-01-01 00:00"))
### later: 
# new_links = all_links %>% filter(!loc %in% done_links)



# base_urls:
base_urls <- tibble(loc = paste0("https://", 
                                     c("", "en.", "bgr.", "de.", "es.", "srb.", "fr.", "hu.", "ge.", "sk."), 
                                     "news-front.su"),
                        version = c("ru", "en", "bgr", "de", "es", "srb", "fr", "hu", "ge", "sk")
)

link <- "https://de.news-front.su/2023/11/06/in-der-ukraine-sind-die-nazis-eine-organisierte-politische-kraft-die-britische-schauspielerin-roseanne-barr/"

# new function #####

scrape_nf_article <- function(link, nf_version) {
  print(paste(Sys.time(), link))

html <- link %>% read_html()
doc_hash <- rlang::hash(html)

base_url <- case_when(
  nf_version == base_urls$version[1] ~ base_urls$loc[1],
  nf_version == base_urls$version[2] ~ base_urls$loc[2],
  nf_version == base_urls$version[3] ~ base_urls$loc[3],
  nf_version == base_urls$version[4] ~ base_urls$loc[4],
  nf_version == base_urls$version[5] ~ base_urls$loc[5],
  nf_version == base_urls$version[6] ~ base_urls$loc[6],
  nf_version == base_urls$version[7] ~ base_urls$loc[7],
  nf_version == base_urls$version[8] ~ base_urls$loc[8],
  nf_version == base_urls$version[9] ~ base_urls$loc[9],
  nf_version == base_urls$version[10] ~ base_urls$loc[10]
)

# page_data:
page_data <- tibble(

  link = link,
  
  time_published = html %>% html_elements(xpath = "//meta[@property='article:published_time']") %>% html_attr("content") %>% lubridate::ymd_hms(., tz = "UTC"),
  time_modified = html %>% html_elements(xpath = "//meta[@property='article:modified_time']") %>% html_attr("content") %>% lubridate::ymd_hms(., tz = "UTC"),
  
  header = html %>% html_elements(".entry-title") %>% html_text2(),
  lead = html %>% html_elements(".article h2") %>% html_text2() %>% str_c(., collapse = " ") %>% str_squish(),
  text = html %>% html_elements(".article__content p") %>% html_text2() %>% str_c(., collapse = " ") %>% str_squish(),
  
  nf_version,
  doc_hash = doc_hash,
  
  capture_time = Sys.time()

)
# push to DB 
DBI::dbWriteTable(conn, name = "page_data", 
                  value = page_data %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                  append = TRUE
)   


  # videolinks (iframe)
videos <- tibble(
  doc_hash,
  video_url = html %>% html_elements(".article iframe") %>% html_attr("src")
)
# push to DB 
if (nrow(videos) > 0) {
  DBI::dbWriteTable(conn, name = "videos", 
                  value = videos %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                  append = TRUE
  )
}


  # images 

images <- tibble(
  doc_hash,
  img_url = html %>% html_elements(".article__content img") %>% html_attr("src"),
  img_alt = html %>% html_elements(".article__content img") %>% html_attr("alt")
) %>% filter(!str_starts(img_url, "data"))
# push to DB 
DBI::dbWriteTable(conn, name = "images", 
                  value = images %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                  append = TRUE
)   


  # tags
tag_label <- html %>% html_elements(".tag") %>% html_text2()
tag_url <- html %>% html_elements(".tag") %>% html_attr("href")

tags <- tibble(
  doc_hash,
  tag_label
)

  # tag_url: only write if new tag:
# get existing links in beginning:
existing_tags <- tbl(mydb, "tag_list") %>% collect()

if (!tag %in% existing_tags$tag_label) {
  new_tags <- tibble(tag_label = tag_label,
                    tag_url = tag_url
  ) %>% filter(!tag_label %in% existing_tags$tag_label)
  
  existing_tags %<>% bind_rows(., new_tags)
  
  # push to DB 
  DBI::dbWriteTable(conn, name = "tag_list", 
                    value = new_tags %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                    append = TRUE
  )   
}

  
  # Quotes:
quotes <- tibble(
  doc_hash,
  quote <- html %>% html_elements(".article blockquote") %>% html_text2() # includes Begleitsatz (no idea how you call this in english :D)
)
# push to DB 
if (nrow(quotes) > 0) {
  DBI::dbWriteTable(conn, name = "quotes", 
                  value = quotes %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                  append = TRUE
  )   
}

links <- tibble(
  doc_hash,
  link_text = html %>% html_elements(".article__content a") %>% html_text2(),
  link_url = html %>% html_elements(".article__content a") %>% html_attr("href")
  ) 

external_links <- links %>% filter(., str_detect(link_url, base_url, negate = T))
internal_links <- links %>% filter(., str_detect(link_url, base_url))

# push to DB 
if (nrow(external_links) > 0) {
  DBI::dbWriteTable(conn, name = "external_links", 
                  value = external_links %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                  append = TRUE
  )
}

if (nrow(internal_links) > 0) {
  DBI::dbWriteTable(conn, name = "internal_links", 
                    value = internal_links %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                    append = TRUE
  )
}


} # end of scrape_nf_article


# apply function ####

for (row in 1:nrow(new_links)) {
  srape_nf_article(link = new_links$loc,
                   nf_version = new_links$version)
}
