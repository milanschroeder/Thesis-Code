
library(tidyverse)
library(RMariaDB)
library(magrittr)
library(rvest)
library(chromote)

# helper: ####

chromote_session <- ChromoteSession$new() # initial start of chromote session

fetch_problematic_content <- function(url_, chromote_session, timeout = 60000){
  
  # if necessary, restart session:
  if(!chromote_session$is_active()){chromote_session <- ChromoteSession$new()} 
  
  # return response as text:
  chromote_session$Runtime$evaluate(
    glue::glue('fetch("{url_}").then(response => response.text());'), 
    awaitPromise = TRUE,
    timeout = timeout
  )$result$value
}

# connect DB ####

source("00_connect_DB_newsfront.R")

# Daten abrufen 

all_links <- tbl(conn, "url_list") %>% collect()

start_from = as.POSIXct("2000-12-31 23:59")
# already scraped:
if ("page_data" %in% DBI::dbListTables(conn)) {
  done_links <- tbl(conn, "page_data") %>% select(loc, available_online) %>% collect()

  new_links <- all_links %>% 

    filter(as.POSIXct(lastmod) >= start_from) %>% # for now
 
    left_join(., done_links, by=join_by(loc)) %>%
    filter(str_detect(origin, "author|category|page|tag", negate=T),
	   is.na(available_online)) %>% select(-available_online) %>%
    distinct(loc, .keep_all=T) %>% 
    slice_sample(., prop = 1) # shuffle to distribute penetration
  
} else{
  new_links <- all_links %>% 

    filter(as.POSIXct(lastmod) > start_from) %>% 

    filter(str_detect(origin, "author|category|page|tag", negate=T)) %>%
    distinct(loc, .keep_all=T) %>% 
    slice_sample(., prop = 1) # shuffle to distribute penetration
}


# base_urls:
base_urls <- tibble(loc = paste0("https://", 
                                     c("", "en.", "bgr.", "de.", "es.", "srb.", "fr.", "hu.", "ge.", "sk.", "pl.", "id."), 
                                     "news-front.su"),
                        version = c("ru", "en", "bgr", "de", "es", "srb", "fr", "hu", "ge", "sk", "pl", "id")
)

# get existing links in beginning:
if ("tag_list" %in% DBI::dbListTables(conn)) {
  existing_tags <- tbl(conn, "tag_list") %>% collect()
} else{
  existing_tags <- tibble()
}

# scraping function #####

scrape_nf_article <- function(link, nf_version, C_session = chromote_session) {

  print(paste(Sys.time(), link))
  
# read page: 
html <- try(
  link %>% read_html(encoding = "utf-8")
  )
# catch weird encoding issues and other stuff:
if ("try-error" %in% class(html)) {
  html <- 
    link %>% 
    fetch_problematic_content(., C_session) %>% 
    read_html() # better leave undefined
}

doc_hash <- rlang::hash(html %>% toString())

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

  loc = link,
  
  time_published = html %>% html_elements(xpath = "//meta[@property='article:published_time']") %>% html_attr("content") %>% lubridate::ymd_hms(., tz = "UTC"),
  time_modified = ifelse(is_empty(html %>% html_elements(xpath = "//meta[@property='article:modified_time']") %>% html_attr("content") %>% lubridate::ymd_hms(., tz = "UTC")),
                         html %>% html_elements(xpath = "//meta[@property='article:published_time']") %>% html_attr("content") %>% lubridate::ymd_hms(., tz = "UTC"),
                         html %>% html_elements(xpath = "//meta[@property='article:modified_time']") %>% html_attr("content") %>% lubridate::ymd_hms(., tz = "UTC")
                         ) %>% as.POSIXct(., tz = "UTC"),
  
  header = html %>% html_elements(".entry-title") %>% html_text2(),
  
  lead = 
    # complex bc ge and hu version lead is very weird sometimes -> in that case capture unformatted first paragraph (usually intended as lead) 
   c(
    html %>% 
      html_elements(xpath = "//div[@class='article__content']//p/strong[not(parent::a)]") %>% html_text2()
    ,
    html %>% 
      html_elements(xpath = "//div[@class='article__content']//h2[not(parent::a)]") %>% html_text2()
    ,
    html %>% 
      html_elements(xpath = "//div[@class='article__content']//h3[not(parent::a)]") %>% html_text2()
  ) %>% 
    str_c(., collapse = " ") %>% 
    ifelse(. == "", 
           html %>% html_elements(".article__content p:nth-child(1)") %>% html_text2(),
           .) %>% 
    str_c(., collapse = " ") %>% 
    str_remove_all(., "[\t\n]") %>% str_c(., collapse = " ") %>% str_squish(),
  
  text = html %>% html_elements(".article__content p") %>% html_text2() %>% str_c(., collapse = " ") %>% 
    str_remove_all("Aufgrund von Zensur und Sperrung aller Medien und alternativer Meinungen abonnieren Sie bitte unseren Telegram-Kanal") %>%  str_squish(),
  
  version_nf = nf_version,
  doc_hash = doc_hash,
  available_online = 1,
  capture_time = Sys.time() %>% lubridate::ymd_hms(., tz = "UTC")
)
# push to DB 
DBI::dbWriteTable(conn, name = "page_data", 
                  value = page_data %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                  append = TRUE
)   



### html_pages: #### (deprecated)
# html_page <- tibble(
#   doc_hash,
#   html_doc = toString(html)
# )
# push to DB 
# DBI::dbWriteTable(conn, name = "html_pages", 
#                   value = html_page %>% dplyr::mutate(across(.cols = !is.character, as.character)),
#                   append = TRUE
# )   




  # videolinks (iframe)
# also captures other embeddings
videos <- tibble(
  doc_hash,
  video_url = html %>% html_elements(".article iframe") %>% html_attr("src")
) %>%
 filter(str_detect(video_url, "about:blank", negate = T))
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

article_tags <- tibble(
  doc_hash,
  tag_label,
  version_nf = nf_version
)

  # tag_url: only write if new tag:
  
if (any(!tag_url %in% existing_tags$tag_url)) {
  new_tags <- tibble(tag_label = tag_label,
                    tag_url = tag_url,
                    version_nf = nf_version
  ) %>% filter(!tag_url %in% existing_tags$tag_url)
  
  existing_tags %<>% bind_rows(., new_tags)
  
  # push tag_list to DB 
  DBI::dbWriteTable(conn, name = "tag_list", 
                    value = new_tags %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                    append = TRUE
  )   
  
  # push article_tags to DB 
  DBI::dbWriteTable(conn, name = "article_tags", 
                    value = article_tags %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                    append = TRUE
  )   
}

  
  # Quotes:
quotes <- tibble(
  doc_hash,
  quote = html %>% html_elements(".article blockquote") %>% html_text2() %>% 
    str_remove_all("Aufgrund von Zensur und Sperrung aller Medien und alternativer Meinungen abonnieren Sie bitte unseren Telegram-Kanal") %>% stringi::stri_remove_empty() %>% 
    str_squish()
    # includes Begleitsatz (no idea how you call this in english :D)
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
  ) %>% filter(str_detect(link_url, "about:blank", negate = T))

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

for (page_i in 1:nrow(new_links)) {

    if("try-error" %in% class(try(
      scrape_nf_article(link = new_links$loc[page_i],
                        nf_version = new_links$version_nf[page_i])
                      ))
    ){
      print(paste("***error saving***", page_i, " / ", nrow(new_links), new_links$loc[page_i]))
      write(x = paste(Sys.time(), new_links$loc[page_i], "main",  sep = ", "), 
            file = "scrapelog_nf.txt", append = T, sep = "\n")
    } else{print(paste(page_i, " / ", nrow(new_links), "done"))}
  
}
