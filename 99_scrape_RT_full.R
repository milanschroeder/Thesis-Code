# Scrape full RT DE Archive:
library(pacman)
p_load(magrittr, tidyverse, rvest)

source(file = "00_RT_Scraper.R")

# TEST ####

# test with first ever articles: 
testlinks <- scrapeRT(period_end = "07-09-2014", folder = "test")
test_df <- scrapeRT(period_end = "07-09-2014", folder = "test", full_scrape = T)

# add some articles with more features to test:
more_links <- c("/programme/451-programme/113305-451-grad-gibt-wahlhilfe-keine/", "/meinung/151965-politisch-mediale-akzeptanz-wenn-gute/")
testlinks <- c(testlinks, more_links)
test_df %<>% bind_rows(., 
                       df_tmp = tibble(links_RT_search = more_links))

test_articles_df_table_export <- scrape_articles(linklist = testlinks, folder = "test", lists_as_character = F, nested = F)
test_articles_df <- scrape_articles(linklist = testlinks, folder = "test", lists_as_character = F, nested = T)

# in one pipe:
# test_articles_df <- 
#   scrapeRT(period_end = "07-09-2014", folder = "test") %>% 
#   scrape_articles(., folder = "test")


# one function to scrape them: 
test_articles_df <- scrape_articles(linklist = scrapeRT(folder = "scrape"), 
                                    folder = "scrape")

# ToDo: ensure it doesn't break when some URL is broken (though that shouldn't happen if not inserted manually)
test <- scrapeRT(full_scrape = T, max_attempts = 2, folder = "test", period_start = "20-10-2022", period_end = "23-10-2022")


# save:
openxlsx::write.xlsx(x = RT_archive_partly, file = "RT_searches/archive.xlsx", asTable = T, overwrite = T)
openxlsx::write.xlsx(test_articles_df_table_export, "test/test_data.xlsx", asTable = T, overwrite = T)





# scrape via search function ####
linksRT_all <- scrapeRT(full_scrape = T)
linksRT_all_again <- scrapeRT(period_start = "18-02-2022", full_scrape = T, subfolder = "articles_more")

# scrape from saved:
links_RT_more <- scrape_searches_from_saved(full_scrape = T, folder = "RT_searches", subfolder = "articles_more")

# save(links_RT_more, file = "RT_searches/linksRT_more.RData")
load("RT_searches/linksRT_all.RData")
load("RT_searches/linksRT_more.RData")
load("RT_searches/article_archive_partly.RData")


# list of unique links:
links <- c(links_RT_more$links_RT_search, linksRT_all$links_RT_search) %>% unlist() %>% unique()

# scrape articles:
first_article <- 1
# first_article <- 19269
RT_archive <- scrape_articles(linklist = links[first_article:length(links)], nested = T, begin_with_article = first_article)
# save(RT_archive_partly, file = "RT_searches/article_archive_partly.RData")


# scrape articles from saved:
RT_archive_partly <- scrape_articles_from_saved(nested = T)

archiveJSON <- RT_archive_partly %>% jsonlite::toJSON()

jsonlite::write_json(RT_archive_partly, "RT_searches/archive.JSON")
write.csv2(RT_archive_partly %>%
             mutate(across(.fns = ~ as.character(.)), 
                    across(.fns = ~ replace(., . == "character(0)", "")),  
                    across(.fns = ~ replace(., . == "logical(0)", ""))), 
           "RT_searches/archive.csv")


# get more URLs:
RT_archive_partly <- jsonlite::fromJSON("RT_searches/archive.JSON")

intextlinks <- bind_rows(RT_archive_partly$intext_links)

recommendations <- bind_rows(RT_archive_partly$recommendation)
recommendations_main <- bind_rows(recommendations$recommendation_main)
recommendations_main_links <- bind_rows(recommendations_main$recommendation_main_link)

recommendations_embedded <- bind_rows(recommendations$recommendation_embedded)
recommendations_embedded_links <- bind_rows(recommendations_embedded$recommendation_embedded_link)

recommendations_embedded_links <- flatten(recommendations_embedded$recommendation_embedded_link) %>% flatten() %>% unlist()

moreURLS <- c(
  intextlinks$intext_links, 
  recommendations_main_links$recommendation_main_link, recommendations_embedded_links) %>% 
  unique()  

# relative links:
relativeURLs <- moreURLS %>% str_subset(., "^/") %>% str_replace_all(., "^/", "https://de.rt.com/")

# short URLs with redirect:
shortURLS <- moreURLS %>% 
  str_subset(., "kurz.rt.com/")

get_longURLs <- function(linklist = shortURLS){
  
  library(tidyverse)
  library(RSelenium)
  library(rvest)
  
  # start Remote Driver:
  eCap <- list(
    phantomjs.page.settings.userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/100.0.4896.75 Safari/537.36"
  )
  
  system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
  rD <- rsDriver(browser = "chrome",
                 chromever = "106.0.5249.61",
                 # ToDo: automate version selection
                 verbose = F,
                 javascript = T,
                 nativeEvents = T,
                 extraCapabilities = eCap)
  remDr <- rD[["client"]]
  
  full_links <- c()
  
  for (link in linklist) {
      new_full_link <- "start"
    while (str_detect(new_full_link, "https://de.rt.com") == F) {
      remDr$navigate(link)
      Sys.sleep(1)
      new_full_link <- remDr$getCurrentUrl()[[1]]
    }
    full_links <- append(full_links, new_full_link)
  }
  return(full_links)
}

long_shortURLs <- get_longURLs()

# old and new sublevel domains work interchangably:
oldURLS <- moreURLS %>% 
  str_subset(., "deutsch.rt.com/") 
oldURLS %<>% str_replace_all(., "https://deutsch.rt.com/", "https://de.rt.com/")

newURLS <- moreURLS %>% 
  str_subset(., "de.rt.com/") 

URLs_combined <- c(newURLS, oldURLS, relativeURLs, long_shortURLs) %>% 
  unique() %>% 
  tibble(link = .) 

new_URLs <-  setdiff(URLs_combined$link, RT_archive_partly$link)
# are there encoding issues?!

NROW(c(new_URLs, RT_archive_partly$link))
n_distinct(c(new_URLs, RT_archive_partly$link))

# add variable indicating how article was found!
# ToDo: add recommendation count to dataset!



# SCRAPE from sitemap: ####

# list of base sitemap URLs:
sitemap_versions <- tribble(
  ~version, ~link,
  # "bk", "https://rt.rs/...,
  "de", "https://deutsch.rt.com/sitemap.xml",
  "en", "https://swentr.site/sitemap.xml",
  "fr", "https://francais.rt.com/sitemap.xml",
  "ru", "https://russian.rt.com/sitemap.xml",
  "ar", "https://arabic.rt.com/sitemap.xml",
  "es", "https://actualidad.rt.com/sitemap.xml"
)

# load results of last scrape:
load("sitemaps/sitemaps_crawl.RData")

# get time of last scrape:
lastscrape <- if (exists("sitemap_base_archived")) {
  lubridate::ymd_hms(max(sitemap_base_archived$last_scrape))  
} else {lubridate::ymd_hms("0000-01-01 00:00:00 UTC")}

# scrape base sitemaps for sub-sitemaps:
sitemap_base = tibble()
for (i in 1:nrow(sitemap_versions)) {
  page <- rvest::read_html(sitemap_versions$link[i])
  sitemap_base_tmp <- tibble(loc = page %>% html_elements("loc") %>% html_text2(),
                              lastmod = page %>% html_elements("lastmod") %>% lubridate::ymd(),
                              last_scrape = Sys.time(),
                              version = sitemap_versions$version[i]
                              )  
  sitemap_base <- bind_rows(sitemap_base, sitemap_base_tmp)
  }

# only check updated sub-sitemaps:
updated_sitemaps <- dplyr::filter(sitemap_base, lastmod > lastscrape) 

# ToDo: TEST! 
# replace only updated sitemaps in sitemap_base_archived:
sitemap_base_archived <- rbind(filter(sitemap_base, !(loc %in% updated_sitemaps$loc)),
                               updated_sitemaps)

if (!dir.exists("sitemaps")) {
  dir.create("sitemaps")
}

# old stuff:
{
# write.csv(sitemap_base_archived, "sitemaps/sitemaps_base_archived.csv")
# updated_sitemaps <- read.csv("sitemaps/sitemaps_base_archived.csv")

# special cases:

# {
# sitemap_arabic <- tribble(
#   ~version, ~link,
#   "ar", "https://arabic.rt.com/sitemap.xml")
# 
#   sitemaps_arabic = tibble()
#   for (i in 1:nrow(sitemap_arabic)) {
#     page <- rvest::read_html(sitemap_arabic$link[i])
#     sitemap_arabic_tmp <- tibble(loc = page %>% html_elements("loc") %>% html_text2(),
#                                lastmod = page %>% html_elements("lastmod") %>% lubridate::ymd(),
#                                last_scrape = Sys.time(),
#                                version = sitemap_arabic$version[i]
#     )  
#     sitemaps_arabic <- bind_rows(sitemaps_arabic, sitemap_arabic_tmp)
#   }
  # 
  # special_case <- updated_sitemaps %>% filter(loc == "https://swentr.site/sitemap_2000.xml")
  # special_case <- bind_rows(special_case, sitemaps_arabic)
  
#   updated_sitemaps <- updated_sitemaps %>% filter(loc != "https://swentr.site/sitemap_2000.xml")
# }

# sitemaps_more = tibble()
# for (i in 1:nrow(special_case)) {
#   page <- rvest::read_html(special_case$loc[i])
#   sitemap_tmp <- list(loc = page %>% html_elements(xpath = "//url/loc") %>% html_text2(),
#                       lastmod_utc = page %>% html_elements("lastmod") %>% lubridate::ymd_hms(tz = "UTC"),
#                       lastmod_tz = page %>% html_elements("lastmod") %>% html_text2() %>% str_sub(start = -6),
#                       last_scrape = Sys.time(),
#                       language = special_case$version[i]
#   )  
#   sitemaps_more <- bind_rows(sitemaps_more, sitemap_tmp)
# }
  
  # sitemap_base_archived <- bind_rows(
  #   sitemap_base %>% 
  #     mutate(lastmod = as.Date(lastmod), 
  #            last_scrape = as.POSIXct(last_scrape)) %>% select(-X), 
  #   special_case)
  
  
  
}
# end of old stuff 

# function to scrape, but preferable to use loop:
updated_locs <- updated_sitemaps %>% select(loc) %>% as_vector()
get_urls_sitemap <- function(sitemap){
  sitemap_year <- tibble(loc = rvest::read_html(sitemap) %>% html_elements("loc") %>% html_text2(),
                         lastmod_utc = rvest::read_html(sitemap) %>% html_elements("lastmod") %>% lubridate::ymd_hms(tz = "UTC"),
                         lastmod_tz = rvest::read_html(sitemap) %>% html_elements("lastmod") %>% html_text2() %>% str_sub(start = -6),
                         
  )
  return(sitemap_year)
  Sys.sleep(.5)
}
article_links <- map_dfr(updated_locs, get_urls_sitemap)

# as loop
sitemaps = tibble()
for (i in 1:nrow(updated_sitemaps)) {
  page <- rvest::read_html(updated_sitemaps$loc[i])
  sitemap_tmp <- list(loc = page %>% html_elements(xpath = "//url/loc") %>% html_text2(),
                      lastmod_utc = page %>% html_elements("lastmod") %>% lubridate::ymd_hms(tz = "UTC"),
                      lastmod_tz = page %>% html_elements("lastmod") %>% html_text2() %>% str_sub(start = -6),
                      last_scrape = Sys.time(),
                      language = updated_sitemaps$version[i]
  )  
 sitemaps <- bind_rows(sitemaps, sitemap_tmp)
}

sitemaps <- sitemaps_all
save(sitemaps, sitemap_base_archived, file = "sitemaps/sitemaps_crawl.RData")
write.csv(sitemaps, "sitemaps/crawled_urls.csv")
