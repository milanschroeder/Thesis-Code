# sitemap updater:

# connect to DB: #####
pacman::p_load(DBI, RSQLite, dplyr)
RT_DB <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = "ignore/Russian_Media_Database_RT.sqlite") 


# define list of sources(sitemap_versions) ####

# list of base sitemap URLs:
if (!("sitemap_versions" %in% DBI::dbListTables(RT_DB))) {  # create if not exist
sitemap_versions <- tribble(
  ~version, ~link,
  "bk", "https://rt.rs/sitemap.xml",      # add "lat." for non-kyrillic version
  "de", "https://deutsch.rt.com/sitemap.xml",
  "en", "https://swentr.site/sitemap.xml",
  "fr", "https://francais.rt.com/sitemap.xml",
  "ru", "https://russian.rt.com/sitemap.xml",
  "ar", "https://arabic.rt.com/sitemap.xml",
  "es", "https://actualidad.rt.com/sitemap.xml"
)
DBI::dbWriteTable(conn = RT_DB, name = "sitemap_versions", value = sitemap_versions,
                  field.types = c(version = "TEXT", link = "TEXT"))
}

# load results of last scrape:
# load("ignore/sitemaps/sitemaps_crawl.RData")

# base_sitemap scraping function ####

# get time of last scrape: 
get_lastscrape <- function(df, lastscrape_var = "last_scrape"){
  if ("base_sitemaps" %in% DBI::dbListTables(RT_DB)) {
  (dplyr::tbl(RT_DB, df) %>% select(last_scrape = lastscrape_var) %>% collect() %>% 
                   summarize(lastscrape = max(lubridate::ymd_hms(last_scrape), na.rm = T)))$lastscrape
  } else {lubridate::ymd_hms("0000-01-01 00:00:00 UTC")}
}
lastscrape_base <- get_lastscrape("base_sitemaps")


# scrape base sitemaps for sub-sitemaps:
scrape_base_sitemaps <- function(){
  pacman::p_load(tidyverse, rvest, DBI)
  sitemap_versions <- dplyr::tbl(RT_DB, "sitemap_versions") %>% collect() 
  
  sitemap_base <- tibble()
  for (i in 1:nrow(sitemap_versions)) {
    page <- rvest::read_html(sitemap_versions$link[i])
    sitemap_base_tmp <- tibble(loc = page %>% rvest::html_elements("loc") %>% rvest::html_text2(),
                               lastmod = page %>% rvest::html_elements("lastmod") %>% lubridate::ymd(),
                               last_scrape = Sys.time(),
                               version = sitemap_versions$version[i]
    )  
    sitemap_base <- bind_rows(sitemap_base, sitemap_base_tmp)
  }
  
  
  # check which were updated:
  lastscrape_base <- get_lastscrape("base_sitemaps") # get time of last scrape
  updated_sitemaps <- dplyr::filter(sitemap_base, lastmod > lastscrape_base) 
  
  # safe full list to RT_DB:
  base_sitemaps <- updated_sitemaps %>% 
    rbind(., dplyr::tbl(RT_DB, "base_sitemaps") %>% collect()) %>%
    distinct(loc, .keep_all = T)
  
  DBI::dbWriteTable(conn = RT_DB, name = "base_sitemaps", 
                    value = base_sitemaps %>% mutate(across(.cols = !is.character, as.character)),
                    field.types = c(
                      loc = "TEXT",
                      lastmod = "DATE",
                      last_scrape = "TIMESTAMP",
                      version = "TEXT"
                    ), overwrite = T
                    )
  
  return(updated_sitemaps)
}

# sitemap scraping function ####

scrape_sitemaps <- function(sitemaps_source = updated_sitemaps, sleeptime = .5){
  
  # options: 1) scrape not only updated_sitemaps (default), 2) base_sitemaps from RT_DB ("all"), 3) individually specified df_linklist (with var $loc, $version)
  scrape_all <- F    
  if (sitemaps_source == "all") {
      sitemaps_source <- dplyr::tbl(RT_DB, "base_sitemaps") %>% collect()
      scrape_all <- T
    }
  
  # scrape with loop
  sitemaps_new <- tibble()
  for (i in 1:nrow(sitemaps_source)) {
      
    scrapetime_tmp <- Sys.time() # saved here to make sure not to miss any articles posted between scraping and saving
    page <- try(rvest::read_html(sitemaps_source$loc[i]), silent = T)
     
     # write into log if error:
      if ("try-error" %in% class(page)) {
        write(x = paste(Sys.time(), sitemaps_source$loc[i], "scrape_sitemaps",  sep = ", "), 
              file = "ignore/scrape_log.txt", append = T, sep = "\n")
      }else{
      
        # get data:
        sitemap_tmp <- tibble(loc = page %>% html_elements(xpath = "//url/loc") %>% html_text2(),
                              lastmod_utc = page %>% html_elements("lastmod") %>% lubridate::ymd_hms(tz = "UTC"),
                              lastmod_tz = page %>% html_elements("lastmod") %>% html_text2() %>% str_sub(start = -6),
                              last_scrape = scrapetime_tmp,
                              version = sitemaps_source$version[i],
                              source_loc = sitemaps_source$loc[i]
                              )  
        sitemaps_new <- bind_rows(sitemaps_new, sitemap_tmp)
        cat(Sys.time(), sitemaps_source$loc[i], "succesfully captured")
      }
    Sys.sleep(sleeptime)
    } # end of loop
  
  if (scrape_all == F) {
  # check which pages were updated:
    lastscrape_sitemaps <- get_lastscrape("page_list") # get time of last scrape
    updated_pages <- dplyr::filter(sitemaps_new, lastmod_utc > lastscrape_sitemaps) 
  } else{
  # just save all
    updated_pages <- sitemaps_new 
  }
  
  # no comparison to existing links in RT_DB to keep track of page updates!

  DBI::dbWriteTable(conn = RT_DB, name = "page_list", 
                    value = updated_pages %>% mutate(across(.cols = !is.character, as.character)),
                    field.types = c(
                      loc = "TEXT",
                      lastmod_utc = "TIMESTAMP",
                      lastmod_tz = "TEXT",
                      last_scrape = "TIMESTAMP",
                      version = "TEXT",
                      source_loc = "TEXT"
                      ), append = TRUE
                    ) 
  }