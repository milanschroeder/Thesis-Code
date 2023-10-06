# sitemap updater:

# connect to DB: #####
library(pacman)
pacman::p_load(tidyverse, DBI, RSQLite)
RT_DB <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = "ignore/Russian_Media_Database_RT.sqlite") 

# function to get time of last scrape #####
get_lastscrape <- function(df, lastscrape_var = "last_scrape"){
  
  pacman::p_load(tidyverse, DBI, RSQLite)
  RT_DB <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = "ignore/Russian_Media_Database_RT.sqlite") 
  
  if ("base_sitemaps" %in% DBI::dbListTables(RT_DB)) {
    (dplyr::tbl(RT_DB, df) %>% dplyr::select(last_scrape = lastscrape_var) %>% dplyr::collect() %>% 
       dplyr::summarize(lastscrape = max(lubridate::ymd_hms(last_scrape), na.rm = T)))$lastscrape
  } else {lubridate::ymd_hms("0000-01-01 00:00:00 UTC")}
}


# define list of sources(sitemap_versions) ####

# list of base sitemap URLs:
if (!("sitemap_versions" %in% DBI::dbListTables(RT_DB))) {  # create if not exist
sitemap_versions <- dplyr::tribble(
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

# scrape base sitemaps for sub-sitemaps:
scrape_base_sitemaps <- function(){
  
  pacman::p_load(tidyverse, rvest, DBI, RSQLite)
  RT_DB <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = "ignore/Russian_Media_Database_RT.sqlite") 
  
  sitemap_versions <- dplyr::tbl(RT_DB, "sitemap_versions") %>% dplyr::collect() 
  
  sitemap_base <- tibble::tibble()
  for (i in 1:nrow(sitemap_versions)) {
    page <- rvest::read_html(sitemap_versions$link[i])
    sitemap_base_tmp <- tibble(source_loc = page %>% rvest::html_elements("loc") %>% rvest::html_text2(),
                               lastmod = page %>% rvest::html_elements("lastmod") %>% lubridate::ymd(),
                               last_scrape = Sys.time(),
                               version = sitemap_versions$version[i]
    )  
    sitemap_base <- dplyr::bind_rows(sitemap_base, sitemap_base_tmp)
  }
  
  
  # check which were updated:
  lastscrape_base <- get_lastscrape("base_sitemaps") # get time of last scrape
  updated_sitemaps <- dplyr::filter(sitemap_base, lastmod > lastscrape_base) 
  
  # safe full list to RT_DB:
  base_sitemaps <- updated_sitemaps %>% 
    rbind(., dplyr::tbl(RT_DB, "base_sitemaps") %>% dplyr::collect()) %>%
    dplyr::distinct(source_loc, .keep_all = T)
  
# ToDo: 
  # append instead of overwrite!
  
  DBI::dbWriteTable(conn = RT_DB, name = "base_sitemaps", 
                    value = base_sitemaps %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                    field.types = c(
                      source_loc = "TEXT",
                      lastmod = "DATE",
                      last_scrape = "TIMESTAMP",
                      version = "TEXT"
                    ), overwrite = T
                    )
  
  return(updated_sitemaps)
}

# sitemap scraping function ####

scrape_sitemaps <- function(sitemaps_source = updated_sitemaps, sleeptime = .5){
  
  pacman::p_load(tidyverse, rvest, DBI, RSQLite)
  RT_DB <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = "ignore/Russian_Media_Database_RT.sqlite") 
  
  # options: 1) scrape not only updated_sitemaps (default), 2) base_sitemaps from RT_DB ("all"), 3) individually specified df_linklist (with var $loc, $version)
  scrape_all <- F    
  if (sitemaps_source == "all") {
      sitemaps_source <- dplyr::tbl(RT_DB, "base_sitemaps") %>% dplyr::collect()
      scrape_all <- T
    }
  
  # scrape with loop
  sitemaps_new <- tibble::tibble()
  for (i in 1:nrow(sitemaps_source)) {
      
    scrapetime_tmp <- Sys.time() # saved here to make sure not to miss any articles posted between scraping and saving
    page <- try(rvest::read_html(sitemaps_source$source_loc[i]), silent = T)
     
     # write into log if error:
      if ("try-error" %in% class(page)) {
        write(x = paste(Sys.time(), sitemaps_source$source_loc[i], "scrape_sitemaps",  sep = ", "), 
              file = "ignore/scrape_log.txt", append = T, sep = "\n")
      }else{
      
        # get data:
        sitemap_tmp <- tibble(loc = page %>% rvest::html_elements(xpath = "//url/loc") %>% rvest::html_text2(),
                              lastmod_utc = page %>% rvest::html_elements("lastmod") %>% lubridate::ymd_hms(tz = "UTC"),
                              lastmod_tz = page %>% rvest::html_elements("lastmod") %>% rvest::html_text2() %>% stringr::str_sub(start = -6),
                              last_scrape = scrapetime_tmp,
                              version = sitemaps_source$version[i],
                              source_loc = sitemaps_source$source_loc[i]
                              )  
        sitemaps_new <- dplyr::bind_rows(sitemaps_new, sitemap_tmp)
      # get latinized version of RT Balkan as well:
        if (sitemaps_source$version[i] == "bk") {
          sitemaps_new <- dplyr::bind_rows(sitemaps_new, sitemap_tmp %>% 
                                             mutate(loc = loc %>% str_replace("://", "://lat."),
                                                    version = "bk-lat"))
        }
        cat(Sys.time(), sitemaps_source$source_loc[i], "succesfully captured")
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
                    value = updated_pages %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                    append = TRUE
                    ) 
  
  return(updated_pages)
  
  }