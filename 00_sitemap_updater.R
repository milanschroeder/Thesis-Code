# sitemap updater:

# connect to DB: #####
require(tidyverse)
require(DBI)

source("00_connect_DB_RT.R")

# helper function to get time of last scrape #####
get_lastscrape <- function(df, lastscrape_var = "last_scrape"){
  
  require(tidyverse)
  require(DBI)
  

  if ("base_sitemaps" %in% DBI::dbListTables(conn)) {
    dplyr::tbl(conn, df) %>% dplyr::select(last_scrape = lastscrape_var) %>% arrange(-lastscrape_var) %>% head(1) %>% dplyr::pull()
    # (dplyr::tbl(conn, df) %>% dplyr::select(last_scrape = lastscrape_var) %>% dplyr::collect() %>% 
    #    dplyr::summarize(lastscrape = max(lubridate::ymd_hms(last_scrape), na.rm = T)))$lastscrape
  } else {lubridate::ymd_hms("0000-01-01 00:00:00 UTC")}
}


# define list of sources(sitemap_versions) ####

# list of base sitemap URLs:
if (!("sitemap_versions" %in% DBI::dbListTables(conn))) {  # create if not exist
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
DBI::dbWriteTable(conn = conn, name = "sitemap_versions", value = sitemap_versions,
                  field.types = c(version = "TEXT", link = "TEXT"))
}

# load results of last scrape:
# load("ignore/sitemaps/sitemaps_crawl.RData")

# base_sitemap scraping function ####

# scrape base sitemaps for sub-sitemaps:
scrape_base_sitemaps <- function(){
  
  require(tidyverse)
  require(rvest)
  require(DBI)
  
  
  sitemap_versions <- dplyr::tbl(conn, "sitemap_versions") %>% dplyr::collect() 
  
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
  
  # safe full list to conn:
  base_sitemaps <- updated_sitemaps %>% 
    bind_rows(., dplyr::tbl(conn, "base_sitemaps") %>% dplyr::collect()) %>%
    dplyr::distinct(source_loc, .keep_all = T)
  
# ToDo: 
  # append instead of overwrite!
  
  DBI::dbWriteTable(conn = conn, name = "base_sitemaps", 
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
  
  require(tidyverse)
  require(rvest)
  require(DBI)
  
  
  # options: 1) scrape not only updated_sitemaps (default), 2) base_sitemaps from RT_DB ("all"), 3) individually specified df_linklist (with var $loc, $version)
  scrape_all <- F    
  # if (sitemaps_source == "all") {
  #     sitemaps_source <- dplyr::tbl(conn, "base_sitemaps") %>% dplyr::collect()
  #     scrape_all <- T
  #   }
  
  lastscrape_sitemaps <- get_lastscrape("page_list") # get time of last scrape
      
  # scrape with loop
  updated_pages <- tibble()
  for (i in 1:nrow(sitemaps_source)) {
      
    scrapetime_tmp <- Sys.time() # saved here to make sure not to miss any articles posted between scraping and saving
    page <- try(rvest::read_html(sitemaps_source$source_loc[i]), silent = T)
     
     # write into log if error:
      if ("try-error" %in% class(page)) {
        # write(x = paste(Sys.time(), sitemaps_source$source_loc[i], "scrape_sitemaps",  sep = ", "), 
        #       file = "ignore/scrape_log.txt", append = T, sep = "\n")
        print(paste("ERROR saving", sitemaps_source$source_loc[i], ":("))
      }else{
      
        # get data:
        sitemap_tmp <- tibble(loc = page %>% rvest::html_elements(xpath = "//url/loc") %>% rvest::html_text2(),
                              lastmod_utc = page %>% rvest::html_elements("lastmod") %>% lubridate::ymd_hms(tz = "UTC"),
                              lastmod_tz = page %>% rvest::html_elements("lastmod") %>% rvest::html_text2() %>% stringr::str_sub(start = -6),
                              last_scrape = scrapetime_tmp,
                              version = sitemaps_source$version[i],
                              source_loc = sitemaps_source$source_loc[i]
                              ) %>% 
          dplyr::filter(., lastmod_utc > lastscrape_sitemaps) 
        
        sitemaps_new <- sitemap_tmp

      # get latinized version of RT Balkan as well:
        if (sitemaps_source$version[i] == "bk") {
          sitemaps_new <- dplyr::bind_rows(sitemaps_new, sitemap_tmp %>% 
                                             mutate(loc = loc %>% str_replace("://", "://lat."),
                                                    version = "bk-lat"))
        }
       
    

# save to DB:   
    DBI::dbWriteTable(conn = conn, name = "page_list", 
                    value = sitemaps_new %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                    append = TRUE
                    ) 
    
    updated_pages <- dplyr::bind_rows(updated_pages, sitemaps_new)
    
    cat(toString(Sys.time()), sitemaps_source$source_loc[i], "succesfully captured\n")
    
    Sys.sleep(sleeptime)
    
      } 
    } # end of loop
  
  # no comparison to existing links in conn to keep track of page updates!
  
  return(updated_pages)
  
}
