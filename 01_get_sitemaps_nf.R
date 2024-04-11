# get sitemaps:

# create sitemap_index ####
  # (or read from DB)
sitemap_index_nf <- 
  tibble(loc = paste0("https://",
                      c("", "en.", "bgr.", "de.", "es.", "srb.", "fr.", "hu.", "ge.", "sk.", "pl.", "id."),
                      "news-front.su/sitemap_index.xml"),
         version = c("ru", "en", "bgr", "de", "es", "srb", "fr", "hu", "ge", "sk", "pl", "id")
  )

# connect DB ####
source("00_connect_DB_newsfront.R") # returns conn
conn_nf <- conn

# load old url_list:
old_pages_nf <- DBI::dbReadTable(conn, "url_list") # read from DB 


update_sitemaps <- function(sitemap_index = sitemap_index, conn = NULL, old_pages = NULL){
  
  library(tidyerse)
  # install.packages("devtools")
  library(devtools)
  # install_github("pixgarden/xsitemap")
  library(xsitemap)

  # customize function to avoid loosing datetime information and filter for new sitemaps: #### 
  sitemapGet <- function (urltocheck, user_agent, version_index = NULL, update_indicator = NULL) {
    if (missing(user_agent)) {
      user_agent <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/67.0.3396.99 Safari/537.36"
    }
    URL <- xml2::url_parse(urltocheck)
    
    # if not a sitemap:
    if (is.na(URL$path) && !is.na(URL$domain)) {
      message("Searching for XML Sitemap URL...")
      xmlsitemap_from_robots <- xsitemapGetFromRobotsTxt(urltocheck, 
                                                         user_agent)
      if (xmlsitemap_from_robots != "") {
        sitemapGet(xmlsitemap_from_robots, user_agent)
      }
      else {
        xmlsitemap_from_guessing <- xsitemapGuess(urltocheck, 
                                                  user_agent)
        if (!is.null(xmlsitemap_from_guessing)) {
          sitemapGet(xmlsitemap_from_guessing, user_agent)
        }
        else {
          warning(paste("Can't find xml sitemap url for", 
                        urltocheck))
          return(FALSE)
        }
      }
    }
    
    # if a sitemap:
    else if (!is.na(URL$path)) {
      message(paste("Reaching for XML sitemap...", urltocheck))
      request <- httr::GET(urltocheck, httr::user_agent(user_agent))
      if (request$status_code != 200 && request$status_code != 301) {
        warning(paste("xml sitemap is not accessible (HTTP:", 
                      request$status_code))
        return(NULL)
      }
      xml_doc <- XML::xmlParse(request, encoding = "UTF-8")
      xml_data <- XML::xmlToList(xml_doc)
      if (!is.list(xml_data)) {
        warning("empty sitemap index")
        return(NULL)
      }
      nb_children <- length(xml_data)
      
      # sitemap indexes: #### 
      if (!is.null(xml_data$sitemap$loc)) { # for indexes, it's called $sitemap$loc, for regular sitemaps $url$loc
        if (nb_children < 50001) {
          message(paste("sitemap index detected - ", nb_children, 
                        " sitemap url(s) found"))
        }
        else {
          warning(paste("too many URLs - ", nb_children, 
                        " web page url(s) found"))
          return(NULL)
        }
        
        # filter for new or updated sitemaps:
        index <- data.table::rbindlist(xml_data, fill = T) %>% 
          mutate(id = row_number(),
                 lastmod = lastmod %>% clock::date_time_parse(., format = "%Y-%m-%dT%H:%M:%S%Ez", zone = "UTC") %>% lubridate::ymd_hms(., tz="UTC"),
                 origin = urltools::url_parse(loc)$path) 
        if (!is.null(version_index) & !is.null(update_indicator)) {
          index <- index %>% 
            left_join(., update_indicator %>% filter(version_nf == version_index) %>% select(-version_nf), by=join_by(origin)) %>% 
            filter(lastmod > lastmod_old | is.na(lastmod_old))
          
          message(paste(nrow(index), "updated sitemaps identified"))
        }      
        
        
        # loop over individual sitemaps in index:
        urls <- data.frame(loc = character(), lastmod = as_datetime(character()), stringsAsFactors = FALSE)
        for (i in index$id) {
          individual_sitemap <- xml_data[i]$sitemap$loc
          if (!is.null(individual_sitemap)) {
            message(paste0("\n", i, " >>> ", individual_sitemap))
            new_urls <- sitemapGet(individual_sitemap)
            parsed_urls <- urltools::url_parse(individual_sitemap)
            if (!is.na(parsed_urls$parameter)) {
              new_urls$origin <- paste0(parsed_urls$path, 
                                        "?", parsed_urls$parameter)
            }
            else {
              new_urls$origin <- parsed_urls$path
            }
            urls <- rbind(urls, new_urls)
          }
        }
        rownames(urls) <- NULL
        return(urls)
      }
      
      # regular sitemaps: ####
      else {
        message(paste("regular sitemap detected - ", nb_children, 
                      " web page url(s) found"))
        urls <- data.frame(loc = character(), lastmod = as_datetime(character()), stringsAsFactors = FALSE)
        for (i in 1:(nb_children - 1)) {
          cat(".")
          urls[i, ]$loc <- xml_data[i]$url$loc
          # is it necessary to add origin? (Do we ver get here or if we dont provide direct regular sitemap links?)
          
          if (!is.null(xml_data[i]$url$lastmod)) {
            urls[i, ]$lastmod <- xml_data[i]$url$lastmod %>% clock::date_time_parse(., format = "%Y-%m-%dT%H:%M:%S%Ez", zone = "UTC") %>% lubridate::ymd_hms(., tz="UTC")
          }
        }
        rownames(urls) <- NULL
        return(urls)
      }
    }
    else {
      stop(paste("Mal formatted url", urltocheck))
    }
  }
  
  
  # apply ####
  if (!is.null(old_pages)) {
    update_indicator <- old_pages %>% group_by(origin, version_nf) %>% summarise(lastmod_old = max(lastmod)) %>% ungroup()
    } else{update_indicator <- NULL}
  
  ### if locally (no option bc why not?!):
  pages <- tibble()
  
  for (i in 9:nrow(sitemap_index)) {
    
    new_pages <- 
      sitemapGet(sitemap_index[i,]$loc, version_index = sitemap_index[i,]$version, update_indicator = update_indicator) %>% 
      mutate(version_nf = sitemap_index[i,]$version,
             last_crawl = Sys.time()) %>% # beware: not exact!
      filter(!is.na(lastmod)) %>% # starting page duplicates
  
    #only new sitemaps:
      left_join(., 
                 old_pages %>% 
                   filter(version_nf == sitemap_index[i,]$version) %>% # less to compare
                   select(loc, lastmod_old = lastmod),
                   by = join_by(loc)) %>%  
      filter(lastmod != lastmod_old)
    
   pages <- bind_rows(pages, new_pages)
    ### append to DB:
    if (!is.null(conn)) {
      DBI::dbWriteTable(conn, "url_list", new_pages, append=TRUE)
    }   
  }
  
  old_pages <- bind_rows(old_pages, pages %>% select(-lastmod_old))

}
  
# run sitemap updater for newsfront:
update_sitemaps(sitemap_index_nf, conn_nf, old_pages_nf)

### ToDo: indicate whether updated or first scrape! ####
  # at scrape: group > count > capture_no=count+1
  # initial: group > capture_no=seq_along()

####  at init: only write new or updated to DB: ####

# get previously scraped sitemaps:
# already_scraped <- DBI::dbReadTable(conn, "url_list") %>% 
#   mutate(origin = urltools::url_parse(origin)$path) # bc old method recorded full path

# filter for updates and new pages:
# really_new <- left_join(old_pages, already_scraped %>% select(-c(version_nf, last_crawl), base_sitemap = origin), by=join_by(loc, lastmod)) %>% filter(is.na(base_sitemap)) %>% select(-base_sitemap)

# bind w/o duplicates
# all_nf <- bind_rows(already_scraped, really_new)

# write all to DB (Before: mariadb> TRUNCATE TABLE url_list;)
# write_rds(really_new, "newdataforDB.rds")





# get pages to scrape: ####

to_scrape <- 
  DBI::dbReadTable(conn, "url_list") %>%  
  left_join(., tbl(conn, "page_data") %>% select(loc, available_online) %>% collect(), by=join_by(loc)) %>% 
  filter(stringr::str_detect(origin, "author|category|tag|page", negate = T), is.na(available_online)) %>% select(-available_online) %>% # generally, only scrape articles
  filter(lastmod > ymd("2021-12-31")) %>% 
  sample_frac(1) %>% 
  distinct(loc, .keep_all = T)
