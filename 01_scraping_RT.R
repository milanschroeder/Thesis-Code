# 01 Scraping RT

### by search queries directly from RT ####
# make sure to use DNS server/IP address that avoids blocking

eCap <- list(
  phantomjs.page.settings.userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/100.0.4896.75 Safari/537.36"
)

## create general (basic) function to scrape RT DE searches: ####
scrapeRT <- function(startdate = startdate, 
                     period_end = period_end, 
                     keyword = "mehr",
                     interval_length = 2,
                     sleepmin = 1,
                     sleepmax = 3,
                     folder = "RT_searches"){
  
  # ToDO: Check input
  # startdate & period_end may be anything convertable by lubridate::dmy(), e.g. a string in the format "dd-mm-yyyy"
  # keyword is not case sensitive (what about special characters?)
  # interval length must be (convertable to) an integer (else: use default)
  
  # load required packages
  # ToDO: add installer and check 
  library(lubridate)
  library(RSelenium)
  library(rlist)
  library(rvest)
  
  # start Remote Driver:
  system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
  rD <- rsDriver(browser = "chrome",
                 chromever = "99.0.4844.51",
                 verbose = F,
                 javascript = T,
                 nativeEvents = T,
                 extraCapabilities = eCap)
  remDr <- rD[["client"]]
  
  # be careful not to get blocked:
  # probably better don't include this as an option(?)
  max_attempts = 3
  
  # ToDo: only if option (still missing) named == T
  # list_names <- c()
  
  # create empty vector to write to 
  links_RT_search <- c()
  startdate = lubridate::dmy(startdate)
  enddate <- startdate %>%  
    add_with_rollback(., days(interval_length))
  rollback_length <- ifelse(interval_length == 0,
                     1,
                     interval_length)
  searchterm <- ifelse(keyword == "mehr",
                       "",
                       keyword)
  
  while (startdate <= dmy(period_end)) {
    # loop over search URLs by time interval:
    search <- paste0("https://de.rt.com/search?q=", keyword ,"&df=", startdate, "&dt=", enddate)  
    
    while(remDr$getCurrentUrl() != search){
      remDr$navigate(search)
      }
    
    attempts <- 0
    while(attempts < max_attempts) {
      
      webElem1 <- NULL
      webElem2 <- NULL
      webElemEnd <- NULL
      attempts <- 0
      
      while(is.null(webElem1) & is.null(webElem2) & attempts < max_attempts){
        
        # scroll to bottom
        while (is.null(webElemEnd)) {
          webElemEnd <- tryCatch({remDr$findElement("css", "body")},
                                 error = function(e){NULL})
          try(webElemEnd$sendKeysToElement(list(key = "end")))
        }
        
        pages_source <- remDr$getPageSource()[[1]]
        
        # search for "Weiter" button and click on it
        #  while(is.null(webElem1) & is.null(webElem2) & attempts < max_attempts){
        tryCatch({webElem1 <- remDr$findElement(using = "css selector", ".Button_dark")},
                 error = function(e){NULL})
        
        try(webElem1$clickElement())
        
        Sys.sleep(1)
        
        tryCatch({webElem2 <- remDr$findElement(using = "css selector", ".Button-root.Button-is1to1.Button-type_l.Button_dark")},
                 error = function(e){NULL})
        try(webElem2$clickElement())

        Sys.sleep(1)
         
        # check if still on right page:
        while(remDr$getCurrentUrl() != search){
          remDr$goBack()
        }
               
        # check if sth still changes:
        pages_source_compare <- remDr$getPageSource()[[1]]
        if (pages_source == pages_source_compare){
          attempts = max_attempts
        }
        
        # sleep to avoid blocking
        randsleep <- sample(seq(sleepmin, sleepmax, by = 0.001), 1)
        Sys.sleep(randsleep)
        
        attempts = attempts + 1
        
      }
      
    }
    
    # save and overwrite with every loop, in case sth breaks
  # ToDo: test if this works within new_links assignment pipe as well
    # html_search <- remDr$getPageSource()[[1]] 
    
    # print(paste("search page successfully saved after", i, " clicks"))
    page <- remDr$getPageSource()[[1]]
    
    # save search results as txt:
# ToDo: check if folder exists, else create
    if (interval_length == 0) {
      writeLines(page, paste0("articles/", folder, "/", searchterm, startdate, ".txt"), useBytes = T)
    } else {
      writeLines(page, paste0("articles/", folder, "/", searchterm, startdate, "-", enddate, ".txt"), useBytes = T)
    }
    
    # safe list of links per interval:
    new_links <- page %>% 
      read_html() %>% 
      html_nodes(".HeaderNews-type_5 .Link-isFullCard") %>% html_attr('href')
      # html_elements(., ".HeaderNews-type_5 .Link-isFullCard")
    
    links_RT_search <- append(links_RT_search, new_links)
    
    # prepare vector with list names:
      # list_name <- paste0("week",startdate,"-",enddate)
      # list_names <- append(list_names, list_name)
    
    # ToDo: add option to suppress output: 
    print(paste("period", startdate, "to", enddate, "completed. Setting period to:"))
    
    # go back one period
    
    enddate <- enddate %>% 
      add_with_rollback(., days(rollback_length))
    startdate <- startdate %>% 
      add_with_rollback(., days(rollback_length))
    
    # ToDo: add option to suppress output: 
    print(paste(startdate, "to", enddate))
  }
  
  # ToDo: opt in option for named list
  # names(links_RT_search) <- list_names
  
  # ToDo: add option to select format (df, list, xml_nodeset, ...)
  
  # ToDo: add option to convert to full dataset(?)
  # by calling junctions get_pages_RT and extract_variables_rt, so can also be done in multiple steps with additional options (default)
  
  return(links_RT_search)
}

# identify if still "Weiter" button!
  # try with period = 1

### apply function ####

# set parameter:
# select startdate (here: 01 Jan 2021)
startdate <- "01-01-2021" 
# select time interval (here: 2 to keep small as possible)
period_end <- "01-01-2022"
# keyword & interval as default

# links_RT_search <- scrapeRT(startdate, period_end, interval_length = 2)
links_RT_search_finer <- scrapeRT(startdate, period_end, interval_length = 1)

# candidates
links_RT_search_Baerbock_new <- scrapeRT(startdate, period_end, keyword = "Baerbock", interval_length = 25, sleepmin = 5, sleepmax = 7)
# got 419/422 (soll 408 in 2021)
links_RT_search_Laschet_new <- scrapeRT(startdate, period_end, keyword = "Laschet", interval_length = 25, sleepmin = 5, sleepmax = 7)
# got 390/413 (soll 409 in 2021)
links_RT_search_Scholz_new <- scrapeRT(startdate, period_end, keyword = "Scholz", interval_length = 25, sleepmin = 5, sleepmax = 7)
# got 439/471 (soll 447 in 2021) 
links_RT_search_finest_Baerbock <- scrapeRT(startdate, period_end, keyword = "Baerbock", interval_length = 0, sleepmin = 2, folder = "RT_searches_candidates_finest")
links_RT_search_finest_Laschet <- scrapeRT(startdate, period_end, keyword = "Laschet", interval_length = 0, sleepmin = 2, folder = "RT_searches_candidates_finest")
links_RT_search_finest_Scholz <- scrapeRT(startdate, period_end, keyword = "Scholz", interval_length = 0, sleepmin = 2, folder = "RT_searches_candidates_finest")


# RT_searches_candidates_finest <- map_dfr(.x = candidate, cbind(scrapeRT(startdate, period_end, .x, 0, folder = "RT_searches_candidates_finest"), candidate = .x))

# check on 12-03/12-04 

### ToDo: FIND OUT WHICH ARE MISSING!!! ####

  # search all search result files for "Weiter-Button"
  # get all 68 missing articles

  # convert to df


base_url_rt <- "https://de.rt.com"

candidate_links <- c(links_RT_search_Baerbock_new, links_RT_search_Laschet_new, links_RT_search_Scholz_new) %>% tibble() %>% 
  distinct() %>% 
  mutate(links = paste0(base_url_rt, .))

write_xlsx(candidate_links)

# bind_rows(tibble(), tibble()) gives 3 vars with NAs for all rows of other vars... could be useful!

# look for remaining "Weiter" Buttons!
# need to filter for year!

save.image("all_data.RData")

# save list:
# library(rlist)
# rlist::list.save(links_RT_search, 'Articles/rt_links.rds')




# identical(links_RT_search, 'Articles/rt_links.rds')



## get page source ####
# with function:
{
  get_pages_RT <- function(link){
  # get page resource
  
  remDr$navigate(link)
  
  randsleep <- sample(seq(sleepmin, sleepmax, by = 0.001), 1)
  Sys.sleep(randsleep)
  
  html_page <- remDr$getPageSource()[[1]]
  
  # get variables:
  article_vars <- extract_variables_rt(doc = html_page) %>% 
    bind_cols(link = link, html_page = html_page)
  
  return(article_vars)
}

# data_RT <- map_dfr(links_RT$links_full, get_pages_RT)
# remDr$close()

# data_RT %<>% left_join(., links_RT, by = c("link" = "links_full"))

# # save dataset
 
# write_csv(data_RT, 'Articles/data_rt.csv')
# rio::export(data_RT, "Articles/data_rt.rds")
# 
# # import again:
# data_RT1 <- rio::import("articles/data_rt.rds")
# data_RT2 <- rio::import("articles/data_rt.csv") # seems preferable, but larger
}

# better: loop to save txt
{
# start server

for (i in 1:length(candidate_links$links)) {
  while (remDr$getCurrentUrl() != candidate_links$links[i]) {
    
    remDr$navigate(candidate_links$links[i])
    
    randsleep <- sample(seq(sleepmin, sleepmax, by = 0.001), 1)
    Sys.sleep(randsleep)
    
    page <- remDr$getPageSource()[[1]]
    
    writeLines(page, paste0("articles/RThtml/RTarticle", i, ".txt"), useBytes = T)
    
  }
}

  }
