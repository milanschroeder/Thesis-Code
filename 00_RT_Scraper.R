# SCRAPER FOR RT DE

# make sure to avoid blocking by (via DNS/IP)

## function to scrape RT DE search results: ####
# by search queries directly from RT
scrapeRT <- function(period_start = "04-09-2014", 
                     period_end = format(Sys.time(), "%d-%m-%Y"), 
                     # startdate & period_end may be anything convertable by lubridate::dmy(), e.g. a string in the format "dd-mm-yyyy"
                     
                     keyword = "mehr", # "mehr" is used as default keyword as it appears in both the recommended article line and the info banner on blocked access
                     # keyword is not case sensitive (special characters are allowed but most won't yield any results)
                     
                     interval_length = 2L,
                     # variation of search interval length can lead to slightly different results due to search result presentation 
                     # interval length must be (coercible to) integer (else default is used)
                     
                     sleepmin = 1,
                     sleepmax = 3,
                     
                     full_scrape = F, # should all available details be saved? (default: only links)
                     folder = "RT_searches" # custom folder to save .txt with search results (allows to scrape date, title, lead, thumbnail, etc. later)
){
      
  # load required packages
  # ToDO: add installer and check 
  library(tidyverse)
  library(lubridate)
  library(RSelenium)
  library(rlist)
  library(rvest)
  
  # Check input
  interval_length <- if_else(
    is.na(as.integer(interval_length)), 
    2L,
    as.integer(abs(interval_length)),
    2L
  )
  
  rollback_length <- ifelse(interval_length == 0,
                            1,
                            interval_length)
  
  sleepmin <- if_else(
    is.na(as.numeric(sleepmin)) | sleepmin < 0, 
    1,
    as.numeric(abs(sleepmin)),
    1
  )
  
  sleepmax <- if_else(
    is.na(as.numeric(sleepmax)) | sleepmax < sleepmin, 
    3,
    as.numeric(abs(sleepmax)),
    3
  )
  
  startdate <- if_else(is.na(lubridate::dmy(period_start)),
                       lubridate::dmy("04-09-2014"),
                       lubridate::dmy(period_start),
                       lubridate::dmy("04-09-2014")
  )
  
  period_end <- if_else(is.na(lubridate::dmy(period_end)),
                        lubridate::dmy(format(Sys.time(), "%d-%m-%Y")),
                        lubridate::dmy(period_end),
                        lubridate::dmy(format(Sys.time(), "%d-%m-%Y"))
  )
  
  roll_period <- function(date){
    date %>% 
      lubridate::add_with_rollback(., lubridate::days(rollback_length))
  }
  
  enddate <- startdate %>% roll_period() 
  
  # just to save with meaningful names:
  searchterm <- if_else(keyword == "mehr",
                        "",
                        keyword,
                        "")
  
  
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
  
  # be careful not to get blocked:
  # probably better don't include this as an option(?)
  max_attempts = 3
  
  # ToDo: only if option (still missing) named == T
  # list_names <- c()
  
  # create empty vector to write to 
  links_RT_search <- c()
  if (full_scrape == T) {
    RT_thumbnail_links <- c()
    RT_header <- c()
    RT_lead <- c()
    # RT_articles_date <- c() 
  }
  
  while (startdate <= period_end) {
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
    page_html <- page %>% read_html()
    
    # check if folder exists, else create:
    if (!dir.exists(folder)) {
      dir.create(folder)
    }
    # save search results as txt:
    if (interval_length == 0) {
      writeLines(page, paste0(folder, "/", searchterm, startdate, ".txt"), useBytes = T)
    } else {
      writeLines(page, paste0(folder, "/", searchterm, startdate, "-", enddate, ".txt"), useBytes = T)
    }
    
    
    # safe vector of links per interval:
    new_links <- page_html %>% 
      html_nodes(".HeaderNews-type_5 .Link-isFullCard") %>% html_attr('href')
   
    links_RT_search <- append(links_RT_search, new_links)
   
    # ToDo: secure length is identical
     if (full_scrape == T) {
       new_header <- page_html %>%
       html_elements(., ".HeaderNews-type_5 .Link-isFullCard") %>% html_text2()
       
       new_lead <- page_html %>%
         html_elements(., ".SimpleCard-summary") %>% html_text2()
       
       new_thumbnails <- page_html %>%
         html_nodes(., ".Picture-root .Picture-default") %>% html_attr("src")
       
      # new_date <- ...
      
      RT_thumbnail_links <- append(RT_thumbnail_links, new_thumbnails)
      RT_header <- append(RT_header, new_header)
      RT_lead <- append(RT_lead, new_lead)
      # RT_articles_date <- append(RT_articles_date, new_dates)
    }
    
    
    # prepare vector with list names:
    # list_name <- paste0("week",startdate,"-",enddate)
    # list_names <- append(list_names, list_name)
    
    # ToDo: add option to suppress output: 
    print(paste("period", startdate, "to", enddate, "completed. Setting period to:"))
    
    # go back one period
    enddate <- enddate %>% roll_period()
    startdate <- startdate %>% roll_period()
    
    # ToDo: add option to suppress output: 
    print(paste(startdate, "to", enddate))
  }
  
  # ToDo: opt in option for named list
    # names(links_RT_search) <- list_names
  
  # ToDo: add option to select more formats (vector, df, matrix (using cbind), list, ...)

  cat(paste("Scraping of period", period_start, "to ", period_end, "completed.\n", length(links_RT_search), "articles were found."))
  
  if (full_scrape == T) {
    df_searchresults = tibble(
      links_RT_search,
      RT_thumbnail_links,
      RT_header,
      RT_lead
      # , RT_articles_date
    )
    return(df_searchresults)
    } else{
      return(links_RT_search)
  }  
}
  
## get page sources of articles ####
# with function:
# {
#   get_pages_RT <- function(link, 
#                            sleepmin = 1,
#                            sleepmax = 3
#   ){
#     sleepmin <- if_else(
#       is.na(as.numeric(sleepmin)) | sleepmin < 0, 
#       1,
#       as.numeric(abs(sleepmin)),
#       1
#     )
#     sleepmax <- if_else(
#       is.na(as.numeric(sleepmax)) | sleepmax < sleepmin, 
#       3,
#       as.numeric(abs(sleepmax)),
#       3
#     )
#     
#     remDr$navigate(link)
#     
#     randsleep <- sample(seq(sleepmin, sleepmax, by = 0.001), 1)
#     Sys.sleep(randsleep)
#     
#     html_page <- remDr$getPageSource()[[1]]
#     
#     # get variables:
#     article_vars <- extract_variables_rt(doc = html_page) %>% 
#       bind_cols(link = link, html_page = html_page)
#     
#     return(article_vars)
#   }
#   
# 
# }

# better: loop to save article txt ####

# {
#   # start server
#   
#   for (i in 1:length(candidate_links$links)) {
#     while (remDr$getCurrentUrl() != candidate_links$links[i]) {
#       
#       remDr$navigate(candidate_links$links[i])
#       
#       randsleep <- sample(seq(sleepmin, sleepmax, by = 0.001), 1)
#       Sys.sleep(randsleep)
#       
#       page <- remDr$getPageSource()[[1]]
#       
#       writeLines(page, paste0("articles/RThtml/RTarticle", i, ".txt"), useBytes = T)
#       
#     }
#   }
#   
# }


# scrape articles from saved txt: 

scrape_articles <- function(linklist, 
                            sleepmin = 1,
                            sleepmax = 3,
                            folder = "RT_searches",
                            subfolder = "articles",
                            baseURL = "de.rt.com"){
  
  library(tidyverse)
  # library(lubridate)
  library(RSelenium)
  library(rvest)
  
  sleepmin <- if_else(
    is.na(as.numeric(sleepmin)) | sleepmin < 0, 
    1,
    as.numeric(abs(sleepmin)),
    1
  )
  sleepmax <- if_else(
    is.na(as.numeric(sleepmax)) | sleepmax < sleepmin, 
    3,
    as.numeric(abs(sleepmax)),
    3
  )
  
  RT_articles <- tibble()
  
  art_no = 0
  
  
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
  
  read_page_data <- function(doc = doc, 
                             folder = folder,
                             subfolder = subfolder,
                             full_link = full_link){
    page_data <- tibble(
      header = doc %>% html_elements(".HeadLine-type_2") %>% html_text2(), 
      # or use header = doc %>% html_elements(xpath = "//meta[@property='og:title']") %>% html_attr("content")
      lead = doc %>% html_element(".p-24 .Text-type_1") %>% html_text2(), 
      # or use lead = doc %>% html_elements(xpath = "//meta[@name='description']") %>% html_attr("content")
      
      text = doc %>% html_element(".ViewText-root") %>% html_text2(),
      link = full_link,
      category = doc %>% html_element(".Breadcrumbs-arrow+ .Breadcrumbs-item span") %>% html_text2(),
      # date = doc %>% html_element(".Timestamp-default") %>% html_text2(), 
      # should be identical but who knows...
      date_time = doc %>% html_elements(xpath = "//meta[@name='publish-date']") %>% html_attr("content"),
      intext_links = list(doc %>% html_nodes(".ViewText-root a") %>% html_attr("href")),
      recommended_link = list(doc %>% html_nodes("strong+ a") %>% html_attr("href")),
      tags = list(doc %>% html_elements(".Tags-link") %>% html_text2()),
      tags_links = list(doc %>% html_nodes(".Tags-link") %>% html_attr("href")),
      image_main = list(doc %>% html_nodes(".lazyloaded") %>% html_attr("src")),
      image_main_caption = list(doc %>% html_element(".Cover-caption") %>% html_text2()),
      image_rights = list(doc %>% html_element(".Cover-imageSource") %>% html_text2()),
      multimedia_links = list(doc %>% html_nodes(".EmbedBlock-root") %>% html_attr("href")),
      author = doc %>% html_element("p:nth-child(1) em") %>% html_text2(),
      savedoc = paste0(folder, "/", subfolder, "/article", art_no, ".txt")
    )
    return(page_data)
  }
  
  for (link in linklist) {
      
    art_no = art_no + 1
    
    full_link <- paste0(baseURL, link)
    remDr$navigate(full_link)
    
    randsleep <- sample(seq(sleepmin, sleepmax, by = 0.001), 1)
    Sys.sleep(randsleep)
    
    html_page <- remDr$getPageSource()[[1]] 
    doc <- html_page %>% read_html()
    
    # check if folder exists, else create:
    if (!dir.exists(paste0(folder, "/", subfolder))) {
      dir.create(paste0(folder, "/", subfolder))
    }
    # save pages as .txt(in case sth breaks:
      writeLines(html_page, paste0(folder, "/", subfolder, "/article", art_no, ".txt"), useBytes = T)
    
    # ToDo: from saved files alternatively:
      # for (article in dir(paste0(folder, "/articles"))) {
      #   doc <- readr::read_file(paste0(folder, "articles/", article)) %>% 
      #     read_html(encoding = "UTF-8")
      
      if (art_no > 1) {
        df_new <- read_page_data()
        RT_articles <- RT_articles %>% add_row(., df_new)
      } else {
          RT_articles <- read_page_data()
        }
      }  
      
    return(RT_articles)    
}
