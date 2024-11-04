# SCRAPER FOR RT DE

### DEPRECATED VERSION BASED ON SEARCH FUNCTION & R-SELENIUM ###


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
                     
                     sleepmin = 0.5,
                     sleepmax = 1.5,
                     max_attempts = 2, # attempts to load more on page
                     
                     full_scrape = F, # should all available details be saved? (default: only links)
                     folder = "RT_searches", # custom folder to save .txt with search results (allows to scrape date, title, lead, thumbnail, etc. later)
                     subfolder = "searches"
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
  
  # be careful not to get blocked:
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
  
  # randsleep <- function(sleepmin = sleepmin, sleepmax = sleepmax){
    # sleeptime <- sample(seq(sleepmin, sleepmax, by = 0.001), 1)
    # Sys.sleep(sleeptime)
  # } 
  
  startdate <- if_else(is.na(lubridate::dmy(period_start)),
                       format(dmy("04-09-2014"), "%d-%m-%Y"),
                       format(dmy(period_start), "%d-%m-%Y"),
                       format(dmy("04-09-2014"), "%d-%m-%Y")
                       )
  
  period_end <- if_else(is.na(lubridate::dmy(period_end)),
                        format(dmy(Sys.time()), "%d-%m-%Y"),
                        format(dmy(period_end), "%d-%m-%Y"),
                        format(dmy(Sys.time()), "%d-%m-%Y")
                        )
  
  roll_period <- function(date){
    # input must be format dmy
    dmy(date) %>% 
      lubridate::add_with_rollback(., lubridate::days(rollback_length)) %>% 
      format(., "%d-%m-%Y")
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
  
  while (dmy(startdate) <= dmy(period_end)) {
    # loop over search URLs by time interval:
    search <- paste0("https://de.rt.com/search?q=", keyword ,"&df=", startdate, "&dt=", enddate)  
    
    while(remDr$getCurrentUrl() != search){
      remDr$navigate(search)
    }
    
    attempts <- 0
    while(attempts < max_attempts) {
      
      webElem1 <- NULL
      webElem2 <- NULL
      attempts <- 0
      
      
      # function to scroll to bottom
      scroll_down <- function(){
        webElemEnd <- NULL
        while (is.null(webElemEnd)) {
          webElemEnd <- tryCatch({remDr$findElement("css", "body")},
                                 error = function(e){NULL})
          try(webElemEnd$sendKeysToElement(list(key = "end")))
        }
      }
      
      
      while( # is.null(webElem1) & is.null(webElem2) & 
            attempts < max_attempts){
        
        scroll_down()
        
        pages_source_compare <- remDr$getPageSource()[[1]]
        
        # search for "Weiter" button and click on it
        #  while(is.null(webElem1) & is.null(webElem2) & attempts < max_attempts){
        tryCatch({webElem1 <- remDr$findElement(using = "css selector", ".Button_dark")},
                 error = function(e){NULL})
        
        try(webElem1$clickElement())
        
        # # check if still on right page:
        if(remDr$getCurrentUrl() != search){
          remDr$navigate(search)
          attempts <- 0
          sleeptime <- sample(seq(sleepmin, sleepmax, by = 0.001), 1)
          Sys.sleep(sleeptime)
        }
  #      randsleep()
        
        scroll_down()
        
        tryCatch({webElem2 <- remDr$findElement(using = "css selector", ".Button_dark")},
                 error = function(e){NULL})
        try(webElem2$clickElement())
        
        sleeptime <- sample(seq(sleepmin, sleepmax, by = 0.001), 1)
        Sys.sleep(sleeptime)
        
        # # check if still on right page:
        if(remDr$getCurrentUrl() != search){
          remDr$navigate(search)
          attempts <- 0
        }
        
     # Page source changes if more to load
      #  check if sth still changes:
        
        pages_source <- remDr$getPageSource()[[1]]
        if (pages_source == pages_source_compare){
          attempts <- attempts + 1
         }
         
        # # sleep to avoid blocking
        # sleeptime <- sample(seq(sleepmin, sleepmax, by = 0.001), 1)
        # Sys.sleep(sleeptime)
        
      }
      
    }
    
    # save and overwrite with every loop, in case sth breaks
    # ToDo: test if this works within new_links assignment pipe as well
    # html_search <- remDr$getPageSource()[[1]] 
    
    # print(paste("search page successfully saved after", i, " clicks"))
    page <- remDr$getPageSource()[[1]]
    page_html <- page %>% read_html()
    
    # check if folder exists, else create:
    if (!dir.exists(paste0(folder, "/", subfolder))) {
      dir.create(paste0(folder, "/", subfolder))
    }
    # save search results as txt:
    if (interval_length == 0) {
      writeLines(page, paste0(folder, "/", subfolder, "/", searchterm, startdate, ".txt"), useBytes = T)
    } else {
      writeLines(page, paste0(folder, "/", subfolder, "/", searchterm, startdate, "-", enddate, ".txt"), useBytes = T)
    }
    
    
    # safe vector of links per interval:
    new_links <- page_html %>% 
      html_nodes(".HeaderNews-type_5 .Link-isFullCard") %>% html_attr('href')

    links_RT_search <- append(links_RT_search, new_links)
    
    # ToDo: secure length is identical
     if (full_scrape == T) {
       # how to make sure everything has same length?
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
      links_RT_search = list(tibble(links_RT_search)),
      RT_thumbnail_links = list(tibble(RT_thumbnail_links)),
      RT_header = list(tibble(RT_header)),
      RT_lead = list(tibble(RT_lead))
      # , RT_articles_date
    )
    return(df_searchresults)
    } else{
      return(links_RT_search)
    }  
  remDr$close()
}
  
# scrape from saved files ####
scrape_searches_from_saved <- function(
    full_scrape = F, # should all available details be saved? (default: only links)
    folder = "RT_searches", # custom folder to save .txt with search results (allows to scrape date, title, lead, thumbnail, etc. later)
    subfolder = "searches"
){
  
  # load required packages
  # ToDO: add installer and check 
  library(readr)
  library(tidyverse)
  library(lubridate)
  library(rvest)
  
  # create empty vector to write to 
  links_RT_search <- c()
  if (full_scrape == T) {
    RT_thumbnail_links <- c()
    RT_header <- c()
    RT_lead <- c()
    # RT_articles_date <- c() 
    files <- c()
  }
  
  file_path <- paste0(folder, "/", subfolder)
  
  for (file in dir(file_path)) {
    # to test:
    # print(file) }
    
    page_html <- readr::read_file(paste0(file_path, "/", file)) %>% 
      read_html(encoding = "UTF-8")
    
    # safe vector of links per interval:
    new_links <- page_html %>% 
      html_nodes(".HeaderNews-type_5 .Link-isFullCard") %>% html_attr('href')
    
    links_RT_search <- append(links_RT_search, new_links)
    
    # ToDo: secure length is identical
    if (full_scrape == T) {
      # how to make sure everything has same length?
      new_header <- page_html %>%
        html_elements(., ".HeaderNews-type_5 .Link-isFullCard") %>% html_text2()
      
      new_lead <- page_html %>%
        html_elements(., ".SimpleCard-summary") %>% html_text2()
      
      new_thumbnails <- page_html %>%
        html_nodes(., ".Picture-root .Picture-default") %>% html_attr("src")
      
      new_file <- rep(files, length.out = length(new_links))
      
      # new_date <- ...
      
      RT_thumbnail_links <- append(RT_thumbnail_links, new_thumbnails)
      RT_header <- append(RT_header, new_header)
      RT_lead <- append(RT_lead, new_lead)
      # RT_articles_date <- append(RT_articles_date, new_dates)
      files <- append(file, new_file)
    }
  }
  if (full_scrape == T) {
    df_searchresults = tibble(
      links_RT_search = list(tibble(links_RT_search)),
      RT_thumbnail_links = list(tibble(RT_thumbnail_links)),
      RT_header = list(tibble(RT_header)),
      RT_lead = list(tibble(RT_lead)),
      # , RT_articles_date
      file = list(tibble(files))
    )
    return(df_searchresults)
  } else{
    return(links_RT_search)
    }
}

# scrape articles: ####

scrape_articles <- function(linklist, 
                            sleepmin = 0.5,
                            sleepmax = 1.5,
                            folder = "RT_searches",
                            subfolder = "articles",
                            baseURL = "https://de.rt.com",
                            lists_as_character = F, # gives nicer NAs, but looses nested structure. Date formats are preserved.
                            nested = F, # want a nested df instead of lists/c(""), -> deprecated
                            begin_with_article = 0
                            ){
  
  library(tidyverse)
  library(lubridate)
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
  
  # randsleep <- function(sleepmin = sleepmin, sleepmax = sleepmax){
  #   sleeptime <- sample(seq(sleepmin, sleepmax, by = 0.001), 1)
  #   Sys.sleep(sleeptime)
  # } 
  
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
  
# ToDO: Find out while this does not work with a function:  
  # read_page_data <- function(doc = doc, 
  #                            folder = folder,
  #                            subfolder = subfolder,
  #                            full_link = full_link){
  #   page_data <- tibble(
  #     header = doc %>% html_elements(., ".HeadLine-type_2") %>% html_text2(), 
  #     # or use header = doc %>% html_elements(xpath = "//meta[@property='og:title']") %>% html_attr("content")
  #     lead = doc %>% html_element(".p-24 .Text-type_1") %>% html_text2(), 
  #     # or use lead = doc %>% html_elements(xpath = "//meta[@name='description']") %>% html_attr("content")
  #     
  #     text = doc %>% html_element(".ViewText-root") %>% html_text2(),
  #     link = full_link,
  #     category = doc %>% html_element(".Breadcrumbs-arrow+ .Breadcrumbs-item span") %>% html_text2(),
  #     # date = doc %>% html_element(".Timestamp-default") %>% html_text2(), 
  #     # should be identical but who knows...
  #     date_time = doc %>% html_elements(xpath = "//meta[@name='publish-date']") %>% html_attr("content"),
  #     intext_links = list(doc %>% html_nodes(".ViewText-root a") %>% html_attr("href")),
  #     recommended_link = list(doc %>% html_nodes("strong+ a") %>% html_attr("href")),
  #     tags = list(doc %>% html_elements(".Tags-link") %>% html_text2()),
  #     tags_links = list(doc %>% html_nodes(".Tags-link") %>% html_attr("href")),
  #     image_main = list(doc %>% html_nodes(".lazyloaded") %>% html_attr("src")),
  #     image_main_caption = list(doc %>% html_element(".Cover-caption") %>% html_text2()),
  #     image_rights = list(doc %>% html_element(".Cover-imageSource") %>% html_text2()),
  #     multimedia_links = list(doc %>% html_nodes(".EmbedBlock-root") %>% html_attr("href")),
  #     author = doc %>% html_element("p:nth-child(1) em") %>% html_text2() %>% stringr::str_remove(., pattern = "[v]on|[V]on "),
  #     savedoc = paste0(folder, "/", subfolder, "/article", art_no, ".txt")
  #   )
  #  return(page_data)
  # }
  # 
  
 # RT_articles <- tibble()
  art_no <- begin_with_article - 1
  
  for (link in linklist) {
      
    art_no <- art_no + 1
    
    full_link <- paste0(baseURL, link)
    remDr$navigate(full_link)
    
    # ToDo?: add scrolldown (jsut do be sure everything is loaded)
    
    sleeptime <- sample(seq(sleepmin, sleepmax, by = 0.001), 1)
    Sys.sleep(sleeptime)
    
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
      
  # save data:
    if (nested == T) {
      
      # save data:
      page_data <- tibble(
        header = doc %>% html_elements(., ".HeadLine-type_2") %>% html_text2() %>% stringr::str_c(., collapse = " "), 
        # or use header = doc %>% html_elements(xpath = "//meta[@property='og:title']") %>% html_attr("content")
        link = full_link,
        
        author = doc %>% html_element("p:nth-child(1) em") %>% html_text2() %>% stringr::str_remove(., pattern = "[v]on|[V]on "), # test with more articles
        # date = doc %>% html_element(".Timestamp-default") %>% html_text2(), 
        # should be identical but who knows...
        date_time = doc %>% html_elements(xpath = "//meta[@name='publish-date']") %>% html_attr("content") %>% lubridate::ymd_hms(),
        date = as.Date(date_time),
        
        category = doc %>% html_element(".Breadcrumbs-arrow+ .Breadcrumbs-item span") %>% html_text2(),
        
        category_full = list(tibble(
          category_full_link = stringr::str_c(collapse = ";", 
                                              ifelse(stringr::str_length(doc %>% html_nodes(".RTLink-root") %>% html_attr("href")) > 0,
                                                     stringr::str_c("https://de.rt.com", doc %>% html_nodes(".RTLink-root") %>% html_attr("href")),
                                                     NA)),
          category_full = stringr::str_c(collapse = ";", doc %>% html_elements(".RTLink-root span") %>% html_text2()),
        )),
        
        tags = list(tibble(
          tags_name = doc %>% html_elements(".Tags-link") %>% html_text2(),
          tags_links = ifelse(stringr::str_length(doc %>% html_nodes(".Tags-link") %>% html_attr("href"))>0,
                              stringr::str_c("https://de.rt.com", doc %>% html_nodes(".Tags-link") %>% html_attr("href")),
                              NA)
        )),
        
        lead = doc %>% html_element(".p-24 .Text-type_1") %>% html_text2(), 
        # or use lead = doc %>% html_elements(xpath = "//meta[@name='description']") %>% html_attr("content")
        text_article = stringr::str_c(doc %>% html_elements(".ViewText-root p") %>% html_text2(), collapse = " "), # pure article text
        text_all = stringr::str_c(doc %>% html_elements(".ViewText-root") %>% html_text2() %>%  # all text including intextlinks & embeddings
                                    stringr::str_remove_all(., "noscript pattern"),
                                  collapse = " "),
        
        intext_links = list(tibble(intext_links = doc %>% html_nodes(".ViewText-root a") %>% html_attr("href"))),
        
        recommendation = list(tibble(
          recommendation_main = list(tibble(
            recommendation_main_link = list(tibble(recommendation_main_link = doc %>% html_nodes("strong+ a") %>% html_attr("href"))),
            recommendation_main_title = list(tibble(recommendation_main_title = doc %>% html_nodes("strong+ a") %>% html_text2())),
          )),
          recommendation_embedded = list(tibble(
            recommendation_embedded_thumbnail = list(tibble(recommendation_embedded_thumbnail = doc %>% html_elements(".ReadMore-root .Picture-root img") %>% html_attr("data-src"))),
            recommendation_embedded_title = list(tibble(recommendation_embedded_title = doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_text2())),
            recommendation_embedded_link = list(tibble(ifelse(stringr::str_length(doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_attr("href")) > 0,
                                                              stringr::str_c("https://de.rt.com", doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_attr("href")),
                                                              NA)
                                                       ))
          ))
        )),  
        
        image = list(tibble(
          image_src = 
           list(tibble(
             image_src = doc %>% html_nodes(".lazyloaded") %>% html_attr("src") # could be more stable (e.g. using .Cover-root)?
           )),
          image_caption =
           list(tibble(
          image_caption = doc %>% html_elements(".Cover-caption") %>% html_text2()
           )),
          image_rights = 
           list(tibble(
          image_rights = doc %>% html_elements(".Cover-imageSource") %>% html_text2()
           ))
          # multimedia_links = list(doc %>% html_nodes(".EmbedBlock-root") %>% html_attr("href")),
        )),
        
        # Embeddings:  
        embeddings = list(tibble(
          
          embeddings_yt_link = list(tibble(embeddings_yt_link = ifelse(stringr::str_length(doc %>% html_elements(xpath = "//div[@data-rtmodule='YouTubeEmbed']") %>% html_attr("id")) > 0,
                                                                       stringr::str_c("https://www.youtube.com/watch?v=", doc %>% html_elements(xpath = "//div[@data-rtmodule='YouTubeEmbed']") %>% html_attr("id")),
                                                                       NA))),
          # The following is loaded lazy, hence would need additional clicks to save.
          # embeddings_yt_cover = list(doc %>% html_elements(xpath = "//div[@class='YouTubeEmbed-cover']/picture/source") %>% html_attr("srcset")), 
          
          # embeddings_twitter = list(doc %>% html_nodes(".TwitterEmbed") %>% html_attr("src")), # only for newer twitter embeddings
          embeddings_twitter  = list(tibble(
            embeddings_twitter_link = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']/*/a") %>% html_attr("href"),
            embeddings_twitter_date = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']/*/a") %>% html_text2() %>% lubridate::parse_date_time(., "%B %d, %Y") %>% as.character(),
            embeddings_twitter_text = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']") %>% html_text2(),
          )),
          
          # embeddings_all = list(doc %>% html_nodes(".EmbedBlock-root") %>% html_attr("src")),
          embeddings_other_link = list(tibble(embeddings_other_link = doc %>% html_elements(xpath = "//ancestor-or-self::*[@class='AllEmbed']//*") %>% html_attr("src"))) # including commentary section etc.
          
        )),
        savedoc = paste0(folder, "/", subfolder, "/article", art_no, ".txt")
      )
              
      
  # nesting afterwards: 
  
  #   page_data <- page_data %>%    
  #   nest(.,
  #         tags = c(tags_name, tags_links),
  #   #      recommendations = c(
  #           recommendations_main = c(recommendation_main_link, recommendation_main_title),
  #           recommendations_embedded = c(recommendation_embedded_thumbnail, recommendation_embedded_link, recommendation_embedded_title),
  #    #     ),
  #         images = c(image_src, image_caption, image_rights),
  # #        embeddings = c(
  #           embeddings_yt = c(embeddings_yt_link),
  #           embeddings_twitter = c(embeddings_twitter_link, embeddings_twitter_date, embeddings_twitter_text),
  #           embeddings_other = c(embeddings_other_link)
  #  #         )
  #         ) %>% 
  #       nest(.,
  #            recommendations = c(recommendations_main, recommendations_embedded),
  #            embeddings = c(embeddings_yt, embeddings_twitter, embeddings_other)
  #            )
  
    } else {
      
      page_data <- tibble(
        header = doc %>% html_elements(., ".HeadLine-type_2") %>% html_text2() %>% stringr::str_c(., collapse = " "), 
        # or use header = doc %>% html_elements(xpath = "//meta[@property='og:title']") %>% html_attr("content")
        link = full_link,
        
        category = doc %>% html_element(".Breadcrumbs-arrow+ .Breadcrumbs-item span") %>% html_text2(),
        # category_link = stringr::str_c(collapse = ";", 
        #                              ifelse(stringr::str_length(doc %>% html_nodes(".Breadcrumbs-arrow+") %>% html_attr("href")) > 0,
        #                                     stringr::str_c("https://de.rt.com", doc %>% html_nodes(".Breadcrumbs-arrow+") %>% html_attr("href")),
        #                                     NA)),
        
        category_full_link = stringr::str_c(collapse = ";", 
                                            ifelse(stringr::str_length(doc %>% html_nodes(".RTLink-root") %>% html_attr("href")) > 0,
                                                   stringr::str_c("https://de.rt.com", doc %>% html_nodes(".RTLink-root") %>% html_attr("href")),
                                                   NA)),
        category_full = stringr::str_c(collapse = ";", doc %>% html_elements(".RTLink-root span") %>% html_text2()),
        
        author = doc %>% html_element("p:nth-child(1) em") %>% html_text2() %>% stringr::str_remove(., pattern = "[v]on|[V]on "), # test with more articles
        # date = doc %>% html_element(".Timestamp-default") %>% html_text2(), 
        # should be identical but who knows...
        date_time = doc %>% html_elements(xpath = "//meta[@name='publish-date']") %>% html_attr("content") %>% lubridate::ymd_hms(),
        date = as.Date(date_time),
        
        tags_name = stringr::str_c(collapse = ";", doc %>% html_elements(".Tags-link") %>% html_text2()),
        tags_links = stringr::str_c(collapse = ";", 
                                    ifelse(stringr::str_length(doc %>% html_nodes(".Tags-link") %>% html_attr("href")) > 0,
                                           stringr::str_c("https://de.rt.com", doc %>% html_nodes(".Tags-link") %>% html_attr("href")),
                                           NA)),
        
        lead = doc %>% html_element(".p-24 .Text-type_1") %>% html_text2() %>% stringr::str_c(., collapse = " "), 
        # or use lead = doc %>% html_elements(xpath = "//meta[@name='description']") %>% html_attr("content")
        text_article = stringr::str_c(doc %>% html_elements(".ViewText-root p") %>% html_text2(), collapse = " "), # pure article text
        text_all = stringr::str_c(doc %>% html_elements(".ViewText-root") %>% html_text2() %>%  # all text including intextlinks & embeddings
                                         stringr::str_remove_all(., "noscript pattern"),
                                  collapse = " "),
        
        intext_links = stringr::str_c(collapse = ";", doc %>% html_nodes(".ViewText-root a") %>% html_attr("href")),
        
        recommendation_main_link = stringr::str_c(collapse = ";", doc %>% html_nodes("strong+ a") %>% html_attr("href")),
        recommendation_main_title = stringr::str_c(collapse = ";", doc %>% html_nodes("strong+ a") %>% html_text2()),
        recommendation_embedded_thumbnail = stringr::str_c(collapse = ";", doc %>% html_elements(".ReadMore-root .Picture-root img") %>% html_attr("data-src")),
        recommendation_embedded_title = stringr::str_c(collapse = ";", doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_text2()),
        recommendation_embedded_link = stringr::str_c(collapse = ";",
                                                      ifelse(stringr::str_length(doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_attr("href")) > 0,
                                                             stringr::str_c("https://de.rt.com", doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_attr("href")),
                                                             NA)),
        
        image_src = stringr::str_c(collapse = ";", doc %>% html_nodes(".lazyloaded") %>% html_attr("src")), # could be more stable (e.g. using .Cover-root)?
        image_caption = stringr::str_c(collapse = ";", doc %>% html_elements(".Cover-caption") %>% html_text2()),
        image_rights = stringr::str_c(collapse = ";", doc %>% html_elements(".Cover-imageSource") %>% html_text2()),
        # multimedia_links = list(doc %>% html_nodes(".EmbedBlock-root") %>% html_attr("href")),
        
        # Embeddings:  
        embeddings_yt_link = stringr::str_c(collapse = ";", 
                                            ifelse(stringr::str_length(doc %>% html_elements(xpath = "//div[@data-rtmodule='YouTubeEmbed']") %>% html_attr("id")) > 0,
                                                   stringr::str_c("https://www.youtube.com/watch?v=", doc %>% html_elements(xpath = "//div[@data-rtmodule='YouTubeEmbed']") %>% html_attr("id")),
                                                    NA)),
        # The following is loaded lazy, hence would need additional clicks to save.
        # embeddings_yt_cover = list(doc %>% html_elements(xpath = "//div[@class='YouTubeEmbed-cover']/picture/source") %>% html_attr("srcset")), 
        
        # embeddings_twitter = list(doc %>% html_nodes(".TwitterEmbed") %>% html_attr("src")), # only for newer twitter embeddings
        embeddings_twitter_link = stringr::str_c(collapse = ";", doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']/*/a") %>% html_attr("href")),
        embeddings_twitter_date = stringr::str_c(collapse = ";", doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']/*/a") %>% html_text2() %>% lubridate::parse_date_time(., "%B %d, %Y") %>% as.character()),
        embeddings_twitter_text = stringr::str_c(collapse = ";", doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']") %>% html_text2()),
        
        # embeddings_all = list(doc %>% html_nodes(".EmbedBlock-root") %>% html_attr("src")),
        embeddings_other_link = stringr::str_c(collapse = ";", doc %>% html_elements(xpath = "//ancestor-or-self::*[@class='AllEmbed']//*") %>% html_attr("src")), # including commentary section etc.
        
        savedoc = paste0(folder, "/", subfolder, "/article", art_no, ".txt")
      )
    }
    
      
      if (art_no > begin_with_article) {
        df_new <- page_data
        RT_articles <- RT_articles %>% add_row(., df_new)
      } else {
          RT_articles <- page_data
        }
    }  
  
  
  # replace character(0) with proper NA
 if (lists_as_character == T) {
   RT_articles <- RT_articles %>% 
     mutate(across(.fns = ~ as.character(.)), 
            across(.fns = ~ replace(., . == "character(0)", NA)),  
            across(.fns = ~ replace(., . == "logical(0)", NA)))
 }
  
  remDr$close()
  return(RT_articles)    
}


scrape_and_save <- function(linklist, 
                            sleepmin = 0.5,
                            sleepmax = 1.5,
                            Full_url = T,
                            folder = "rt_archive",
                            subfolder = "articles",
                            baseURL = "https://de.rt.com",
                            lists_as_character = F, # gives nicer NAs, but looses nested structure. Date formats are preserved.
                            nested = F, # want a nested df instead of lists/c(""), -> deprecated
                            begin_with_article = 0
){
  
  library(tidyverse)
  library(lubridate)
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
  
  # randsleep <- function(sleepmin = sleepmin, sleepmax = sleepmax){
  #   sleeptime <- sample(seq(sleepmin, sleepmax, by = 0.001), 1)
  #   Sys.sleep(sleeptime)
  # } 
  
  
  # RT_articles <- tibble()
  art_no <- begin_with_article - 1
  
  for (link in linklist) {
    
    art_no <- art_no + 1
    
    full_link <- ifelse(full_url == T, link, paste0(baseURL, link))
    
# add try catch and record as deleted
  try(html_page <- read_html(full_link))    
    
  if ("try-error" %in% class(html_page)) {
    
    # insert df_row for articles no longer online 
    if (nested == T) {
      
      # save data:
      page_data <- tibble(
        header = NA, 
        link = full_link,
        author = NA,
        
        date_time_utc = NA, # or get from sitemaps
        date = NA, # or get from sitemaps
        tz_original = NA, # or get from sitemaps
        
        category = NA,
        
        category_full = list(tibble(
          category_full_link = NA,
          category_full = NA,
        )),
        
        tags = list(tibble(
          tags_name = NA,
          tags_links = NA
        )),
        
        lead = NA, 
        text_article = NA,
        text_all = NA,
        
        intext_links = list(tibble(intext_links = NA)),
        
        recommendation = list(tibble(
          recommendation_main = list(tibble(
  # why this structure and not simple tibble?
            recommendation_main_link = list(tibble(NA)),
            recommendation_main_title = list(tibble(NA)),
          )),
  # why this structure and not simple tibble?
          recommendation_embedded = list(tibble(
            recommendation_embedded_thumbnail = list(tibble(NA)),
            recommendation_embedded_title = list(tibble(NA)),
            recommendation_embedded_link = list(tibble(NA))
            ))
          )),  
        
  # why this structure and not simple tibble (how to secure length?)
        image = list(tibble(
          image_src = 
            list(tibble(
              image_src = NA
            )),
          image_caption =
            list(tibble(
              image_caption = NA
            )),
          image_rights = 
            list(tibble(
              image_rights = NA
            ))
          # multimedia_links = list(doc %>% html_nodes(".EmbedBlock-root") %>% html_attr("href")),
        )),
        
        # Embeddings:  
        embeddings = list(tibble(
          
          embeddings_yt_link = list(tibble(embeddings_yt_link = NA)),
          
          embeddings_twitter  = list(tibble(
            embeddings_twitter_link = NA,
            embeddings_twitter_date = NA,
            embeddings_twitter_text = NA,
          )),
          
          # embeddings_all = list(doc %>% html_nodes(".EmbedBlock-root") %>% html_attr("src")),
          embeddings_other_link = list(tibble(embeddings_other_link = NA))
        )),
  
        savedoc = NA,
        available_online = F
      )
      
      # write to JSON:
      json_loc <- paste0(folder, "/rt_archive.json")
      
      if (file.exists(json_loc)) {
        appended <- fromJSON(json_loc) %>% 
          rbind(., page_data)
        
        write_json(appended, json_loc, pretty = T, na = "string", Date = 'ISO8601', POSIXt = 'ISO8601') 
        
      } else{
        write_json(page_data, json_loc, pretty = T, na = "string", Date = 'ISO8601', POSIXt = 'ISO8601') 
      }
      # end of write_json
    
  }else{ 
    # if page available

    doc <- toString(html_page)
    doc_hash <- rlang::hash(doc)
    save_loc <- paste0(folder, "/", subfolder, "/", doc_hash[1:2], "/", doc_hash[3:4])
  
   
    # check if folder exists, else create:
    if (!dir.exists(paste0(save_loc))) {
      dir.create(save_loc)
    }
    # save pages as .txt(in case sth breaks:
    writeLines(doc, paste0(save_loc, "/", doc_hash, ".txt"), useBytes = T)
    
    # ToDo: from saved files alternatively:
    # for (article in dir(paste0(folder, "/articles"))) {
    #   doc <- readr::read_file(paste0(folder, "articles/", article)) %>% 
    #     read_html(encoding = "UTF-8")
  
    # save data:
    if (nested == T) {
      
      # save data:
      page_data <- tibble(
        header = doc %>% html_elements(., ".HeadLine-type_2") %>% html_text2() %>% stringr::str_c(., collapse = " "), 
        # or use header = doc %>% html_elements(xpath = "//meta[@property='og:title']") %>% html_attr("content")
        link = full_link,
        
        author = doc %>% html_element("p:nth-child(1) em") %>% html_text2() %>% stringr::str_remove(., pattern = "[v]on|[V]on "), # test with more articles
        # date = doc %>% html_element(".Timestamp-default") %>% html_text2(), 
        # should be identical but who knows...
        date_time_utc = doc %>% html_element(".Timestamp-default") %>% html_attr("datetime") %>% lubridate::ymd_hms(., tz = "UTC") %>% as.character(),
        date = as.Date(date_time_utc %>% lubridate::ymd_hms(tz = "UTC")) %>% as.character(),
        tz_original = doc %>% html_element(".Timestamp-default") %>% html_attr("datetime") %>% str_sub(start = -6),
        
        category = doc %>% html_element(".Breadcrumbs-arrow+ .Breadcrumbs-item span") %>% html_text2(),
        
        category_full = list(tibble(
          category_full_link = stringr::str_c(collapse = ";", 
                                              ifelse(stringr::str_length(doc %>% html_nodes(".RTLink-root") %>% html_attr("href")) > 0,
                                                     stringr::str_c("https://de.rt.com", doc %>% html_nodes(".RTLink-root") %>% html_attr("href")),
                                                     NA)),
          category_full = stringr::str_c(collapse = ";", doc %>% html_elements(".RTLink-root span") %>% html_text2()),
        )),
        
        tags = list(tibble(
          tags_name = doc %>% html_elements(".Tags-link") %>% html_text2(),
          tags_links = ifelse(stringr::str_length(doc %>% html_nodes(".Tags-link") %>% html_attr("href"))>0,
                              stringr::str_c("https://de.rt.com", doc %>% html_nodes(".Tags-link") %>% html_attr("href")),
                              NA)
        )),
        
        lead = doc %>% html_element(".p-24 .Text-type_1") %>% html_text2(), 
        # or use lead = doc %>% html_elements(xpath = "//meta[@name='description']") %>% html_attr("content")
        text_article = stringr::str_c(doc %>% html_elements(".ViewText-root p") %>% html_text2(), collapse = " "), # pure article text
        text_all = stringr::str_c(doc %>% html_elements(".ViewText-root") %>% html_text2() %>%  # all text including intextlinks & embeddings
                                    stringr::str_remove_all(., "noscript pattern"),
                                  collapse = " "),
        
        intext_links = list(tibble(intext_links = doc %>% html_nodes(".ViewText-root a") %>% html_attr("href"))),
        
        recommendation = list(tibble(
          recommendation_main = list(tibble(
            recommendation_main_link = list(tibble(recommendation_main_link = doc %>% html_nodes("strong+ a") %>% html_attr("href"))),
            recommendation_main_title = list(tibble(recommendation_main_title = doc %>% html_nodes("strong+ a") %>% html_text2())),
          )),
          recommendation_embedded = list(tibble(
            recommendation_embedded_thumbnail = list(tibble(recommendation_embedded_thumbnail = doc %>% html_elements(".ReadMore-root .Picture-root img") %>% html_attr("data-src"))),
            recommendation_embedded_title = list(tibble(recommendation_embedded_title = doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_text2())),
            recommendation_embedded_link = list(tibble(ifelse(stringr::str_length(doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_attr("href")) > 0,
                                                              stringr::str_c("https://de.rt.com", doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_attr("href")),
                                                              NA)
            ))
          ))
        )),  
        
        image = list(tibble(
          image_src = 
            list(tibble(
              image_src = doc %>% html_nodes(".lazyloaded") %>% html_attr("src") # could be more stable (e.g. using .Cover-root)?
            )),
          image_caption =
            list(tibble(
              image_caption = doc %>% html_elements(".Cover-caption") %>% html_text2()
            )),
          image_rights = 
            list(tibble(
              image_rights = doc %>% html_elements(".Cover-imageSource") %>% html_text2()
            ))
          # multimedia_links = list(doc %>% html_nodes(".EmbedBlock-root") %>% html_attr("href")),
        )),
        
        # Embeddings:  
        embeddings = list(tibble(
          
          embeddings_yt_link = list(tibble(embeddings_yt_link = ifelse(stringr::str_length(doc %>% html_elements(xpath = "//div[@data-rtmodule='YouTubeEmbed']") %>% html_attr("id")) > 0,
                                                                       stringr::str_c("https://www.youtube.com/watch?v=", doc %>% html_elements(xpath = "//div[@data-rtmodule='YouTubeEmbed']") %>% html_attr("id")),
                                                                       NA))),
          # The following is loaded lazy, hence would need additional clicks to save.
          # embeddings_yt_cover = list(doc %>% html_elements(xpath = "//div[@class='YouTubeEmbed-cover']/picture/source") %>% html_attr("srcset")), 
          
          # embeddings_twitter = list(doc %>% html_nodes(".TwitterEmbed") %>% html_attr("src")), # only for newer twitter embeddings
          embeddings_twitter  = list(tibble(
            embeddings_twitter_link = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']/*/a") %>% html_attr("href"),
            embeddings_twitter_date = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']/*/a") %>% html_text2() %>% lubridate::parse_date_time(., "%B %d, %Y") %>% as.character(),
            embeddings_twitter_text = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']") %>% html_text2(),
          )),
          
          # embeddings_all = list(doc %>% html_nodes(".EmbedBlock-root") %>% html_attr("src")),
          embeddings_other_link = list(tibble(embeddings_other_link = doc %>% html_elements(xpath = "//ancestor-or-self::*[@class='AllEmbed']//*") %>% html_attr("src"))) # including commentary section etc.
          
        )),
        savedoc = doc_hash,
        available_online = T
      )
      
        # write to JSON:
        json_loc <- paste0(folder, "/rt_archive.json")
        
        if (file.exists(json_loc)) {
          appended <- fromJSON(json_loc) %>% 
            rbind(., page_data)
  
          write_json(appended, json_loc, pretty = T, na = "string", Date = 'ISO8601', POSIXt = 'ISO8601') 
          
        } else{
          write_json(page_data, json_loc, pretty = T, na = "string", Date = 'ISO8601', POSIXt = 'ISO8601') 
        }
       # end of write_json
    # end of nested
        
    } else {
      
      # start of non-nested/csv
      
      page_data <- tibble(
        header = doc %>% html_elements(., ".HeadLine-type_2") %>% html_text2() %>% stringr::str_c(., collapse = " "), 
        # or use header = doc %>% html_elements(xpath = "//meta[@property='og:title']") %>% html_attr("content")
        link = full_link,
        
        category = doc %>% html_element(".Breadcrumbs-arrow+ .Breadcrumbs-item span") %>% html_text2(),
        # category_link = stringr::str_c(collapse = ";", 
        #                              ifelse(stringr::str_length(doc %>% html_nodes(".Breadcrumbs-arrow+") %>% html_attr("href")) > 0,
        #                                     stringr::str_c("https://de.rt.com", doc %>% html_nodes(".Breadcrumbs-arrow+") %>% html_attr("href")),
        #                                     NA)),
        
        category_full_link = stringr::str_c(collapse = ";", 
                                            ifelse(stringr::str_length(doc %>% html_nodes(".RTLink-root") %>% html_attr("href")) > 0,
                                                   stringr::str_c("https://de.rt.com", doc %>% html_nodes(".RTLink-root") %>% html_attr("href")),
                                                   NA)),
        category_full = stringr::str_c(collapse = ";", doc %>% html_elements(".RTLink-root span") %>% html_text2()),
        
        author = doc %>% html_element("p:nth-child(1) em") %>% html_text2() %>% stringr::str_remove(., pattern = "[v]on|[V]on "), # test with more articles
        # date = doc %>% html_element(".Timestamp-default") %>% html_text2(), 
        # should be identical but who knows...
        date_time = doc %>% html_elements(xpath = "//meta[@name='publish-date']") %>% html_attr("content") %>% lubridate::ymd_hms(),
        date = as.Date(date_time),
        
        tags_name = stringr::str_c(collapse = ";", doc %>% html_elements(".Tags-link") %>% html_text2()),
        tags_links = stringr::str_c(collapse = ";", 
                                    ifelse(stringr::str_length(doc %>% html_nodes(".Tags-link") %>% html_attr("href")) > 0,
                                           stringr::str_c("https://de.rt.com", doc %>% html_nodes(".Tags-link") %>% html_attr("href")),
                                           NA)),
        
        lead = doc %>% html_element(".p-24 .Text-type_1") %>% html_text2() %>% stringr::str_c(., collapse = " "), 
        # or use lead = doc %>% html_elements(xpath = "//meta[@name='description']") %>% html_attr("content")
        text_article = stringr::str_c(doc %>% html_elements(".ViewText-root p") %>% html_text2(), collapse = " "), # pure article text
        text_all = stringr::str_c(doc %>% html_elements(".ViewText-root") %>% html_text2() %>%  # all text including intextlinks & embeddings
                                    stringr::str_remove_all(., "noscript pattern"),
                                  collapse = " "),
        
        intext_links = stringr::str_c(collapse = ";", doc %>% html_nodes(".ViewText-root a") %>% html_attr("href")),
        
        recommendation_main_link = stringr::str_c(collapse = ";", doc %>% html_nodes("strong+ a") %>% html_attr("href")),
        recommendation_main_title = stringr::str_c(collapse = ";", doc %>% html_nodes("strong+ a") %>% html_text2()),
        recommendation_embedded_thumbnail = stringr::str_c(collapse = ";", doc %>% html_elements(".ReadMore-root .Picture-root img") %>% html_attr("data-src")),
        recommendation_embedded_title = stringr::str_c(collapse = ";", doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_text2()),
        recommendation_embedded_link = stringr::str_c(collapse = ";",
                                                      ifelse(stringr::str_length(doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_attr("href")) > 0,
                                                             stringr::str_c("https://de.rt.com", doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_attr("href")),
                                                             NA)),
        
        image_src = stringr::str_c(collapse = ";", doc %>% html_nodes(".lazyloaded") %>% html_attr("src")), # could be more stable (e.g. using .Cover-root)?
        image_caption = stringr::str_c(collapse = ";", doc %>% html_elements(".Cover-caption") %>% html_text2()),
        image_rights = stringr::str_c(collapse = ";", doc %>% html_elements(".Cover-imageSource") %>% html_text2()),
        # multimedia_links = list(doc %>% html_nodes(".EmbedBlock-root") %>% html_attr("href")),
        
        # Embeddings:  
        embeddings_yt_link = stringr::str_c(collapse = ";", 
                                            ifelse(stringr::str_length(doc %>% html_elements(xpath = "//div[@data-rtmodule='YouTubeEmbed']") %>% html_attr("id")) > 0,
                                                   stringr::str_c("https://www.youtube.com/watch?v=", doc %>% html_elements(xpath = "//div[@data-rtmodule='YouTubeEmbed']") %>% html_attr("id")),
                                                   NA)),
        # The following is loaded lazy, hence would need additional clicks to save.
        # embeddings_yt_cover = list(doc %>% html_elements(xpath = "//div[@class='YouTubeEmbed-cover']/picture/source") %>% html_attr("srcset")), 
        
        # embeddings_twitter = list(doc %>% html_nodes(".TwitterEmbed") %>% html_attr("src")), # only for newer twitter embeddings
        embeddings_twitter_link = stringr::str_c(collapse = ";", doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']/*/a") %>% html_attr("href")),
        embeddings_twitter_date = stringr::str_c(collapse = ";", doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']/*/a") %>% html_text2() %>% lubridate::parse_date_time(., "%B %d, %Y") %>% as.character()),
        embeddings_twitter_text = stringr::str_c(collapse = ";", doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']") %>% html_text2()),
        
        # embeddings_all = list(doc %>% html_nodes(".EmbedBlock-root") %>% html_attr("src")),
        embeddings_other_link = stringr::str_c(collapse = ";", doc %>% html_elements(xpath = "//ancestor-or-self::*[@class='AllEmbed']//*") %>% html_attr("src")), # including commentary section etc.
        
        savedoc = doc_hash,
        available_online = T
      )
    }
    
# write/append csv 
    
  }  # end of try_catch
    
    if (art_no > begin_with_article) {
      df_new <- page_data
      RT_articles <- RT_articles %>% add_row(., df_new)
    } else {
      RT_articles <- page_data
    }
  
  
  # replace character(0) with proper NA
  if (lists_as_character == T) {
    RT_articles <- RT_articles %>% 
      mutate(across(.fns = ~ as.character(.)), 
             across(.fns = ~ replace(., . == "character(0)", NA)),  
             across(.fns = ~ replace(., . == "logical(0)", NA)))
  }
 }
  return(RT_articles)    
}
}


# scrape articles from saved: ####

scrape_articles_from_saved <- function(folder = "RT_searches",
                                      subfolder = "articles",
                                      baseURL = "https://de.rt.com",
                                      lists_as_character = F, # gives nicer NAs, but looses nested structure. Date formats are preserved.
                                      nested = F # want a nested df instead of lists/c(""), -> deprecated
){
  
  library(tidyverse)
  library(lubridate)
  library(rvest)


  # RT_articles <- tibble()
  
  file_path <- paste0(folder, "/", subfolder)
  
  for (file in dir(file_path)) {
    
    doc <- readr::read_file(paste0(file_path, "/", file)) %>% 
      read_html(encoding = "UTF-8")
    
    # get data:
    if (nested == T) {
      
      # save data:
      page_data <- tibble(
        header = doc %>% html_elements(., ".HeadLine-type_2") %>% html_text2() %>% stringr::str_c(., collapse = " "), 
        # or use header = doc %>% html_elements(xpath = "//meta[@property='og:title']") %>% html_attr("content")
        link = doc %>% html_elements(xpath = "//meta[@property='og:url']") %>% html_attr("content"),
        
        author = doc %>% html_element("p:nth-child(1) em") %>% html_text2() %>% stringr::str_remove(., pattern = "[v]on|[V]on "), # test with more articles
        # date = doc %>% html_element(".Timestamp-default") %>% html_text2(), 
        # should be identical but who knows...
        date_time = doc %>% html_elements(xpath = "//meta[@name='publish-date']") %>% html_attr("content") %>% lubridate::ymd_hms(),
        date = as.Date(date_time),
        
        category = doc %>% html_element(".Breadcrumbs-arrow+ .Breadcrumbs-item span") %>% html_text2(),
        
        category_full = list(tibble(
          category_full_link = stringr::str_c(collapse = ";", 
                                              ifelse(stringr::str_length(doc %>% html_nodes(".RTLink-root") %>% html_attr("href")) > 0,
                                                     stringr::str_c(baseURL, doc %>% html_nodes(".RTLink-root") %>% html_attr("href")),
                                                     NA)),
          category_full = stringr::str_c(collapse = ";", doc %>% html_elements(".RTLink-root span") %>% html_text2()),
        )),
        
        tags = list(tibble(
          tags_name = doc %>% html_elements(".Tags-link") %>% html_text2(),
          tags_links = ifelse(stringr::str_length(doc %>% html_nodes(".Tags-link") %>% html_attr("href"))>0,
                              stringr::str_c(baseURL, doc %>% html_nodes(".Tags-link") %>% html_attr("href")),
                              NA)
        )),
        
        lead = doc %>% html_element(".p-24 .Text-type_1") %>% html_text2(), 
        # or use lead = doc %>% html_elements(xpath = "//meta[@name='description']") %>% html_attr("content")
        text_article = stringr::str_c(doc %>% html_elements(".ViewText-root p") %>% html_text2(), collapse = " "), # pure article text
        text_all = stringr::str_c(doc %>% html_elements(".ViewText-root") %>% html_text2() %>%  # all text including intextlinks & embeddings
                                    stringr::str_remove_all(., "noscript pattern"),
                                  collapse = " "),
        
        intext_links = list(tibble(intext_links = doc %>% html_nodes(".ViewText-root a") %>% html_attr("href"))),
        
        recommendation = list(tibble(
          recommendation_main = list(tibble(
            recommendation_main_link = list(tibble(recommendation_main_link = doc %>% html_nodes("strong+ a") %>% html_attr("href"))),
            recommendation_main_title = list(tibble(recommendation_main_title = doc %>% html_nodes("strong+ a") %>% html_text2())),
          )),
          recommendation_embedded = list(tibble(
            recommendation_embedded_thumbnail = list(tibble(recommendation_embedded_thumbnail = doc %>% html_elements(".ReadMore-root .Picture-root img") %>% html_attr("data-src"))),
            recommendation_embedded_title = list(tibble(recommendation_embedded_title = doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_text2())),
            recommendation_embedded_link = list(tibble(ifelse(stringr::str_length(doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_attr("href")) > 0,
                                                              stringr::str_c(baseURL, doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_attr("href")),
                                                              NA)
            ))
          ))
        )),  
        
        image = list(tibble(
          image_src = 
            list(tibble(
              image_src = doc %>% html_nodes(".lazyloaded") %>% html_attr("src") # could be more stable (e.g. using .Cover-root)?
            )),
          image_caption =
            list(tibble(
              image_caption = doc %>% html_elements(".Cover-caption") %>% html_text2()
            )),
          image_rights = 
            list(tibble(
              image_rights = doc %>% html_elements(".Cover-imageSource") %>% html_text2()
            ))
          # multimedia_links = list(doc %>% html_nodes(".EmbedBlock-root") %>% html_attr("href")),
        )),
        
        # Embeddings:  
        embeddings = list(tibble(
          
          embeddings_yt_link = list(tibble(embeddings_yt_link = ifelse(stringr::str_length(doc %>% html_elements(xpath = "//div[@data-rtmodule='YouTubeEmbed']") %>% html_attr("id")) > 0,
                                                                       stringr::str_c("https://www.youtube.com/watch?v=", doc %>% html_elements(xpath = "//div[@data-rtmodule='YouTubeEmbed']") %>% html_attr("id")),
                                                                       NA))),
          # The following is loaded lazy, hence would need additional clicks to save.
          # embeddings_yt_cover = list(doc %>% html_elements(xpath = "//div[@class='YouTubeEmbed-cover']/picture/source") %>% html_attr("srcset")), 
          
          # embeddings_twitter = list(doc %>% html_nodes(".TwitterEmbed") %>% html_attr("src")), # only for newer twitter embeddings
          embeddings_twitter  = list(tibble(
            embeddings_twitter_link = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']/*/a") %>% html_attr("href"),
            embeddings_twitter_date = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']/*/a") %>% html_text2() %>% lubridate::parse_date_time(., "%B %d, %Y") %>% as.character(),
            embeddings_twitter_text = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']") %>% html_text2(),
          )),
          
          # embeddings_all = list(doc %>% html_nodes(".EmbedBlock-root") %>% html_attr("src")),
          embeddings_other_link = list(tibble(embeddings_other_link = doc %>% html_elements(xpath = "//ancestor-or-self::*[@class='AllEmbed']//*") %>% html_attr("src"))) # including commentary section etc.
          
        )),
        savedoc = file
      )
      
      
      # nesting afterwards: 
      
      #   page_data <- page_data %>%    
      #   nest(.,
      #         tags = c(tags_name, tags_links),
      #   #      recommendations = c(
      #           recommendations_main = c(recommendation_main_link, recommendation_main_title),
      #           recommendations_embedded = c(recommendation_embedded_thumbnail, recommendation_embedded_link, recommendation_embedded_title),
      #    #     ),
      #         images = c(image_src, image_caption, image_rights),
      # #        embeddings = c(
      #           embeddings_yt = c(embeddings_yt_link),
      #           embeddings_twitter = c(embeddings_twitter_link, embeddings_twitter_date, embeddings_twitter_text),
      #           embeddings_other = c(embeddings_other_link)
      #  #         )
      #         ) %>% 
      #       nest(.,
      #            recommendations = c(recommendations_main, recommendations_embedded),
      #            embeddings = c(embeddings_yt, embeddings_twitter, embeddings_other)
      #            )
      
    } else {
      
      page_data <- tibble(
        header = doc %>% html_elements(., ".HeadLine-type_2") %>% html_text2() %>% stringr::str_c(., collapse = " "), 
        # or use header = doc %>% html_elements(xpath = "//meta[@property='og:title']") %>% html_attr("content")
        link = doc %>% html_elements(xpath = "//meta[@property='og:url']") %>% html_attr("content"),
        
        category = doc %>% html_element(".Breadcrumbs-arrow+ .Breadcrumbs-item span") %>% html_text2(),
        # category_link = stringr::str_c(collapse = ";", 
        #                              ifelse(stringr::str_length(doc %>% html_nodes(".Breadcrumbs-arrow+") %>% html_attr("href")) > 0,
        #                                     stringr::str_c("https://de.rt.com", doc %>% html_nodes(".Breadcrumbs-arrow+") %>% html_attr("href")),
        #                                     NA)),
        
        category_full_link = stringr::str_c(collapse = ";", 
                                            ifelse(stringr::str_length(doc %>% html_nodes(".RTLink-root") %>% html_attr("href")) > 0,
                                                   stringr::str_c(baseURL, doc %>% html_nodes(".RTLink-root") %>% html_attr("href")),
                                                   NA)),
        category_full = stringr::str_c(collapse = ";", doc %>% html_elements(".RTLink-root span") %>% html_text2()),
        
        author = doc %>% html_element("p:nth-child(1) em") %>% html_text2() %>% stringr::str_remove(., pattern = "[v]on|[V]on "), # test with more articles
        # date = doc %>% html_element(".Timestamp-default") %>% html_text2(), 
        # should be identical but who knows...
        date_time = doc %>% html_elements(xpath = "//meta[@name='publish-date']") %>% html_attr("content") %>% lubridate::ymd_hms(),
        date = as.Date(date_time),
        
        tags_name = stringr::str_c(collapse = ";", doc %>% html_elements(".Tags-link") %>% html_text2()),
        tags_links = stringr::str_c(collapse = ";", 
                                    ifelse(stringr::str_length(doc %>% html_nodes(".Tags-link") %>% html_attr("href")) > 0,
                                           stringr::str_c(baseURL, doc %>% html_nodes(".Tags-link") %>% html_attr("href")),
                                           NA)),
        
        lead = doc %>% html_element(".p-24 .Text-type_1") %>% html_text2() %>% stringr::str_c(., collapse = " "), 
        # or use lead = doc %>% html_elements(xpath = "//meta[@name='description']") %>% html_attr("content")
        text_article = stringr::str_c(doc %>% html_elements(".ViewText-root p") %>% html_text2(), collapse = " "), # pure article text
        text_all = stringr::str_c(doc %>% html_elements(".ViewText-root") %>% html_text2() %>%  # all text including intextlinks & embeddings
                                    stringr::str_remove_all(., "noscript pattern"),
                                  collapse = " "),
        
        intext_links = stringr::str_c(collapse = ";", doc %>% html_nodes(".ViewText-root a") %>% html_attr("href")),
        
        recommendation_main_link = stringr::str_c(collapse = ";", doc %>% html_nodes("strong+ a") %>% html_attr("href")),
        recommendation_main_title = stringr::str_c(collapse = ";", doc %>% html_nodes("strong+ a") %>% html_text2()),
        recommendation_embedded_thumbnail = stringr::str_c(collapse = ";", doc %>% html_elements(".ReadMore-root .Picture-root img") %>% html_attr("data-src")),
        recommendation_embedded_title = stringr::str_c(collapse = ";", doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_text2()),
        recommendation_embedded_link = stringr::str_c(collapse = ";",
                                                      ifelse(stringr::str_length(doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_attr("href")) > 0,
                                                             stringr::str_c(baseURL, doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_attr("href")),
                                                             NA)),
        
        image_src = stringr::str_c(collapse = ";", doc %>% html_nodes(".lazyloaded") %>% html_attr("src")), # could be more stable (e.g. using .Cover-root)?
        image_caption = stringr::str_c(collapse = ";", doc %>% html_elements(".Cover-caption") %>% html_text2()),
        image_rights = stringr::str_c(collapse = ";", doc %>% html_elements(".Cover-imageSource") %>% html_text2()),
        # multimedia_links = list(doc %>% html_nodes(".EmbedBlock-root") %>% html_attr("href")),
        
        # Embeddings:  
        embeddings_yt_link = stringr::str_c(collapse = ";", 
                                            ifelse(stringr::str_length(doc %>% html_elements(xpath = "//div[@data-rtmodule='YouTubeEmbed']") %>% html_attr("id")) > 0,
                                                   stringr::str_c("https://www.youtube.com/watch?v=", doc %>% html_elements(xpath = "//div[@data-rtmodule='YouTubeEmbed']") %>% html_attr("id")),
                                                   NA)),
        # The following is loaded lazy, hence would need additional clicks to save.
        # embeddings_yt_cover = list(doc %>% html_elements(xpath = "//div[@class='YouTubeEmbed-cover']/picture/source") %>% html_attr("srcset")), 
        
        # embeddings_twitter = list(doc %>% html_nodes(".TwitterEmbed") %>% html_attr("src")), # only for newer twitter embeddings
        embeddings_twitter_link = stringr::str_c(collapse = ";", doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']/*/a") %>% html_attr("href")),
        embeddings_twitter_date = stringr::str_c(collapse = ";", doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']/*/a") %>% html_text2() %>% lubridate::parse_date_time(., "%B %d, %Y") %>% as.character()),
        embeddings_twitter_text = stringr::str_c(collapse = ";", doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']") %>% html_text2()),
        
        # embeddings_all = list(doc %>% html_nodes(".EmbedBlock-root") %>% html_attr("src")),
        embeddings_other_link = stringr::str_c(collapse = ";", doc %>% html_elements(xpath = "//ancestor-or-self::*[@class='AllEmbed']//*") %>% html_attr("src")), # including commentary section etc.
        
        savedoc = file
      )
    }
    
    
    if (file != dir(file_path)[1]) {
      df_new <- page_data
      RT_articles <- RT_articles %>% add_row(., df_new)
    } else {
      RT_articles <- page_data
    }
  }  
  
  
  # replace character(0) with proper NA
  if (lists_as_character == T) {
    RT_articles <- RT_articles %>% 
      mutate(across(.fns = ~ as.character(.)), 
             across(.fns = ~ replace(., . == "character(0)", NA)),  
             across(.fns = ~ replace(., . == "logical(0)", NA)))
  }
  
  return(RT_articles)    
}
