# 01 Scraping RT

### by search queries directly from RT ####
# make sure to use DNS server/IP address that avoids blocking

eCap <- list(
  phantomjs.page.settings.userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/100.0.4896.75 Safari/537.36"
)

## create general (basic) function to scrape RT DE: ####
scrapeRT <- function(startdate = startdate, 
                     period_end = period_end, 
                     keyword = "mehr",
                     interval_length = 2,
                     sleepmin = 1,
                     sleepmax = 3){
  
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
  
  while (startdate <= dmy(period_end)) {
    # loop over search URLs by time interval:
    search <- paste0("https://de.rt.com/search?q=", keyword ,"&df=", startdate, "&dt=", enddate)  
    
    ### everything else in between:
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
    writeLines(page, paste0("articles/RT_searches_candidates/", keyword , startdate, "-", enddate, ".txt"), useBytes = T)
    
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
      add_with_rollback(., days(interval_length))
    startdate <- startdate %>% 
      add_with_rollback(., days(interval_length))
    
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
period_end <- "31-12-2021"
# keyword & interval as default

links_RT_search <- scrapeRT(startdate, period_end, interval_length = 1)

links_RT_search_Baerbock_new <- scrapeRT(startdate, period_end, keyword = "Baerbock", interval_length = 25, sleepmin = 5, sleepmax = 7)
# got 419/422 (soll 408 in 2021)
links_RT_search_Laschet_new <- scrapeRT(startdate, period_end, keyword = "Laschet", interval_length = 25, sleepmin = 5, sleepmax = 7)
# got 390/413 (soll 409 in 2021)
links_RT_search_Scholz_new <- scrapeRT(startdate, period_end, keyword = "Scholz", interval_length = 25, sleepmin = 5, sleepmax = 7)
# got 439/471 (soll 447 in 2021) 

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


### get data from articles ####

# flatten list:
library(rvest)
# links_RT_search <- read_rds('Articles/rt_links.rds')
# links_RT_flat <- links_RT_search %>% flatten()

links_RT <- c()
for(i in 1:length(links_RT_search)){
  
  for(j in 1:length(links_RT_search[[i]])){
    
    links_RT <- append(links_RT, xml_attrs(links_RT_search[[i]][[j]])[1][[1]])
    
  }
  j = 1
}


library(magrittr)
links_RT %<>% tibble(links_RT) %>% 
  mutate(
    id = str_split(links_RT, "/*/", simplify = T, n = 3)[,3],
    id = str_split(id, "-", simplify = T)[,1],
    id = regmatches(id, gregexpr("[[:digit:]]+", id)) %>%  unlist() %>% as.numeric(),
    links_full = paste0("https://de.rt.com", links_RT)
  ) %>% 
  select(-.)


readr::write_csv(links_RT, "Articles/links_rt.csv")
links_RT <- read.csv("articles/links_rt.csv")

# somehow different (df vs tbl_df), shouldn't matter
identical(test$links_full, links_RT$links_full)



# think of a way to include this in function but run only once:
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
# start Selenium
library(RSelenium)
rD <- rsDriver(browser = "chrome",
               chromever = "99.0.4844.51",
               verbose = F)
remDr <- rD[["client"]]

extract_variables_rt <- function(doc = doc){
  
  # ideally (but hopefully unnecessary): include try/catch
  doc <- read_html(doc)
  df <- bind_cols(
    tibble(
      date = case_when(
        !is.null_or_na(doc %>% html_elements(".ArticleView-timestamp .Timestamp-default")) ~ str_c("", doc %>% html_elements(".ArticleView-timestamp .Timestamp-default") %>% html_text2()),
        !is.null_or_na(doc %>% html_elements(".amp-pages__date")) ~ str_c("", doc %>% html_elements(".amp-pages__date") %>% html_text2()), 
        TRUE ~ NA_character_)
    ),
    tibble(
      heading = case_when(
        !is.null_or_na(doc %>% html_elements(".HeadLine-type_2")) ~ str_c("", doc %>% html_elements(".HeadLine-type_2") %>% html_text2()),
        !is.null_or_na(doc %>% html_elements(".amp-pages__title")) ~ str_c("", doc %>% html_elements(".amp-pages__title") %>% html_text2()), 
        TRUE ~ NA_character_)
    ),
    tibble(
      lead = case_when(
        !is.null_or_na(doc %>% html_elements(".Text-type_1")) ~ str_c("", doc %>% html_elements(".Text-type_1") %>% html_text2()),
        !is.null_or_na(doc %>% html_elements(".amp-pages__summary")) ~ str_c("", doc %>% html_elements(".amp-pages__summary") %>% html_text2()), 
        TRUE ~ NA_character_)
    ),
    tibble(
      resort = case_when(
        !is.null_or_na(doc %>% html_elements(".Breadcrumbs-arrow+ .Breadcrumbs-item span")) ~ str_c("", doc %>% html_elements(".Breadcrumbs-arrow+ .Breadcrumbs-item span") %>% html_text2()),
        # not shown on amp pages:
        !is.null_or_na(doc %>% html_elements(".amp-pages__title")) ~ "amp", 
        TRUE ~ NA_character_)
    ),
    tibble(
      text = case_when(
        !is.null_or_na(doc %>% html_elements(".ViewText-root")) ~ str_c("", doc %>% html_elements(".ViewText-root") %>% html_text2()),
        !is.null_or_na(doc %>% html_elements(".text")) ~ str_c("", doc %>% html_elements(".text") %>% html_text2()), 
        TRUE ~ NA_character_)
    ),
    wordcount = NA,
    source = "RTDE",
    page = NA
  ) %>% 
    select(heading, source, resort, wordcount, page, date, text, lead)
  return(df)
}

## get page source ####
# with function:
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
# 
# remDr$close()

# data_RT %<>% left_join(., links_RT, by = c("link" = "links_full"))



# better: loop to save txt

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


# 
# # save dataset
# save.image("all_data.RData")
# 
# write_csv(data_RT, 'Articles/data_rt.csv')
# rio::export(data_RT, "Articles/data_rt.rds")
# 
# # import again:
# data_RT1 <- rio::import("articles/data_rt.rds")
# data_RT2 <- rio::import("articles/data_rt.csv") # seems preferable, but larger
# 

# ToDo:
# clean dates
# wordcount
# keyword variables
# sentiment?
# analyse :D



### convert pages to data: #####


# IA version
{extract_variables_rt <- function(doc = doc){
  
  # ideally (but hopefully unnecessary): include try/catch
  doc <- read_html(doc)
  df <- bind_cols(
    tibble(
      date = case_when(
        !is.null_or_na(doc %>% html_elements(".ArticleView-timestamp .Timestamp-default")) ~ str_c("", doc %>% html_elements(".ArticleView-timestamp .Timestamp-default") %>% html_text2()),
        !is.null_or_na(doc %>% html_elements(".amp-pages__date")) ~ str_c("", doc %>% html_elements(".amp-pages__date") %>% html_text2()), 
        TRUE ~ NA_character_)
    ),
    tibble(
      heading = case_when(
        !is.null_or_na(doc %>% html_elements(".HeadLine-type_2")) ~ str_c("", doc %>% html_elements(".HeadLine-type_2") %>% html_text2()),
        !is.null_or_na(doc %>% html_elements(".amp-pages__title")) ~ str_c("", doc %>% html_elements(".amp-pages__title") %>% html_text2()), 
        TRUE ~ NA_character_)
    ),
    tibble(
      lead = case_when(
        !is.null_or_na(doc %>% html_elements(".Text-type_1")) ~ str_c("", doc %>% html_elements(".Text-type_1") %>% html_text2()),
        !is.null_or_na(doc %>% html_elements(".amp-pages__summary")) ~ str_c("", doc %>% html_elements(".amp-pages__summary") %>% html_text2()), 
        TRUE ~ NA_character_)
    ),
    tibble(
      resort = case_when(
        !is.null_or_na(doc %>% html_elements(".Breadcrumbs-arrow+ .Breadcrumbs-item span")) ~ str_c("", doc %>% html_elements(".Breadcrumbs-arrow+ .Breadcrumbs-item span") %>% html_text2()),
        # not shown on amp pages:
        !is.null_or_na(doc %>% html_elements(".amp-pages__title")) ~ "amp", 
        TRUE ~ NA_character_)
    ),
    tibble(
      text = case_when(
        !is.null_or_na(doc %>% html_elements(".ViewText-root")) ~ str_c("", doc %>% html_elements(".ViewText-root") %>% html_text2()),
        !is.null_or_na(doc %>% html_elements(".text")) ~ str_c("", doc %>% html_elements(".text") %>% html_text2()), 
        TRUE ~ NA_character_)
    ),
    id = NA,
    wordcount = NA,
    source = "RTDE",
    page = NA
  ) %>% 
    select(id, heading, source, resort, wordcount, page, date, text, lead)
  return(df)
}
df_rt <- map_dfr(html_pages, extract_variables_rt)
}



# old version:


df_rt_articles <- tibble()

for (ID in dir("articles/RThtml")) {
  
  doc <- readr::read_file(paste0("articles/RThtml/RTarticle", i, ".txt")) %>% 
    read_html()

df_rt_articles <- 
  append(
            date =  doc %>% html_elements(".ArticleView-timestamp .Timestamp-default") %>% html_text2(),
              #  %>% tibble() %>% rename(., "date" = "."),
            heading = doc  %>% html_elements(".HeadLine-type_2") %>% html_text2(),
              #  %>% tibble() %>% rename(., "heading" = "."),
            lead = doc %>% html_elements(".Text-type_1") %>% html_text2() %>%
                   tibble() %>% rename(., "lead" = "."),
            resort = doc %>% html_elements(".Breadcrumbs-arrow+ .Breadcrumbs-item span") %>% html_text2() %>%
                   tibble() %>% rename(., "resort" = "."),
                 doc %>% html_elements(".ViewText-root")%>% html_text2() %>%
                   tibble() %>% rename(., "text" = "."),
                 id = ID,
                 wordcount = NA,
                 source = "RTDE",
                 page = NA
  )
}

df_rt_articles %<>%
    select(id, heading, source, resort, wordcount, page, date, text, lead)


#   return(df)
#   }

month_abbs_de <- df_rt %>% 
  mutate(month_abb = str_sub(date, -19, -16)) %>% 
  group_by(month_abb) %>% 
  summarize(n()) %>% 
  arrange(month_abb) %>% 
  select(month_abb) %>%
  cbind(month = c(4, 8, 12, 2, 1, 7, 6, 5, 3, 5, 11, 10, 9, 4, 8, 12, 2, 1, 7, 6, 3, 11, 10, 9, NA)) %>% 
  arrange(month)

df_rt %<>% 
  mutate(year = str_sub(date, -14, -11) %>% as.numeric(),
         month_abb = str_sub(date, -19, -16),
         day = str_sub(date, 1, 2) %>% as.numeric() %>% sprintf(fmt = "%02d", .)
  ) %>% 
  left_join(., month_abbs_de, by = "month_abb") %>% 
  mutate(date = paste0(year, "/", month, "/", day) %>% as.POSIXlt(., format = "%Y/%m/%d")) %>% 
  arrange(date, resort) %>% 
  unique()

df_study <- df_rt %>% filter(year == 2021)

df_study %<>% mutate(
  mention_Laschet = str_detect(text, "Laschet"),
  mention_Scholz = str_detect(text, "Scholz"),
  mention_Baerbock = str_detect(text, "Baerbock")
)

df_study %>% 
  group_by(mention_Laschet, mention_Scholz, mention_Baerbock) %>% 
  summarize(n())

# LOL each one is mentioned in one article only xD ... I'm done. 

