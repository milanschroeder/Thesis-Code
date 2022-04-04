# 01 Scraping Sources

## load packages ####
library(pacman)
p_load(tidyverse)
p_load(magrittr)
p_load(roperators)
p_load(rvest)
p_load(rlist)
p_load(xml2)
p_load(lubridate)
p_load(RSelenium)
p_load(httr)
# I use the Internet Archive (IA) to scrape all articles released by RT Deutsch over the year 2021 ####

## get archive entries: (by calender) ####

aktuell <- read_html("https://web.archive.org/web/20210601000000*/de.rt.com/aktuell")

dates <- paste0(2021, rep(sprintf("%02d", 1:12), 31), sprintf("%02d", 1:31)) %>% sort()
# delete non-existing dates:
noDates <- c(paste0(202102, 29:31), paste0(2021, sprintf("%02d", c(04, 06, 09, 11)), 31))
dates <- dates[dates %ni% noDates]

IA_URLS_aktuell <- paste0("https://web.archive.org/web/", dates, "/de.rt.com/aktuell")
IA_URLS_home <- paste0("https://web.archive.org/web/", dates, "/de.rt.com")

## get article URLs:

# same across aktuell/home (there also includes "Picture of the Day" and "Feature Video")

links_RT <- map_df(.x = IA_URLS_home, .f =  ~ read_html(.) %>% html_elements(".Link-isFullCard"))


 

### by trying index number (if intended directly from RT): ####
# observation: URLS of RT Articles from 2021 have an 6-digit index number starting with 11 (and some with 12)
## strategy: scrape all of both, then filter by date
# issue: Access blocked frequently + many unnecessary requests
index <- sprintf("%04d", 0000:9999)
RT_URLS <- paste0("https://de.rt.com/*/1", rep(c(1,2), each = 10000) , index ,"-*")

library(httr)
RT_site <- RT_URLS[1] %>% 
  html_session(add_headers(`User-Agent`="Mozilla/5.0 (iPhone; CPU iPhone OS 10_3_1 like Mac OS X) AppleWebKit/603.1.30 (KHTML, like Gecko) Version/10.0 Mobile/14E304 Safari/602.1")) %>% 
read_html()

test <- tibble(
header = session %>% html_elements(".HeadLine-type_2") %>% html_text2(),
date = session %>% html_elements(".ArticleView-timestamp .Timestamp-default") %>% html_text2(),
text = session %>% html_elements(".ViewText-root") %>% html_text2(),
resort = session %>% html_elements(".Breadcrumbs-arrow+ .Breadcrumbs-item span") %>% html_text2(),
URL = RT_URLS[1]
)
### by search queries directly from RT ####


## create general (basic) function to scrape RT DE: ####
scrapeRT <- function(startdate = startdate, 
                     period_end = period_end, 
                     keyword = "mehr",
                     interval_length = 2){

 # ToDO: Check input
  # startdate & period_end may be anything convertable by lubridate::dmy(), e.g. a string in the format "dd-mm-yyyy"
  # keyword is not case sensitive (what about special characters?)
  # intervall length must be (convertable to) an integer (else: use default)
  
  # load required packages
  # ToDO: add installer and check 
  library(lubridate)
  library(RSelenium)
  library(rlist)
  library(rvest)

  # start Remote Driver:
  system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=TRUE, show.output.on.console = F)
  rD <- rsDriver(browser = "chrome",
                 chromever = "99.0.4844.51",
                 verbose = F,
                 port = port)
  remDr <- rD[["client"]]
  
  # be careful not to get blocked:
    # probably better don't include this as an option(?)
  sleepmin = 1
  sleepmax = 3
  max_attempts = 3
  
  # ToDo: only if option (still missing) named == T
  list_names <- c()
  
  # create empty list to write to 
    # ToDo: empty xml nodeset would be better (select format option?)
  links_RT_search <- list()
  startdate = lubridate::dmy(startdate)
  enddate <- startdate %>%  
    add_with_rollback(., days(interval_length))

while (enddate < dmy(period_end)) {
  # loop over search URLs by time intervall:
  search <- paste0("https://de.rt.com/search?q=", keyword ,"&df=", startdate, "&dt=", enddate)  

  ### everything else in between:
  remDr$navigate(search)
  
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
      # try(webElem1$clickElement())
      tryCatch({webElem2 <- remDr$findElement(using = "css selector", ".Button-root.Button-is1to1.Button-type_l.Button_dark")},
                          error = function(e){NULL})
      try(webElem2$clickElement())
      
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
  
  # safe list of links per intervall:
  new_links <- remDr$getPageSource()[[1]] %>% 
    read_html() %>% 
    html_elements(., ".HeaderNews-type_5 .Link-isFullCard")
  links_RT_search <- list.append(links_RT_search, new_links)
  
  # prepare vector with list names:
  list_name <- paste0("week",startdate,"-",enddate)
  list_names <- append(list_names, list_name)
  
  # ToDo: add option to suppress output: 
  print(paste("period", startdate, "to", enddate, "completed."))
  
  # go back one period
enddate <- enddate %>% 
  add_with_rollback(., days(intervall_length))
startdate <- startdate %>% 
  add_with_rollback(., days(intervall_length))

# ToDo: add option to suppress output: 
print(paste("period is now", startdate, "to", enddate))
}
  
# ToDo: opt in option for named list
names(links_RT_search) <- list_names

# ToDo: add option to select format (df, list, xml_nodeset, ...)

# ToDo: add option to convert to full dataset(?)
  # by calling junctions get_pages_RT and extract_variables_rt, so can also be done in multiple steps with additional options (default)

return(links_RT_search)
}

### apply function ####
links_RT_search <- scrapeRT(startdate, period_end)

# save list:
library(rlist)
rlist::list.save(links_RT_search, 'Articles/rt_links.rds')

# set parameter:
  # select startdate (here: 01 Jan 2021)
  startdate <- "01-01-2021" 
  # select time interval (here: 2 to keep it as small as possible)
  period_end <- "31-12-2021"
  # keyword & interval as default


identical(links_RT_search, 'Articles/rt_links.rds')
# saved 02/04/2022 19:00

### get data from articles ####

# flatten list:
library(rvest)
links_RT_search <- read_rds('Articles/rt_links.rds')
links_RT_flat <- links_RT_search %>% flatten()

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

data_RT <- map_dfr(links_RT$links_full, get_pages_RT)
remDr$close()

data_RT %<>% left_join(., links_RT, by = c("link" = "links_full"))

# save dataset
write_csv(data_RT, 'Articles/data_rt.csv')
rio::export(data_RT, "Articles/data_rt.rds")

# import again:
data_RT1 <- rio::import("articles/data_rt.rds")
data_RT2 <- rio::import("articles/data_rt.csv") # seems preferable, but larger


# ToDo:
# clean dates
# wordcount
# keyword variables
# sentiment?
# analyse :D


### by scraping IA search result list ####

p_load(RSelenium)

system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)

rD <- rsDriver(browser = "chrome",
               chromever = "99.0.4844.51")
remDr <- rD[["client"]]
base_url <- "https://web.archive.org/web/*/de.rt.com*"
remDr$navigate(base_url)


sleepmin = .5
sleepmax = 1
total_pages <- 200
html_tables <- vector("list", total_pages)

src <- remDr$getPageSource()[[1]]
html_tables[1] <- src

for (i in 2:total_pages) {
  webElem <- NULL
  while(is.null(webElem)){
    webElem <- tryCatch({remDr$findElement(using = "css selector", "#resultsUrl_next a")},
                        error = function(e){})
    }
    webElem$clickElement()
    
    randsleep <- sample(seq(sleepmin, sleepmax, by = 0.001), 1)
    Sys.sleep(randsleep)
    
    html_tables[i] <- remDr$getPageSource()[[1]]
}
remDr$close()

## get contents: ####
library(rvest)
RT_links <- map(html_tables, ~ read_html(., encoding = "UTF-8") %>% html_table() %>% as.data.frame())
IA_links <- map(html_tables, ~ read_html(., encoding = "UTF-8") %>% html_elements("#resultsUrl a")) %>% purrr::flatten()

### ENCODING ERROR !!!!! (concerning few relevant articles but still...)
library(xml2)

IA_URLS <- array()
for (i in 1:length(IA_links)) {
  IA_URLS[i] <- xml2::xml_attrs(IA_links[[i]])
}

RT_links %<>% bind_rows(., .id = "column_label") %>% select(URL, From, To) %>%
 mutate(year = str_sub(From, -4, -1) %>% as.numeric(),
        month = str_sub(From, 1, 3) %>% match(., month.abb),
        day = str_sub(From, 5, -7) %>% as.numeric() %>% sprintf(fmt = "%02d", .),
        archived = paste0(year, "/", month, "/", day) %>% as.POSIXlt(., format = "%Y/%m/%d")) %>% 
  bind_cols(., IA_URLS) %>% 
    mutate(., IA_URL = paste0("https://web.archive.org", ...8)) %>% 
  # filter for 2021 (and 2022) entries 
  filter(between(year, 2021, 2022)) %>% 
  arrange(archived)

### get first entries of each article: ####

system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
rD <- rsDriver(browser = "chrome",
               chromever = "99.0.4844.51",
               verbose = F)
remDr <- rD[["client"]]


sleepmin = .5
sleepmax = 1
total_pages <- nrow(RT_links)
# html_pages <- vector("list", total_pages)

for (i in 1:total_pages) {
  remDr$navigate(RT_links$IA_URL[i])
  webElem2 <- NULL
    
  while(is.null(webElem2)){
      webElem2 <- tryCatch({remDr$findElement(using = "css selector", ".captures-range-info a")},
                            error = function(e){})
    }
  webElem2$clickElement()
  
  randsleep <- sample(seq(sleepmin, sleepmax, by = 0.001), 1)
  Sys.sleep(randsleep)
  
  html_pages[i] <- remDr$getPageSource()[[1]]
  print(paste("page", i, "successfully saved"))
}
remDr$close()
#link to page 4723 broken (it's only an error page), was replaced by direct link to de.rt.com 

### save pages ####

p_load(rlist)

broken_entry_starts_with <- str_sub(html_pages[[21]], 1, 34) # get a string that identify html of only broken archive entries
test <- unlist(html_pages)
RT_links$broken <- str_detect(test, broken_entry_starts_with)



# don't want to do this again... save list

rlist::list.save(html_pages, 'Articles/html_pages_rt.rds')
html_pages <- readr::read_rds('Articles/html_pages_rt.rds')


### convert pages to data: #####
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
    id = NA,
    wordcount = NA,
    source = "RTDE",
    page = NA
    ) %>% 
    select(id, heading, source, resort, wordcount, page, date, text, lead)
  return(df)
}
df_rt <- map_dfr(html_pages, extract_variables_rt)

# old version:
# df <- bind_cols(
#                  doc %>% read_html() %>% html_elements(".ArticleView-timestamp .Timestamp-default") %>% html_text2() %>%
#                    tibble() %>% rename(., "date" = "."),
#                  doc %>% read_html() %>% html_elements(".HeadLine-type_2") %>% html_text2() %>%
#                    tibble() %>% rename(., "heading" = "."),
#                  doc %>% read_html() %>% html_elements(".Text-type_1") %>% html_text2() %>%
#                    tibble() %>% rename(., "lead" = "."),
#                  doc %>% read_html() %>% html_elements(".Breadcrumbs-arrow+ .Breadcrumbs-item span") %>% html_text2() %>%
#                    tibble() %>% rename(., "resort" = "."),
#                  doc %>% read_html() %>% html_elements(".ViewText-root")%>% html_text2() %>%
#                    tibble() %>% rename(., "text" = "."),
#                  id = NA,
#                  wordcount = NA,
#                  source = "RTDE",
#                  page = NA
#   ) %>% 
#     select(id, heading, source, resort, wordcount, page, date, text, lead)
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

# For Comparison, I use the Factiva Database to scrape all articles released online by major national daily newspapers (TAZ, FR, SZ, Welt) ####

library(tm)
library(xml2)
library(tm.plugin.factiva)

## retrieve data using:
#### la=de and rst=(taz or frarun or sddz or fazalg or dwelt or zbild) and date from 20210101 to 20211231 and [candidate surname]


candidate <- c("Baerbock", "Laschet", "Scholz")
corpus <- list()
position <- 0

# loop over candidates...
for (i in 1:length(candidate)){
  n <- length(dir(paste0("Articles/", candidate[i], "_files")))
    
  # ... and over pages á 100 articles
    for (j in n){
      Factiva_sheet <- paste0("Articles/", candidate[i] , "_files/Factiva", str_sub(candidate[i], 1, 1), j, ".htm")
      source <- tm.plugin.factiva::FactivaSource(Factiva_sheet, 
                                           encoding = "UTF-8", 
                                           format = "HTML")

        # more efficient to loop over source only, but need to include candidate variable, therefore:
      corpus <- append(corpus, tm::Corpus(source))
    }
  
  names(corpus[(position+1):(position+j)]) <- candidate[i]
  position <- position + n
}



## Due to Factiva download limit, just scrape search summaries: ####
# crucial issue: blocked by library service!
# start RD:
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
rD <- rsDriver(browser = "chrome",
               chromever = "99.0.4844.51",
               verbose = F,
               javascript = T,
               nativeEvents = T,
               extraCapabilities = eCap)
remDr <- rD[["client"]]

# remDr$navigate("https://global-1factiva-1com-1guyfu4jo0374.hertie.hh-han.com/ha/default.aspx#./!?&_suid=1648997101625040704780695491305")
# # source local file that 1) calls institutional login site, 2) inputs credentials, 3) logs in, and 4) forwards you to Factiva Home Page (/Pages/Index)
# base::source("01_credentials.R")

# Manually (to avoid detection):
  # 1) go to http://hertie.hh-han.com/HAN-AtoZ/atoz-complete.html
  # 2) select Factiva Database
  # 3) log in with own credentials
  # 4) wait for redirect
  # 5) Ctrl + W / Close new tab / navigate to tab 1
  
# ToDo: Maybe it would work with the updated code, try out later!


# go to search page:
search_page <- "https://global-1factiva-1com-1guyfu4be001b.hertie.hh-han.com/sb/default.aspx?NAPC=S"

newspaper <- c("sddz", "dwelt", "taz", "zbild", "frarun")
candidate <- c("Laschet", "Baerbock", "Scholz")
dates <- c("20210101 to 20210131",
           "20210201 to 20210228",
           "20210301 to 20210331",
           "20210401 to 20210430",
           "20210501 to 20210531",
           "20210601 to 20210630",
           "20210701 to 20210731",
           "20210801 to 20210831",
           "20210901 to 20210930",
           "20211001 to 20211031",
           "20211101 to 20211130",
           "20211201 to 20211231")

results_factiva <- list()
results_factiva_txt <- c()

{
for (i in 1:length(newspaper)) {
  for (j in 1:length(candidate)) {
    # if (j != 1) {repair = 1} # delete later
    for (k in 1:length(dates)) {
    
query <- paste0("la=de and rst=", newspaper[i], " and date from ", dates[k], " and ", candidate[j])
    
remDr$navigate(search_page)

tryCatch({remDr$acceptAlert}, error = function(e){NULL})

if (remDr$getCurrentUrl() == "https://login.hertie.hh-han.com/login/login.html") {
  
  # log in again
  webElemUser <- remDr$findElement(using = "name", "plainuser")
  webElemUser$sendKeysToElement(list(user))
  # webElemUser$sendKeysToActiveElement(list(key = "tab"))
  
  webElemKey <- remDr$findElements(using = "name", "password")[[1]]
  webElemKey$sendKeysToElement(list(key))
  webElemKey$sendKeysToElement(list(key = "enter"))
  
  tryCatch({remDr$acceptAlert}, error = function(e){NULL})
  
  # wait for like 1000 redirects...
  Sys.sleep(30)
  
  # search again
  remDr$navigate(search_page)
  
  # enter search term
  prompt <- remDr$findElement("css selector", '.ace_text-input')
  prompt$sendKeysToElement(list(query))
  
  # don't limit timeframe
  alltime <- remDr$findElement(using = 'xpath', "//select[@id='dr']/option[@value='_Unspecified']")
  alltime$clickElement()
  
  # click search:
  search <- remDr$findElement("css selector", ".standardBtn , #btnSBSearch span")
  search$clickElement()
  
  tryCatch({remDr$acceptAlert}, error = function(e){NULL})
}

# wait for like 1000 redirects...
Sys.sleep(10)

if (remDr$getCurrentUrl() != search_page) {
  remDr$navigate(search_page)
  tryCatch({remDr$acceptAlert}, error = function(e){NULL})
  Sys.sleep(5)
}

# wait until loaded
# while (is.null(alltime)) {
  
  # don't limit timeframe  
alltime <- 
#  tryCatch({
    remDr$findElement(using = 'xpath', "//select[@id='dr']/option[@value='_Unspecified']")
# }, error = function(e){NULL})
# } 
  alltime$clickElement()


# enter search term
prompt <- remDr$findElement("css selector", '.ace_text-input')
prompt$sendKeysToElement(list(query))

# click search:
search <- remDr$findElement("css selector", ".standardBtn , #btnSBSearch span")
search$clickElement()

tryCatch({remDr$acceptAlert}, error = function(e){NULL})

# powernap:
Sys.sleep(10)

if (remDr$getCurrentUrl() == "https://login.hertie.hh-han.com/login/login.html") {
  
  # log in again
  webElemUser <- remDr$findElement(using = "name", "plainuser")
  webElemUser$sendKeysToElement(list(user))
  # webElemUser$sendKeysToActiveElement(list(key = "tab"))
  
  webElemKey <- remDr$findElements(using = "name", "password")[[1]]
  webElemKey$sendKeysToElement(list(key))
  webElemKey$sendKeysToElement(list(key = "enter"))
  
  # wait for like 1000 redirects...
  Sys.sleep(30)
  
  # search again
  remDr$navigate(search_page)
  
  # enter search term
  prompt <- remDr$findElement("css selector", '.ace_text-input')
  prompt$sendKeysToElement(list(query))
  
  # don't limit timeframe
  alltime <- remDr$findElement(using = 'xpath', "//select[@id='dr']/option[@value='_Unspecified']")
  alltime$clickElement()
  
  # click search:
  search <- remDr$findElement("css selector", ".standardBtn , #btnSBSearch span")
  search$clickElement()
}

# get data:
# ToDo: make work as function (with correct output!)
page_source <- remDr$getPageSource()[[1]]

page <- page_source %>% 
  read_html() 

# just to have an alternative format:
txt <- page_source %>% read_html() %>% html_elements("#headlineFrame") %>% html_text2() 

results_factiva_txt <- append(results_factiva_txt, txt)

results_factiva <- list.append(results_factiva, page)



# test if there are more results:
more <- tryCatch({remDr$findElement("css selector", ".nextItem")},
                 error = function(e){NULL})


while(!is.null(more)) {
  
  more$clickElement()
  tryCatch({remDr$acceptAlert}, error = function(e){NULL})
   Sys.sleep(3)
  
  # get data:
  # ToDo: make work as function (with correct output!)
  page_source <- remDr$getPageSource()[[1]]
    page <- page_source %>% 
      read_html() 
  # just to have an alternative format:
  txt <- page_source %>% read_html() %>% html_elements("#headlineFrame") %>% html_text2() 
  results_factiva_txt <- append(results_factiva_txt, txt)
  results_factiva <- list.append(results_factiva, page)
  

    more <- tryCatch({remDr$findElements("css selector", ".nextItem")},
                   error = function(e){})
    if(purrr::is_empty(more) | is.null(more)){ 
          more <-  NULL}
    more <- more[[1]]
      }
    }
  }
}

list.save(results_factiva_candidates, "articles/results_factiva_candidates.rds")
save(results_factiva_candidates_txt, file = "articles/results_factiva_candidates_txt.csv")

}

### flatten list objects to df (works well, so just leave it the base-R way for now) ####

df <- tibble()
for (i in length(corpus)) {
  df <- df %>% rbind(.,
    tibble(
      id = corpus[[i]][["meta"]][["id"]],
      heading = corpus[[i]][["meta"]][["heading"]],
      source = corpus[[i]][["meta"]][["origin"]],
      mention = names(corpus[i]),
      author = corpus[[i]][["meta"]][["author"]],
      resort = na_if(str_c(corpus[[i]][["meta"]][["section"]], collapse = ","), ""),
      wordcount = corpus[[i]][["meta"]][["wordcount"]],
      page = as.numeric(corpus[[i]][["meta"]][["page"]]),
      date = corpus[[i]][["meta"]][["datetimestamp"]],
      text = str_c(corpus[[i]]$content, collapse = " ")
      ) 
    )
}

# scrape number of articles in general (no matter of candidates mentioned): ####
{
results_factiva_all <- list()
results_factiva_all_txt <- c()

  repair = 1 
  repair_i <- 5
  
  for (i in repair_i:length(newspaper)){
    # in case of error, manually finish inner loop run and set repair to next k to proceed
    # if (i != 5) {repair = 1} # delete later (or change condition to: i != i[where error happened])
      for (k in repair:length(dates)){
        
        query <- paste0("la=de and rst=", newspaper[i], " and date from ", dates[k])
        
        remDr$navigate(search_page)
        
        tryCatch({remDr$acceptAlert}, error = function(e){NULL})
        
        if (remDr$getCurrentUrl() == "https://login.hertie.hh-han.com/login/login.html") {
          
          # log in again
          webElemUser <- remDr$findElement(using = "name", "plainuser")
          webElemUser$sendKeysToElement(list(user))
          # webElemUser$sendKeysToActiveElement(list(key = "tab"))
          
          webElemKey <- remDr$findElements(using = "name", "password")[[1]]
          webElemKey$sendKeysToElement(list(key))
          webElemKey$sendKeysToElement(list(key = "enter"))
          
          tryCatch({remDr$acceptAlert}, error = function(e){NULL})
          
          # wait for like 1000 redirects...
          Sys.sleep(30)
          
          # search again
          remDr$navigate(search_page)
          
          # enter search term
          prompt <- remDr$findElement("css selector", '.ace_text-input')
          prompt$sendKeysToElement(list(query))
          
          # don't limit timeframe
          alltime <- remDr$findElement(using = 'xpath', "//select[@id='dr']/option[@value='_Unspecified']")
          alltime$clickElement()
          
          # click search:
          search <- remDr$findElement("css selector", ".standardBtn , #btnSBSearch span")
          search$clickElement()
          
          tryCatch({remDr$acceptAlert}, error = function(e){NULL})
        }
        
        # wait for like 1000 redirects...
        Sys.sleep(10)
        
        if (remDr$getCurrentUrl() != search_page) {
          remDr$navigate(search_page)
          tryCatch({remDr$acceptAlert}, error = function(e){NULL})
          Sys.sleep(5)
        }
        
        # wait until loaded
        # while (is.null(alltime)) {
        
        # don't limit timeframe  
        alltime <- 
          #  tryCatch({
          remDr$findElement(using = 'xpath', "//select[@id='dr']/option[@value='_Unspecified']")
        # }, error = function(e){NULL})
        # } 
        alltime$clickElement()
        
        
        # enter search term
        prompt <- remDr$findElement("css selector", '.ace_text-input')
        prompt$sendKeysToElement(list(query))
        
        # click search:
        search <- remDr$findElement("css selector", ".standardBtn , #btnSBSearch span")
        search$clickElement()
        
        tryCatch({remDr$acceptAlert}, error = function(e){NULL})
        
        # powernap:
        Sys.sleep(10)
        
        if (remDr$getCurrentUrl() == "https://login.hertie.hh-han.com/login/login.html") {
          
          # log in again
          webElemUser <- remDr$findElement(using = "name", "plainuser")
          webElemUser$sendKeysToElement(list(user))
          # webElemUser$sendKeysToActiveElement(list(key = "tab"))
          
          webElemKey <- remDr$findElements(using = "name", "password")[[1]]
          webElemKey$sendKeysToElement(list(key))
          webElemKey$sendKeysToElement(list(key = "enter"))
          
          # wait for like 1000 redirects...
          Sys.sleep(30)
          
          # search again
          remDr$navigate(search_page)
          
          # enter search term
          prompt <- remDr$findElement("css selector", '.ace_text-input')
          prompt$sendKeysToElement(list(query))
          
          # don't limit timeframe
          alltime <- remDr$findElement(using = 'xpath', "//select[@id='dr']/option[@value='_Unspecified']")
          alltime$clickElement()
          
          # click search:
          search <- remDr$findElement("css selector", ".standardBtn , #btnSBSearch span")
          search$clickElement()
        }
        
        # get data:
        # ToDo: make work as function (with correct output!)
        page_source <- remDr$getPageSource()[[1]]
        
        page <- page_source %>% 
          read_html() 
        
        # just to have an alternative format:
        txt <- page_source %>% read_html() %>% html_elements("#headlineFrame") %>% html_text2() 
        
        results_factiva_all_txt <- append(results_factiva_all_txt, txt)
        
        results_factiva_all <- list.append(results_factiva_all, page)
        
        
# run manually if loop broke, then set repair and start full loop again  
      {
        # test if there are more results:
          # ToDo: find out why it sometimes breaks here!
        more <- tryCatch({remDr$findElement("css selector", ".nextItem")},
                         error = function(e){NULL})
        if(purrr::is_empty(more) | is.null(more)){ 
          more <-  NULL}
        
        
        while(!is.null(more)) {
          
          more$clickElement()
          tryCatch({remDr$acceptAlert}, error = function(e){NULL})
          Sys.sleep(3)
          
          # get data:
          # ToDo: make work as function (with correct output!)
          page_source <- remDr$getPageSource()[[1]]
          page <- page_source %>% 
            read_html() 
          # just to have an alternative format:
          txt <- page_source %>% read_html() %>% html_elements("#headlineFrame") %>% html_text2() 
          results_factiva_all_txt <- append(results_factiva_all_txt, txt)
          results_factiva_all <- list.append(results_factiva_all, page)
          
          
          more <- tryCatch({remDr$findElements("css selector", ".nextItem")},
                           error = function(e){})
          if(purrr::is_empty(more) | is.null(more)){ 
            more <-  NULL}
          more <- more[[1]]
        }
        }
      }
    }
  
list.save(results_factiva_all, "articles/results_factiva_all.rds")
save(results_factiva_all_txt, file = "articles/results_factiva_all_txt.csv")

}




##### e.g. using purrr works gives nice output, but only for 3 rows... ####
library(purrr)
t1 <- tibble(map(
  id = corpus, list("meta", "id")),
  heading = map(corpus, list("meta", "heading")))

##### weird stuff...
names(corpus[1])
test <- data.frame(text = unlist(sapply(corpus, `[[`, "content"))) %>% 
data.frame(metadata = unlist(sapply(corpus, `[[`, "meta")))


test2 <- data.frame(metadata = unlist(sapply(corpus, `[[`, "meta")))

test_3 <- tibble(article = unlist(corpus[[1]]))

## Sadly, FAZ was not included in the Academic Factiva Account, therefore those articles were scraped from the FAZ's Archive directly ####
library(rvest)

# loop over docs/create list of docs
  # ToDo!
doc <- read_html("Articles/F.A.Z.-Bibliotheksportal.html", encoding = "UTF-8") 

# function to get variables:
extract_variables_faz <- function(doc = doc){
doc %>% html_elements("pre.docSource") %>% html_text2() %>% 
    str_split(",") %>% 
    unlist() %>% matrix(ncol = 4, byrow = T, dimnames = list(c(), c("source", "date", "delete", "page"))) %>% data.frame() %>% 
 mutate(page = str_extract(string = page, pattern =  "\\d"),
        date = as.POSIXlt(date, format = "%d.%m.%Y")) %>% 
    bind_cols(., 
              doc %>% html_elements("pre.docTitle") %>% html_text2() %>% 
                tibble() %>% rename(., "heading" = "."),
              doc %>% html_elements(".docTitle+ .text , .docBoxTeaser+ .text") %>% html_text2() %>% 
                tibble() %>% rename(., "text" = "."),
              doc %>% html_elements("tr:nth-child(3) td+ td")%>% html_text2() %>% 
                tibble() %>% rename(., "resort" = "."),
              id = NA,
              wordcount = NA
              ) %>% 
  select(id, heading, source, resort, wordcount, page, date, text, -delete)
}

# vectorize it:
doclist <- list(doc)
df_faz <- map_dfr(doclist, extract_variables_faz)

# get polling data: ####
