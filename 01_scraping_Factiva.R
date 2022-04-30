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
p_load(writexl)

## Due to Factiva download limit, just scrape search summaries (and get links to article pages!) ####

# avoid being blocked:
eCap <- list(
  phantomjs.page.settings.userAgent = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:29.0) Gecko/20120101 Firefox/29.0"
)

# basic constants:

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

# start RD:
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
rD <- rsDriver(browser = "chrome",
               chromever = "99.0.4844.51",
               verbose = F,
               javascript = T,
               nativeEvents = T,
               extraCapabilities = eCap)

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

remDr <- rD[["client"]]

# ToDo: Make this whole thing a function!
{
# results_factiva <- list()
# results_factiva_txt <- c()
# candidates <- c()

i = 1
j = 1
k = 1
m = 0

links <- c()
source <- c()
info <- c()
lead <- c()
person <- c()
month <- c()
header <- c()

  repair_i = 1
  repair_j = 1
  repair_k = 1
  
  # ToDo: make this run again second when loop breaks after setting repair_k = k+1 and repair_i = ifelse(repair_k > length(dates), i+1, i)
  {  
    for (i in repair_i:length(newspaper)){
      # in case of error, manually finish inner loop run and set repair to next k to proceed
     if(i != repair_i) {repair_j = 1} # delete later (or change condition to: i != i[where error happened] - does it work as is?)
        for (j in repair_j:length(candidate)) {
      if (j != repair_j) {repair_k = 1} # delete later
          for (k in repair_k:length(dates)) {
    
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

if (remDr$getCurrentUrl() != search_page) {
  remDr$navigate(search_page)
  tryCatch({remDr$acceptAlert}, error = function(e){NULL})
}

Sys.sleep(5)

# ToDo: wait until loaded would be better
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
Sys.sleep(5)

# ToDo: turn into function to avoid repetition
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

writeLines(page_source, paste0("articles/search_results_html/factiva_", newspaper[i], candidate[j] , month.abb[k], "0.txt"), useBytes = T)

page <- page_source %>% 
  read_html() 

# alternative formats:
 # txt <- page_source %>% read_html() %>% html_elements("#headlineFrame") %>% html_text2() 

  # results_factiva_txt <- append(results_factiva_txt, txt)
# candidates <- append(candidates, candidate[j])
# results_factiva <- list.append(results_factiva, page)

new_links_factiva = page %>% 
  html_nodes(".deHeadline") %>% html_attr('href')

# prepare data frame
for (l in 1:length(new_links_factiva)) {
  # article_links_factiva <- append(article_links_factiva, new_links_factiva)
  links <- append(links,
                  paste0(base_url, 
                         str_sub(new_links_factiva[l], 3)) %>%  
                    as.character())
  source <- append(source, 
                   newspaper[i])
  info <- append(info,
                 page%>% 
                   html_element(xpath = paste0('//*[@id="headlines"]/table/tbody/tr[', l, ']/td[3]/div[1]/text()')) %>% 
                   html_text2() %>% as.character())
  lead <- append(lead, 
                 page %>% 
                   html_element(xpath = paste0('//*[@id="headlines"]/table/tbody/tr[', l, ']/td[3]/div[2]/text()')) %>% 
                   html_text2() %>% as.character())
  person <- append(person,
                   candidate[j])
  month <- append(month, 
                  k)
  header <- append(header,
                   page %>% 
                     html_element(css = ".deHeadline") %>% html_text2())
}

# run manually if loop broke, then set repair and start full loop again  
  # ToDO: make this run automatically when loop breaks 
{
  # test if there are more results:
  # ToDo: find out why it sometimes breaks here!
  more <- tryCatch({remDr$findElement("css selector", ".nextItem")},
                   error = function(e){NULL})
  if(purrr::is_empty(more) | is.null(more)){ 
    more <-  NULL}
  if(class(more) == "list") {
    more <- more[[1]]} 

while(!is.null(more)) {
  
  more$clickElement()
  tryCatch({remDr$acceptAlert}, error = function(e){NULL})
   Sys.sleep(3)
  
  m = m+1
   
  # get data:
  # ToDo: make work as function (with correct output!)
  page_source <- remDr$getPageSource()[[1]]
  
  writeLines(page_source, paste0("articles/search_results_html/factiva_", newspaper[i], candidate[j] , month.abb[k], m, ".txt"), useBytes = T)
  
    page <- page_source %>% 
      read_html() 
    
# alternative formats:
    # txt <- page_source %>% read_html() %>% html_elements("#headlineFrame") %>% html_text2() 
    # results_factiva_txt <- append(results_factiva_txt, txt)
  # results_factiva <- list.append(results_factiva, page) # other formats preferable (saving & reloading!)
  candidates <- append(candidates, candidate[j])
  
  
  new_links_factiva = page %>% 
        html_nodes(".deHeadline") %>% html_attr('href')
  
  # get df directly:
  
      for (l in 1:length(new_links_factiva)) {
        # article_links_factiva <- append(article_links_factiva, new_links_factiva)
        links <- append(links,
                        paste0(base_url, 
                               str_sub(new_links_factiva[l], 3)) %>%  
                          as.character())
        source <- append(source, 
                         newspaper[i])
        info <- append(info,
                       page%>% 
                         html_element(xpath = paste0('//*[@id="headlines"]/table/tbody/tr[', l, ']/td[3]/div[1]/text()')) %>% 
                         html_text2() %>% as.character())
        lead <- append(lead, 
                       page %>% 
                         html_element(xpath = paste0('//*[@id="headlines"]/table/tbody/tr[', l, ']/td[3]/div[2]/text()')) %>% 
                         html_text2() %>% as.character())
        person <- append(person,
                         candidate[j])
        month <- append(month, 
                        k)
        header <- append(header,
                         page %>% 
                           html_element(css = ".deHeadline") %>% html_text2())
      }
  

    more <- tryCatch({remDr$findElements("css selector", ".nextItem")},
                   error = function(e){})
    if(purrr::is_empty(more) | is.null(more)){ 
          more <-  NULL}
    
    if (class(more) == "list") {
      more <- more[[1]]} 
        
}
  m = 0
}

    
    }
  }
}
save.image("all_data.RData")
df_factiva_candidates <- tibble(header,
                                links,
                                source,
                                info,
                                lead,
                                person,
                                month)



write_xlsx(df_factiva_candidates, "articles/factiva_data_candidates.xlsx")

}
}

# fucked up the variables probably (some will be missing), but can get them from txt files!!!

# scrape all articles (no matter of candidates mentioned): ####

{
# results_factiva_all <- list()
results_factiva_all_txt <- c()
 
links <- c()
source <- c()
info <- c()
lead <- c()
person <- c()
month <- c()
header <- c()

  repair_k = 9
  repair_i = 5
  # depending on where loop broke (usually some "more") potentially set m=m-i 
  
  # make this run again second when loop breaks after setting repair_k = k+1 and repair_i = ifelse(repair_k > length(dates), i+1, i)
  {  
  for (i in repair_i:length(newspaper)){
    # in case of error, manually finish inner loop run and set repair to next k to proceed
    if (i != repair_i) {repair_k = 1} # delete later (or change condition to: i != i[where error happened] - does it work as is?)
      for (k in repair_k:length(dates)){
        
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
        
        if (remDr$getCurrentUrl() != search_page) {
          remDr$navigate(search_page)
          tryCatch({remDr$acceptAlert}, error = function(e){NULL})
        }
        Sys.sleep(5)
        
        # ToDo: better wait until loaded
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
        Sys.sleep(5)
        
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
        
        # save search result page:
        writeLines(page_source, paste0("articles/search_results_html/factiva_", newspaper[i], month.abb[k], m, ".txt"), useBytes = T)
        
        page <- page_source %>% 
          read_html() 
        
        # just to have an alternative format:
        txt <- page_source %>% read_html() %>% html_elements("#headlineFrame") %>% html_text2() 
        
        results_factiva_all_txt <- append(results_factiva_all_txt, txt)
        # results_factiva_all <- list.append(results_factiva_all, page) # other formats preferable (easier to save and reload!)
        
        # get df directly:
        new_links_factiva = page %>% 
          html_nodes(".deHeadline") %>% html_attr('href')
        
        # prepare df:
        for (l in 1:length(new_links_factiva)) {
          # article_links_factiva <- append(article_links_factiva, new_links_factiva)
          links <- append(links,
                          paste0(base_url, 
                                 str_sub(new_links_factiva[l], 3)) %>%  
                            as.character())
          source <- append(source, 
                           newspaper[i])
          info <- append(info,
                         page %>% 
                           html_element(xpath = paste0('//*[@id="headlines"]/table/tbody/tr[', l, ']/td[3]/div[1]/text()')) %>% 
                           html_text2() %>% as.character())
          lead <- append(lead, 
                         page %>% 
                           html_element(xpath = paste0('//*[@id="headlines"]/table/tbody/tr[', l, ']/td[3]/div[2]/text()')) %>% 
                           html_text2() %>% as.character())
          month <- append(month, 
                          k)
          header <- append(header,
                           page %>% 
                             html_element(css = ".deHeadline") %>% html_text2())
        }
        
    # run manually if loop broke, then set repair and start full loop again  
        # ToDO: make this run automatically when loop breaks 
        {
          # test if there are more results:
          # ToDo: find out why it sometimes breaks here!
          more <- tryCatch({remDr$findElement("css selector", ".nextItem")},
                           error = function(e){NULL})
          if(purrr::is_empty(more) | is.null(more)){ 
            more <-  NULL}
          if (class(more) != "webElement" ) {
            more <- more[[1]]
            } 
          
          while(!is.null(more)) {
            
            more$clickElement()
            tryCatch({remDr$acceptAlert}, error = function(e){NULL})
            Sys.sleep(3)
            
            m = m+1
            
            # get data:
            # ToDo: make work as function (with correct output!)
            page_source <- remDr$getPageSource()[[1]]
            page <- page_source %>% 
              read_html() 
            
            writeLines(page_source, paste0("articles/search_results_html/factiva_", newspaper[i], month.abb[k], m, ".txt"), useBytes = T)
            
            # alternative formats:
            txt <- page_source %>% read_html() %>% html_elements("#headlineFrame") %>% html_text2()           
            
            results_factiva_all_txt <- append(results_factiva_all_txt, txt)
            # results_factiva_all <- list.append(results_factiva_all, page) # this cannot easily be exported, other formats (txt) preferable
          
            
           
          # get df directly:
             new_links_factiva = page %>% 
              html_nodes(".deHeadline") %>% html_attr('href')
            
            # save variables:
            for (l in 1:length(new_links_factiva)) {
              # article_links_factiva <- append(article_links_factiva, new_links_factiva)
              links <- append(links,
                              paste0(base_url, 
                                     str_sub(new_links_factiva[l], 3)) %>%  
                                as.character())
              source <- append(source, 
                               newspaper[i])
              info <- append(info,
                             page %>% 
                               html_element(xpath = paste0('//*[@id="headlines"]/table/tbody/tr[', l, ']/td[3]/div[1]/text()')) %>% 
                               html_text2() %>% as.character())
              lead <- append(lead, 
                             page %>% 
                               html_element(xpath = paste0('//*[@id="headlines"]/table/tbody/tr[', l, ']/td[3]/div[2]/text()')) %>% 
                               html_text2() %>% as.character())
              month <- append(month, 
                              k)
              header <- append(header,
                               page %>% 
                                 html_element(css = ".deHeadline") %>% html_text2())
            }
            
          
            more <- tryCatch({remDr$findElement("css selector", ".nextItem")},
                             error = function(e){NULL})
            if(purrr::is_empty(more) | is.null(more)){ 
              more <-  NULL}
         if (class(more) != "webElement" ) {
           more <- more[[1]]
            } 
          }
          m = 0
        }
    # end of troubleshoot run
      }
    } # end of loop
  } # end of troubleshoot re-run
  

save.image("all_data.RData")

df_factiva_all_probably_wrong <- 
              tibble(header,
                     links,
                     source,
                     info,
                     lead,
                     month)



write_xlsx(df_factiva, "articles/factiva_data.xlsx")

} # end of method 






### missed articles (by hand) ####
# open RS

# i = 4
# k = 3
# {
# query <- paste0("la=de and rst=", newspaper[i], " and date from ", dates[k])
# query
# remDr$navigate(search_page)
# prompt <- remDr$findElement("css selector", '.ace_text-input')
# prompt$sendKeysToElement(list(query))
# 
# alltime <- remDr$findElement(using = 'xpath', "//select[@id='dr']/option[@value='_Unspecified']")
# alltime$clickElement()
# 
# 
# # click search:
# search <- remDr$findElement("css selector", ".standardBtn , #btnSBSearch span")
# search$clickElement()
# }
# # navigate manually to missing results
# page <- remDr$getPageSource()[[1]]
# writeLines(page,
#            paste0("articles/search_results_html_missed/factiva", newspaper[i], month.abb[k], 1, ".txt"), useBytes = T)



# Saving html ####

# # save html as txt:
# for (i in 1:length(factiva_articles)) {
#   writeLines(factiva_articles$.[i], paste0("articles/html/factiva", i, ".txt"), useBytes = T)
# }
# 
# # get back html:
# html_factiva_txt <- c()
# for (i in 1:length(factiva_articles)) {
#   html_factiva_txt[i] <- readr::read_file(paste0("articles/html/factiva", i, ".txt"))
# }
# 

# process data ####

# # get shown n of results into df (allows to filter out duplicates):
# readr::read_file(paste0("articles/html/factiva", i, ".txt")) %>% 
#   read_html(encoding = "UTF-8") %>% 
#   html_element(".resultsBar") %>% 
#   html_text2()
# 
# # also, get candidate/search term: 
# tmp <- readr::read_file(paste0("articles/search_results_html/factiva_", newspaper[i], candidate[j] , month.abb[k], "0.txt")) %>% 
#   read_html(encoding = "UTF-8") %>% 
#   html_element(".ellipsis.sbPreviewLable") %>% 
#   html_text2() %>% 
#   str_split(., boundary("word"), simplify = T)
# term <- tmp[13]

# db search yields 175 644 results

# create df from html documents
{ 
library(magrittr)
base_url <- "https://global-1factiva-1com-1guyfu4be004d.hertie.hh-han.com"
i = 1
n = 1


# prepare vectors to bind later:
links <- c()
source <- c()
info <- c()
lead <- c()
person <- c()
heading <- c()
result_no <- c()

files <- dir("articles/search_results_html_missed")

for (i in 1:length(files)) {

  tmp_file <- readr::read_file(paste0("articles/search_results_html_missed/", files[i])) %>% 
    read_html(encoding = "UTF-8")
    
  tmp_candidate <- tmp_file %>%
    html_element(".ellipsis.sbPreviewLable") %>%
    html_text2() %>%
    str_split(., boundary("word"), simplify = T)
  tmp_candidate <- tmp_candidate[13]
  
  new_links <- tmp_file %>% 
    html_nodes(".deHeadline") %>% 
    html_attr('href')
  
  tmp_titles <- tmp_file %>% 
    html_elements(css = ".deHeadline") %>% 
    html_text2()
  
  for (n in 1:length(new_links)) {
    
    links <- append(links,
                    paste0(base_url, 
                           str_sub(new_links[n], 3)) %>%  
                      as.character())
    heading <- append(heading,
                      tmp_titles[n] %>% 
                        as.character())
    source <- append(source,
                     tmp_file %>%
                       html_element(paste0(".headline:nth-child(", n ,") .leadFields a")) %>%
                       html_text2() %>% as.character())
    info <- append(info,
                   tmp_file %>%
                     html_element(xpath = paste0('//*[@id="headlines"]/table/tbody/tr[', n, ']/td[3]/div[1]/text()')) %>%
                     html_text2() %>% as.character())
    lead <- append(lead,
                   tmp_file %>%
                     html_element(xpath = paste0('//*[@id="headlines"]/table/tbody/tr[', n, ']/td[3]/div[2]/text()')) %>%
                     html_text2() %>% as.character())
    person <- append(person,
                     tmp_candidate)
    result_no <- append(result_no,
                        tmp_file %>%
                          html_element(".resultsBar") %>%
                          html_text2())
  }
}


# old version:
{ 
  
# # loop over search result pages:
# for (i in 1:length(results_factiva)) {
#   
#   new_links_factiva <- results_factiva[[i]] %>% 
#     html_nodes(".deHeadline") %>% html_attr('href')
# 
#   for (j in 1:length(new_links_factiva)) {
#     # article_links_factiva <- append(article_links_factiva, new_links_factiva)
#     links <- append(links,
#       paste0(base_url, 
#                    str_sub(new_links_factiva[j], 3)) %>%  
#            as.character())
#     source <- append(source, 
#       results_factiva[[i]] %>% 
#            html_element(paste0(".headline:nth-child(", j ,") .leadFields a")) %>% 
#            html_text2() %>% as.character())
#     info <- append(info,
#       results_factiva[[i]] %>% 
#            html_element(xpath = paste0('//*[@id="headlines"]/table/tbody/tr[', j, ']/td[3]/div[1]/text()')) %>% 
#            html_text2() %>% as.character())
#     lead <- append(lead, 
#       results_factiva[[i]] %>% 
#            html_element(xpath = paste0('//*[@id="headlines"]/table/tbody/tr[', j, ']/td[3]/div[2]/text()')) %>% 
#            html_text2() %>% as.character())
#     person <- append(person,
#                      candidates[i])
#     heading <- append(heading,
#                       results_factiva[[i]] %>% 
#                         html_element(css = ".deHeadline") %>% html_text2()
#     )
#   }
# }
}

# save raw:
df_factiva_missing <- tibble(heading, links, source, info, lead, person, result_no)


# function to translate written german months into numbers: 
full_german_month_to_mm <- function(month){
  case_when(month == "Januar" ~ 1,
            month == "Februar" ~ 2,
            month == "März" ~ 3,
            month == "April" ~ 4, 
            month == "Mai" ~ 5,
            month == "Juni" ~ 6,
            month == "Juli" ~ 7,
            month == "August" ~ 8,
            month == "September" ~ 9,
            month == "Oktober" ~ 10,
            month == "November" ~ 11,
            month == "Dezember" ~ 12) %>% 
    sprintf(fmt = "%02d", .) %>% 
    as.numeric()
  }

# get formatted date:
df_factiva %<>% 
  mutate(., 
    dd = str_split(info, boundary("word"), simplify = T)[,1] %>% 
      as.numeric() %>% 
      sprintf(fmt = "%02d", .) %>% 
      as.numeric(),
    month = str_split(info, boundary("word"), simplify = T)[,2],
    mm = full_german_month_to_mm(month),
    yyyy = 2021,
    date = paste0(dd, "-", mm, "-", yyyy) %>% dmy()
  ) %>% 
select(-c(info, yyyy))

}

write_xlsx(df_factiva, "articles/factiva_data.xlsx")

# join multiple candidate mentions: ####

df_Baerbock <- df_factiva %>% filter(person == "Baerbock") 
df_Laschet <- df_factiva %>% filter(person == "Laschet") 
df_Scholz <- df_factiva %>% filter(person == "Scholz")

df_candidates <- full_join(df_Baerbock, df_Laschet, by = c("source", "heading", "date", "lead", "links", "dd", "month", "mm"), suffix = c("_Baerbock", "_Laschet"))
df_candidates <- full_join(df_candidates, df_Scholz, by = c("source", "heading", "date", "lead", "links", "dd", "month", "mm"), suffix = c("_Scholz", ""))

# filter unique: ####

# CHECK ERRORS:
  # potential solution: read all to df %>% unique(header/link, date, source, result_no) (and identify which ones were duplicated)
  # FR: limit corpus to Deutschlandausgabe?
  
# overview_cases <- 
#   df_factiva %>% 
#   mutate(month = full_german_month_to_mm(month)) %>% 
#   group_by(person, source, month, result_no) %>% 
#   summarise(n = n()) %>% 
#   arrange(source, person, month, result_no)
# write_xlsx(overview_cases, "articles/overview_cases.xlsx")



### get full texts, but only for candidate related articles alone  ####
# for all articles about 750 000 links would have to be scraped (with 1 per sec that's > 8 days) - links to those are still provided


# get page html as string: 

# start server again:
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
rD <- rsDriver(browser = "chrome",
               chromever = "99.0.4844.51",
               verbose = F,
               javascript = T,
               nativeEvents = T,
               extraCapabilities = eCap)

# log in manually (just to be sure) or create method for institutional login
remDr <- rD[["client"]]

# function to get full articles ####
{  ID = 1
  get_articles_factiva <- function(link, sleepmin = 1.5, sleepmax = 2.5){
    
    remDr$navigate(link)
     
    randsleep <- sample(seq(sleepmin, sleepmax, by = 0.001), 1)
    Sys.sleep(randsleep)
    
    while (remDr$getCurrentUrl()[[1]] != "https://global-1factiva-1com-1guyfu4cl017f.hertie.hh-han.com/ga/default.aspx") {
      remDr$navigate(link)
      
      tryCatch({remDr$acceptAlert}, error = function(e){NULL})
      
      randsleep <- sample(seq(sleepmin, sleepmax, by = 0.001), 1)
      Sys.sleep(randsleep)
    }
    
    page <- remDr$getPageSource()[[1]]
    
    # page_source_html <- page_source %>% read_html()
    
    writeLines(page, paste0("articles/html/factiva", ID, ".txt"), useBytes = T)
    
    ID = ID+1
    
    # alternatively: return list of html & txt:
    # page_source <- list(page_source_html, page_source)
    
    return(tibble(page))
  }

  factiva_articles <- map_dfr(df_candidates$links, get_articles_factiva, .id = "ID")

  
  save.image("all_data.RData")
  
  # save html as txt:
  for (ID in 1:length(factiva_articles)) {
    tmp = factiva_articles$.[ID] %>% read_html(., encoding = "UTF-8")
    writeLines(factiva_articles$.[ID], paste0("articles/html/factiva", ID, ".txt"), useBytes = T)
  }
  
}



# as loop:

ID_repair = 1
while(length(dir("articles/html")) < length(df_candidates$links)){

try(
for (ID in ID_repair:length(df_candidates$links)){
  
  link <- df_candidates$links[ID]

remDr$navigate(link)


error <- tryCatch({remDr$findElement("css selector", "#pageErrorContainer")},
         error = function(e){T})

while(!isTRUE(error)){
  
  try({
    ok <- remDr$findElement("css selector", ".prettyBtn.primaryBtn")
    ok$clickElement()
  })
  
  randsleep <- sample(seq(sleepmin, sleepmax, by = 0.001), 1)
  Sys.sleep(randsleep)
  
  remDr$navigate(link)
  
  error <- tryCatch({remDr$findElement("css selector", "#pageErrorContainer")},
                    error = function(e){T})
  
  tryCatch({remDr$acceptAlert}, error = function(e){})
}

# while (remDr$getCurrentUrl()[[1]] != "https://global-1factiva-1com-1guyfu4cl017f.hertie.hh-han.com/ga/default.aspx") {
#   remDr$navigate(link)
#   
#   randsleep <- sample(seq(sleepmin, sleepmax, by = 0.001), 1)
#   Sys.sleep(randsleep)
# }

error <- tryCatch({remDr$findElement("css selector", "#pageErrorContainer")},
                  error = function(e){T})

while(!isTRUE(error)){
  
  try({
    ok <- remDr$findElement("css selector", ".prettyBtn.primaryBtn")
    ok$clickElement()
  })
  
  randsleep <- sample(seq(sleepmin, sleepmax, by = 0.001), 1)
  Sys.sleep(randsleep)
  
  remDr$navigate(link)
  
  error <- tryCatch({remDr$findElement("css selector", "#pageErrorContainer")},
                    error = function(e){T})
  
  tryCatch({remDr$acceptAlert}, error = function(e){})
  
}

page <- remDr$getPageSource()[[1]]

writeLines(page, paste0("articles/html/factiva", ID, ".txt"), useBytes = T)

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

}
  )
  ID_repair = ID+1
}




