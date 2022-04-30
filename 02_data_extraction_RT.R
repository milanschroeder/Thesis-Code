# 02 RT pages to Data

library(pacman)
p_load(tidyverse, rvest, magrittr)

# ToDo: ####
  # convert RT search results to data
  # clean dates
  # wordcount
  # keyword variables
  # sentiment?
  # merge sources
  # analyse :D

### ToDo: convert RT_searches to df!!! ####  


# date not possible to obtain that easily via CSS...
{
# corpus_RT <- tibble()
# 
# for (i in dir("articles/RT_searches/")) {
#   
#   doc <- readr::read_file(paste0("articles/RT_searches/", i)) %>% 
#     read_html(encoding = "UTF-8")
#   
#   header <-  doc %>% html_elements(".HeaderNews-type_5 .Link-isFullCard") %>% html_text2()
#   
#   for (j in length(header)) {
#   
#   corpus_RT %<>% 
#     append(., 
#       date =  doc %>% html_elements(".ArticleView-timestamp .Timestamp-default") %>% html_text2(),
#       
#       heading = doc %>% 
#         html_elements(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "HeaderNews-type_5", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "Link-isFullCard", " " ))]') %>% 
#         html_text2(),
#       heading = doc %>% html_elements(".HeaderNews-type_5 .Link-isFullCard") %>% html_text2(),
#       
#       lead = doc %>% html_elements(".Text-root.Text-type_4") %>% html_text2(),
#       
#       link = doc %>% html_nodes(".HeaderNews-type_5 .Link-isFullCard") %>% html_attr("href"),
#       
#       resort = link %>% str_extract(., "([^/]+)"),
#       
#       id = link %>% str_extract(., "[0-9]+"),
#       
#       resort = NA,
#       wordcount = NA,
#       source = "RTDE",
#       page = NA,
#       saved = i
#     )
#   }
# }
}
# hence via xpath:
{# 
  # header = doc %>% html_elements(.,
  #   xpath = 
  #     paste0("/html/body/div[1]/main/div/section/div[1]/div[5]/div[1]/div[", 1 ,"]/div[2]/div/article/div[2]/div[1]/div[1]")) %>% 
  #   html_text2()  
  # # length == length(section) == [1:3]
  # 
  # 
  # leading = doc %>% html_elements(.,
  #  xpath = 
  #    paste0("/html/body/div[1]/main/div/section/div[1]/div[5]/div[1]/div[1]/div[2]/div/article/div[2]/div[1]/div[2]")) %>% 
  #   html_text2()    
  # # length == length(section) == [1:3]
  # lead = leader[i]
  # 
  # doc %>% html_elements(.,
  #   xpath = paste0("/html/body/div[1]/main/div/section/div[1]/div[5]/div[1]/div[1]/div[2]/div[", i ,"]/article/div[2]/div[1]/div[1]/div/a")) %>% 
  #   html_text2() %>% print()
  # 
  # # i: article number within section
  # # j: section
  # 
  # doc %>% html_elements(.,
  # xpath = paste0("/html/body/div[1]/main/div/section/div[1]/div[5]/div[1]/div[", j, "]/div[1]")) %>% 
  #   html_text2() %>% print()
  # 
  # # header/link:  /html/body/div[1]/main/div/section/div[1]/div[5]/div[1]/div[1]/div[2]/div[ i ]/article/div[2]/div[1]/div[1]/div/a"
  # #               /html/body/div[1]/main/div/section/div[1]/div[5]/div[1]/div[1]/div[2]/div[ i ]/article/div[2]/div[1]/div[1]/div/a"
  # # lead:         /html/body/div[1]/main/div/section/div[1]/div[5]/div[1]/div[1]/div[2]/div[ i ]/article/div[2]/div[1]
  # # Date:         /html/body/div[1]/main/div/section/div[1]/div[5]/div[1]/div[ j ]/div[1]
  # 
  
} # some notes


# prepare variables here (adapt object name):
  # corpusRT_finest = tibble(`header` = "a", `lead` = "a", `link` = "a", date = "a", `resort` = "a", `id` = "a", `saved` = "a", candidate = "a")
  corpusRT_all = tibble(`header` = "a", `lead` = "a", `link` = "a", date = "a", `resort` = "a", `id` = "a", `saved` = "a", candidate = "a")

# headerRT2 = c()
# leadRT2 = c()
# dateRT2 = c()
# linkRT2 = c()
# resortRT2 = c()
# idRT2 =  c()
# saved2 = c()

# adapt path here:
for (ID in dir("articles/RT_all_searches/")) {

# ...and here:
  doc <- readr::read_file(paste0("articles/RT_all_searches/", ID)) %>% 
    read_html(encoding = "UTF-8")
  
  sections = doc %>% 
    html_elements(., xpath = paste0("/html/body/div[1]/main/div/section/div[1]/div[5]/div[1]/div/div[1]")) %>% 
    html_text2()

  for (i in 1:length(sections)) {
    
  articles <- doc %>% 
    html_elements(., xpath = paste0("/html/body/div[1]/main/div/section/div[1]/div[5]/div[1]/div[", i, "]/div[2]/div/article/div[2]/div[1]/div[1]")) %>% 
      html_text2()
  
  urls <- doc %>% 
    html_nodes(., xpath = paste0("/html/body/div[1]/main/div/section/div[1]/div[5]/div[1]/div[", i, "]/div[2]/div/article/div[2]/div[1]/div[1]/div/a")) %>% 
    html_attr("href")
  
  leadings <- doc %>% 
    html_elements(., xpath = paste0("/html/body/div[1]/main/div/section/div[1]/div[5]/div[1]/div[", i, "]/div[2]/div/article/div[2]/div[1]/div[2]")) %>% 
    html_text2()
  
    for (j in 1:length(articles)) {
      
      # tibble directly -> equal length!
      
      df_new <- tibble(
        header = articles[j] %>% as.character(),
        lead = leadings[j] %>% as.character(),
        link = urls[j] %>% as.character(),
        date = sections[i] %>% as.character(),
        resort = link %>% str_extract(., "([^/]+)") %>% as.character(),
        saved = ID %>% as.character(),
        id = link %>% str_extract(., "[0-9]+") %>% as.character(),
        candidate = saved %>% str_extract(., "([^2]*)") %>% as.character()
      ) 
      
# adapt object name here:
      corpusRT_all %<>% add_row(., df_new)
    
    # old (as vectors): 
        {      # headerRT2 %<>% append(., articles[j])
      # 
      # leadRT2 %<>% append(., leadings[j])
      # 
      # dateRT2 %<>% append(., sections [i])
      # 
      # linkRT2 %<>% append(., urls[j]) 
      #                    
      # resortRT2 %<>% append(., link %>% 
      #                        str_extract(., "([^/]+)")
      #                      )
      # 
      # idRT2 %<>% append(., link %>% 
      #                    str_extract(., "[0-9]+")
      #                  )
      # 
      # saved2 %<>% append(., ID)
      }   
    } # end of articles
  
  } # end of section

} # end of doc

# delete first row and repeated cases:
  corpusRT_all %<>% 
    filter(link != "a") %>% 
    distinct(link, candidate, .keep_all = T) %>% 
    arrange(desc(candidate))


  # ToDo: define length of ID! (?)

corpus_RT_laschet <- filter(corpusRT_all, candidate == "Laschet") %>% mutate(laschet = 1)
corpus_RT_scholz <- filter(corpusRT_all, candidate == "Scholz") %>% mutate(scholz = 1)
corpus_RT_baerbock <- filter(corpusRT_all, candidate == "Baerbock") %>% mutate(baerbock = 1)

# same as for corpus_RT_full before
corpusRT_all %<>% 
  left_join(., corpus_RT_baerbock %>% select(baerbock, saved, link), by = "link", suffix = c("", "_baerbock")) %>% 
  left_join(., corpus_RT_laschet %>% select(laschet, saved, link), by = "link", suffix = c("", "_laschet")) %>% 
  left_join(., corpus_RT_scholz %>% select(scholz, saved, link), by = "link", suffix = c("", "_scholz")) %>% 
  distinct(., id, .keep_all = T)

# rowbind -> distinct

# corpus_RT_candidates <- filter(corpusRT_full, candidate != "")
# corpus_RT_candidates %>% mutate(
#   scholz = ifelse(candidate == "Scholz", 1, 0),
#   laschet = ifelse(candidate == "Laschet", 1, 0),
#   baerbock = ifelse(candidate == "Baerbock", 1, 0)
# )






# corpus_RT2 <- tibble(
#   header = headerRT2,
#   lead = leadRT2,
#   link = linkRT2,
#   # date = dateRT,
#   # resort = resortRT,
#   # id = idRT,
#   # saved = saved,
#   wordcount = NA,
#   source = "RTDE",
#   page = NA
# )
# 
# # 4 columns have length 2413 instead of 2407. This is generally far to few!
# # one bug fixed, now its 6879/6885 -> still 6 difference
# 
# corpusRT_test <- bind_cols(
#   bind_cols(
#   headerRT2,
#   leadRT2,
#   linkRT2
#   ) %>% drop_na() %>% add_row(., `...1` = rep(NA, 14)),
# 
#   bind_cols(
#   resortRT2,
#   dateRT2,
#   idRT2,
#   saved2,
#   wordcount = NA,
#   source = "RTDE",
#   page = NA
#   )
# )

# check & clean -> bind together -> distinct


# get full texts for candidate related articles ####

RT_articles_candidates <- tibble(header = "a", lead = "a", date = "a", intextlinks = list(tibble(a = "a", b = "a")), mehrlink = list(tibble(a = "a")), document = "a", text = "a")

for (article in dir("Articles/RThtml")) {
  doc <- readr::read_file(paste0("articles/RThtml/", article)) %>% 
    read_html(encoding = "UTF-8")
  
  df_new <- tibble(
    header = doc %>% html_element(".HeadLine-type_2") %>% html_text2(),
    lead = doc %>% html_element(".Text-type_1") %>% html_text2(),
    date = doc %>% html_element(".Timestamp-default") %>% html_text2(),
    intextlinks = list(doc %>% html_nodes(".ViewText-root a") %>% html_attr("href")),
    mehrlink = list(doc %>% html_nodes("strong+ a") %>% html_attr("href")),
    document = article,
    text = doc %>% html_element(".ViewText-root") %>% html_text2()
  )
  
  RT_articles_candidates %<>% add_row(., df_new)
}

RT_articles_candidates %<>% filter(., header != "a") 

# wordcount (only here for now, cause only candidate related articles are downloaded)
RT_articles_candidates %<>% mutate(wordcount = str_count(text, "\\w+"),
                                  wordcount_baerbock = str_count(text, "Baerbock"),
                                  wordcount_laschet = str_count(text, "Laschet"),
                                  mentions_scholz = str_count(text, "Scholz"))

# test <- full_join(corpusRT_full, RT_articles_candidates, by = "header", suffix = c("", "_article"))

# merge with corpus_RT_all:
corpusRT_all %<>% left_join(., RT_articles_candidates, by = "header", suffix = c("", "_article"))


# join with corpusRT_full 
# add source column = RTDE

# merge with other fulltexts ####
RT_fulltexts <- rio::import("articles/data_rt.rds", )

# get links from saved html 

links_RT_all <- tibble(intextlinks = list(tibble(a = "a")), mehrlink = list(tibble(a = "a")), ID = 0)
for(i in 1:length(RT_fulltexts$html_page)) {
  page <- RT_fulltexts$html_page[i] %>% read_html(encoding = "UTF-8")
  
df <- tibble(
  intextlinks = list(page %>% html_elements(".ViewText-root a") %>% html_attr("href")),
  mehrlink = list(page %>% html_elements("p strong+ a") %>% html_attr("href")),
  ID = i
)
links_RT_all %<>% add_row(.,
                          df)

}
links_RT_all %<>% filter(ID != 0)

RT_fulltexts %<>% bind_cols(.,
                           links_RT_all %>% select(-ID))

# save (and read in again):
openxlsx::write.xlsx(RT_fulltexts %>% select(-html_page), "articles/RT_fulltext_all.xlsx", asTable = T, overwrite = T)
rt_fulltexts <- rio::import("articles/RT_fulltext_all.xlsx")


# merge with other articles to identify (and scrape) missing fulltexts
# corpusRT_full %<>% mutate(id = as.integer(id))

# test <- full_join(corpusRT_full, RT_fulltexts, by = "id")

#merge with corpus_RT_all now:
corpusRT_all %<>% 
  mutate(id = as.integer(id)) %>% 
  full_join(., RT_fulltexts, by = "id", suffix = c("", "ft"))



# missing texts ####
# identify links to missing texts
missing_texts <- corpus_RT %>% 
  filter(is.na(text)) %>% 
  select(link, header, id, candidate, resort) %>% 
  dplyr::distinct()

# scrape missing articles:
library(rvest)
i = 1
i_repair = 1
eCap <- list(
       phantomjs.page.settings.userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/100.0.4896.75 Safari/537.36"
     )
library(RSelenium)
library(rvest)


while (i < length(missing_texts$link)) {

   system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
       rD <- rsDriver(browser = "chrome",
                      chromever = "99.0.4844.51",
                      verbose = F,
                      javascript = T,
                      nativeEvents = T,
                      extraCapabilities = eCap)
       remDr <- rD[["client"]]
       
 try(
   for (i in i_repair:length(missing_texts$link)) {
      
     # n = str_count(missing_texts$header[i] %>% 
     #                 str_remove_all(., "[:punct:]"), "\\w+")
      
     while(is.na(missing_texts$link[i]) |
           missing_texts$link[i] == "character(0)"){
       searchpage <- paste0("https://de.rt.com/search?q=",
                            str_remove_all(missing_texts$header[i], "[^\\sa-zA-ZÄÜÖäüöß0-9_-]") %>% 
                           #   str_extract(., paste0("^(?:\\S+ ){", n, "}(\\S+)")) %>% 
                              str_replace_all(., " ", "+"), 
                            "&df=&dt=")
       # shorten header with every unsuccessful search:
       # n = n-1
       # if (n < 0) {next} 
       
       remDr$navigate(searchpage)
       
       max_attempts <- 3
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
           
           tryCatch({webElem1 <- remDr$findElement(using = "css selector", ".Button_dark")},
                    error = function(e){NULL})
           
           try(webElem1$clickElement())
           
           Sys.sleep(.3)
           
           tryCatch({webElem2 <- remDr$findElement(using = "css selector", ".Button-root.Button-is1to1.Button-type_l.Button_dark")},
                    error = function(e){NULL})
           try(webElem2$clickElement())
           
           Sys.sleep(.3)
           
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
       
       
       page <- remDr$getPageSource()[[1]]
       
       # check links:
       new_links <- page %>% 
         read_html() %>% 
         html_nodes(".HeaderNews-type_5 .Link-isFullCard") %>% html_attr('href') 
       missing_texts$link[i] <- new_links %>% as.data.frame() %>% filter(., str_detect(., missing_texts$id[i] %>% as.character())) %>% as.character()
       }
    
     RTpage <- paste0("https://de.rt.com", missing_texts$link[i])
     while(remDr$getCurrentUrl() != RTpage){
       remDr$navigate(RTpage)
     }
     
     tmp <- remDr$getPageSource()[[1]]
     writeLines(tmp, paste0("articles/RThtml_missed/RTarticle", 931+i, ".txt"), useBytes = T)
     tmp %<>%  read_html(encoding = "UTF-8")
     
 #   tmp <- rvest::read_html(paste0("https://de.rt.com/", missing_texts$link[i])) 
      
    missing_texts$intextlinks[i] = list(tmp %>% html_elements(".ViewText-root a") %>% html_attr("href"))
    missing_texts$mehrlink[i] = list(tmp %>% html_elements("p strong+ a") %>% html_attr("href"))
    missing_texts$lead[i] = tmp %>% html_element(".Text-type_1") %>% html_text2()
    missing_texts$date[i] = tmp %>% html_element(".Timestamp-default") %>% html_text2()
   #  missing_texts$saved[i] = paste0("RTarticle", 931+i, ".txt")                                               forgot to un-comment -> %>% cbind(., source  = paste0("RTarticle", 931 + 1:length(missing_texts$link), ".txt"))
    missing_texts$text[i] = tmp %>% html_element(".ViewText-root") %>% html_text2()
    
    Sys.sleep(2)
  }
 )
  
 i_repair = i
  
}

# fix broken article kurzclips/video/127426-who-und-un-zur-impfpflicht-corona-ma%C3%9Fnahmen-duerfen-nicht-diskriminierend-sein/ by hand:
{
missing_texts$intextlinks[6695] = missing_texts$mehrlink[6695]
missing_texts$lead[6695] <- NA
missing_texts$text[6695] <- NA
missing_texts$date[6695] <- "20 Nov. 2021 00:00 Uhr"
# fix broken article /kurzclips/video/127493-ich-gehe-nicht-in-irak-zurück-migranten-gespräch-grenzgebiet-eu/ by hand:
missing_texts$intextlinks[6728] = missing_texts$mehrlink[6695]
missing_texts$lead[6728] <- NA
missing_texts$text[6728] <- NA
missing_texts$date[6728] <- "22 Nov. 2021 00:00 Uhr"
}

test <- left_join(corpus_RT, 
                  missing_texts, 
                  by = id, 
                  suffix = c("", "_missed"))

test <- rbind(corpus_RT %>% mutate(across(.fns = ~ as.character(.x))) %>% pivot_longer(., -id, values_drop_na = T),
              missing_texts %>% cbind(., saved  = paste0("RTarticle", 931 + 1:length(missing_texts$link), ".txt")) %>% mutate(across(.fns = ~ as.character(.x))) %>%  pivot_longer(., -id, values_drop_na = T)) %>% 
  drop_na() %>% 
  distinct() %>% 
  pivot_wider()

# ToDo: clean and join
corpus_RT <- rbind(corpus_RT %>% mutate(across(.fns = ~ as.character(.x)  %>% str_trim() %>% str_squish())) %>% pivot_longer(., -id, values_drop_na = T),
              missing_texts %>% cbind(., document  = paste0("RTarticle", 931 + 1:length(missing_texts$link), ".txt")) %>% mutate(across(.fns = ~ as.character(.x) %>% str_trim() %>% str_squish())) %>% rename_with(., .fn = ~ str_c(.x, "_missed"), .cols = -id) %>% 
                pivot_longer(., -id, values_drop_na = T)) %>% 
  distinct(id, name, .keep_all = T) %>% 
  pivot_wider() %>% 
  mutate(
    link = ifelse(is.na(link), link_missed, link),
    # link = ifelse(is.na(link), links_RT, link),
    header = ifelse(is.na(header), header_missed %>% 
                      str_remove_all("\\r"), header), 
    mehrlink = ifelse(is.na(mehrlink), mehrlink_missed, mehrlink),
    intextlinks = ifelse(is.na(intextlinks), intextlinks_missed, intextlinks),
    lead = ifelse(is.na(lead), lead_missed %>% str_remove_all(., "\\r"), lead),
    datetime = ifelse(is.na(date_article), date_missed %>% str_remove_all(., "\\r") %>% str_remove("Uhr") %>% as.POSIXct(format = " %d %b. %Y %H:%M ") %>% as.character(), date_article),
    date = ifelse(is.na(date), date_missed %>% as.Date(format = " %d %b. %Y"), date),
    text = ifelse(is.na(text), text_missed %>% str_remove_all(., "\\r") %>%  str_remove_all("\\n") %>% str_remove_all("noscript pattern"), text),
    fulltext = str_c(header, lead, text),
    resort = link %>% str_extract(., "([^/]+)"),
    document = ifelse(is.na(document), document_missed, document),
    source = "RTDE",
    wordcount = str_count(fulltext, "\\w+"),
    mentions_baerbock = str_count(fulltext, "Baerbock"),
    mentions_laschet = str_count(fulltext, "Laschet"),
    mentions_scholz = str_count(fulltext, "Scholz"),
    scholz = if_else(is.na(saved_scholz) | mentions_scholz == 0, 0, 1, 0),
    baerbock = if_else(is.na(saved_baerbock) | mentions_baerbock == 0, 0, 1, 0),
    laschet = if_else(is.na(saved_laschet) | mentions_laschet == 0, 0, 1, 0)
  ) %>% 
  select(-ends_with("missed"), -links_RT, -lead_article)

corpus_RT <- rbind(tester2 %>% mutate(across(.fns = ~ as.character(.x) %>% str_trim() %>% str_squish())), test) %>% 
  select(-ends_with("missed"), -links_RT, -lead_article)

corpus_RT %<>% 
  mutate(
  # across(.cols = c(intextlinks, mehrlink), 
  #                         .fns = ~ .x %>% 
  #                           str_sub(., 4, -3) %>% 
  #                           str_remove_all(., '\\"') %>% 
  #                           str_remove_all(., ' ') %>% 
  #                           str_trim() %>% 
  #                           str_squish() %>% 
  #                           str_split(., ",")))

  intextlinks = intextlinks %>% 
    str_remove_all(string = ., pattern = '\\)"') %>% 
    str_remove_all(string = ., pattern =  '\\"') %>% 
    str_remove_all(string = ., pattern = ' ') %>% 
    str_remove_all(string = ., pattern = 'c\\(') %>% 
    str_trim() %>% 
    str_squish() %>% 
    str_split(., ","),
  
  mehrlink = mehrlink %>% 
    str_remove_all(string = ., pattern = '\\)"') %>% 
    str_remove_all(string = ., pattern =  '\\"') %>% 
    str_remove_all(string = ., pattern = ' ') %>% 
    str_remove_all(string = ., pattern = 'c\\(') %>% 
    str_trim() %>% 
    str_squish() %>% 
    str_split(string = ., pattern = ",")
  ) %>% 
  # regain types:
  mutate(
    id = as.integer(id),
    date = as.Date(date),
    datetime = as.POSIXct(datetime),
    mentions_scholz = as.integer(mentions_scholz),
    mentions_baerbock = as.integer(mentions_baerbock),
    mentions_laschet = as.integer(mentions_laschet),
    scholz = as.integer(scholz),
    baerbock = as.integer(baerbock),
    laschet = as.integer(laschet),
    wordcount = as.integer(wordcount)
  ) 



# test <- corpus_RT %>% 
#   rowwise() %>%
#   mutate(
#     datetime = ifelse(is.na(datetime),
#                       list(read_html(paste0("articles/RThtml/", document), encoding = "UTF-8") %>%
#                         html_element(xpath = "//meta[@name='publish-date']") %>%
#                         html_attr("content") %>% html_text2()),
#                       as.character(datetime)
#   )
# )

datetime <- c()
# date <- datetime
corpus_RT %<>% arrange(document)
for (i in 1:length(corpus_RT$datetime)) {
  
  try(datetime <- append(datetime,
      rvest::read_html(paste0("articles/RThtml/", corpus_RT$document[i]), encoding = "UTF-8") %>% 
        html_element(xpath = "//meta[@name='publish-date']") %>% 
        html_attr("content") %>%
        as.POSIXct())
  )
  
}

corpus_RT %<>% 
  select(-datetime) %>% 
  bind_cols(., datetime = c(datetime, as.POSIXct("2021-11-20 12:00:00"), as.POSIXct("2021-11-22 12:00:00"))) %>% 
  mutate(date = as.Date(datetime))

# fix remaining data issues:

# corpus_RT %<>% filter(., 
#                  id != "451" &
#                  document != "RTarticle7626.txt" &
#                  document != "RTarticle7659.txt" &
#                  date != "18646" |
#                is.na(date) |
#                  is.na(document)) %>% 
#   mutate(
#     id = as.integer(id),
#     date = as.Date(date),
#     datetime = as.POSIXct(date_article),
#     mentions_scholz = as.integer(wordcount_scholz),
#     mentions_baerbock = as.integer(wordcount_baerbock),
#     mentions_laschet = as.integer(wordcount_laschet),
#     scholz = as.integer(scholz),
#     baerbock = as.integer(baerbock),
#     laschet = as.integer(laschet),
#     wordcount = as.integer(wordcount)
#   ) %>% select(-c(starts_with("wordcount_"), date_article, candidate))

corpus_RT_candidate <- 
  pivot_longer(corpus_RT, 
               cols = c("scholz", "baerbock", "laschet"), 
               names_to = "candidate") %>% 
  filter(value == 1)


# tidy corpus: ####
corpus_RT <- corpusRT_all %>%  # or corpus_RT?
  rename_with(., .cols = ends_with(".x"), .fn = ~ str_sub(.x, end = -3)) %>% 
  mutate(header = ifelse(is.na(header), 
                         heading, 
                         header) %>% 
           str_remove_all("\\r"),
         lead =  str_remove_all(lead, "\\r"),
         # exclude "Mehr zum Thema ..." from text?
         text =  str_remove_all(text, "\\r") %>%  str_remove_all("\\n") %>% str_remove_all("noscript pattern"),
         date_article =  str_remove_all(date_article, "\\r") %>% str_remove("Uhr") %>% as.POSIXct(format = " %d %b. %Y %H:%M "),
         lead_article =  str_remove_all(lead_article, "\\r"),
         date =  str_remove_all(date, "\\r") %>%  as.Date(format = " %d. %B %Y"),
         source = "RTDE",
         fulltext = str_c(header, lead, text)
         ) %>% 
  select(-ends_with(c(".y")), -heading)


# distinct or keep candidate articles for each candidate?
  # corpusRT_all %>% arrange() %>% distinct(id)


# save article htmls as txt ####
RT_fulltexts <- rio::import("articles/data_rt.rds", )
for(i in 1:length(RT_fulltexts$html_page)) {
  writeLines(RT_fulltexts$html_page[i], paste0("articles/RThtml_nocandidate/", i, ".txt"), useBytes = T)
}


# search summaries ####
# the displayed number of search results on RT diverges from the actual number of articles shown as search results.
# Therefore, search summaries were scraped as well:

summariesRT = tibble(N = 0, `saved` = "a", candidate = "a", date = as.Date("2000-01-01"))

for (ID in dir("articles/RT_searches_finest/")) {
  
  # ...and here:
  doc <- readr::read_file(paste0("articles/RT_searches_finest/", ID)) %>% 
    read_html(encoding = "UTF-8")
  
  df_new <- tibble(
    N = doc %>% html_element(".SearchForm-total") %>% html_text2() %>% str_extract(., "\\d+") %>% as.numeric(),
    saved = ID %>% as.character(),
    candidate = saved %>% str_extract(., "([^2]*)") %>% as.character(),
    date = ID %>% str_extract(., "[^a-zA-Z$.]+") %>% as.Date()
  ) 
  
  # adapt object name here:
  summariesRT %<>% add_row(., df_new)
  
} # end of doc

summariesRT %<>% 
  filter(candidate != "a") %>% 
  mutate(candidate = ifelse(candidate == "",
                            "all",
                            candidate)) %>% 
  select(-saved) %>% 
  pivot_wider(., names_from = "candidate", values_from = "N") 

summariesRT_long <- summariesRT %>% pivot_longer(!date, names_to = "candidate", values_to = "articles") %>% 
  mutate(date = as.Date(date)) %>% 
  # join relative numbers:
  left_join(., 
            summariesRT %>%
              mutate(across(.cols = c(Baerbock, Laschet, Scholz), .fns = ~ .x / all, .names = "{.col}_share"),
                     date = as.Date(date)) %>% 
              pivot_longer(cols = ends_with("share"), values_to = "share", names_to = "candidate", names_pattern = "([^_]+)") %>% 
              select(date, candidate, share))





# save search summaries:
openxlsx::write.xlsx(summariesRT, "articles/summaries_RT_searches.xlsx", asTable = T, overwrite = T)
summariesRT <- rio::import("articles/summaries_RT_searches.xlsx")

openxlsx::write.xlsx(summariesRT_long, "articles/summaries_RT_searches_long.xlsx", asTable = T, overwrite = T)
summariesRT_long <- rio::import("articles/summaries_RT_searches_long.xlsx")

# save final corpus: ####
# openxlsx::write.xlsx(corpusRT_full, "articles/RT_articles.xlsx", asTable = T, overwrite = T)
openxlsx::write.xlsx(corpus_RT, "articles/RT_corpus.xlsx", asTable = T, overwrite = T)
corpus_RT <- rio::import("articles/RT_corpus.xlsx")


# create corpus-object
library(quanteda)
RT_corpus <- corpus(corpus_RT, 
                    docid_field = "link", 
                    text_field = "fulltext", 
                    meta = list("resort", "date", "scholz", "baerbock", "laschet", "mentions_baerbock", "mentions_scholz", "mentions_laschet", "wordcount", "source"), 
                    unique_docnames = T)

RT_corpus_candidate <- corpus(corpus_RT_candidate, 
                    docid_field = "id", 
                    text_field = "fulltext", 
                    meta = list("resort", "date", "candidate", "mentions_baerbock", "mentions_scholz", "mentions_laschet", "wordcount", "source"), 
                    unique_docnames = F)


save(list = c("corpus_RT", 
              "RT_corpus",
              "summariesRT", 
              "summariesRT_long",
              "corpus_RT_candidate",
              "RT_corpus_candidate"), 
     file = "articles/RTcorpus.RData")
