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


# prepare variables
corpusRT_full = tibble(`header` = "a", `lead` = "a", `link` = "a", date = "a", `resort` = "a", `id` = "a", `saved` = "a", candidate = "a")

# headerRT2 = c()
# leadRT2 = c()
# dateRT2 = c()
# linkRT2 = c()
# resortRT2 = c()
# idRT2 =  c()
# saved2 = c()


for (ID in dir("articles/RT_searches")) {
  
  doc <- readr::read_file(paste0("articles/RT_searches/", ID)) %>% 
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
      
      corpusRT_full %<>% add_row(., df_new)
      
      # headerRT2 %<>% append(., articles[j])
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
    
    } # end of articles
  
  } # end of section

} # end of doc


# ToDO: define length of ID!

corpus_RT_laschet <- filter(corpusRT_full, candidate == "Laschet") %>% mutate(laschet = 1)
corpus_RT_scholz <- filter(corpusRT_full, candidate == "Scholz") %>% mutate(scholz = 1)
corpus_RT_baerbock <- filter(corpusRT_full, candidate == "Baerbock") %>% mutate(baerbock = 1)

corpusRT_full %<>% filter(header != "a") %>% 
  full_join(., corpus_RT_baerbock %>% select(baerbock, saved, link), by = "link", suffix = c("", "_baerbock")) %>% 
  full_join(., corpus_RT_laschet %>% select(laschet, saved, link), by = "link", suffix = c("", "_laschet")) %>% 
  full_join(., corpus_RT_scholz %>% select(scholz, saved, link), by = "link", suffix = c("", "_scholz")) %>% 
  distinct(., link, .keep_all = T)


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

save.image("all_data.RData")
# check & clean -> bind togerther -> dsitinct


# old version: ####

df_rt_articles <- tibble()

for (ID in dir("articles/RThtml")) {
  
  doc <- readr::read_file(paste0("articles/RThtml/", i)) %>% 
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


# further tidying: 
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


# get full texts for candidate related articles ####

RT_articles_candidates <- tibble(header = "a", lead = "a", date = "a", intextlinks = list(tibble(a = "a", b = "a")), mehrlink = "a", document = "a", text = "a")

for (article in dir("Articles/RThtml")) {
  doc <- readr::read_file(paste0("articles/RThtml/", article)) %>% 
    read_html(encoding = "UTF-8")
  
  df_new <- tibble(
    header = doc %>% html_element(".HeadLine-type_2") %>% html_text2(),
    lead = doc %>% html_element(".Text-type_1") %>% html_text2(),
    date = doc %>% html_element(".Timestamp-default") %>% html_text2(),
    intextlinks = list(doc %>% html_nodes(".ViewText-root a") %>% html_attr("href")),
    mehrlink = doc %>% html_nodes("strong+ a") %>% html_attr("href"),
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

test <- full_join(corpusRT_full, RT_articles_candidates, by = "header", suffix = c("", "_article"))

# join with corpusRT_full
# add source column = RTDE
# save:
openxlsx::write.xlsx(corpusRT_full, "articles/RT_articles.xlsx", asTable = T, overwrite = T)
