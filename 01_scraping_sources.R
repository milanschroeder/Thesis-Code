# 01 Scraping Sources

library(pacman)
p_load(tidyverse)
p_load(magrittr)
p_load(roperators)
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
### WIN: by scraping IA search result list ####

p_load(RSelenium)

system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)

rD <- rsDriver(browser = "chrome",
               chromever = "99.0.4844.51")
remDr <- rD[["client"]]
base_url <- "https://web.archive.org/web/*/de.rt.com*"
remDr$navigate(base_url)

s = 0.5
total_pages <- 200
html_tables <- vector("list", total_pages)

src <- remDr$getPageSource()[[1]]
html_tables[1] <- src



for (i in 2:total_pages) {
    webElem <- remDr$findElement(using = "css selector", "#resultsUrl_next a")
    webElem$clickElement()
    Sys.sleep(s)
    html_tables[i] <- remDr$getPageSource()[[1]]
}
remDr$close()

## get contents: ####
library(rvest)
RT_links <- map(html_tables, ~ read_html(.) %>% html_table() %>% as.data.frame())
IA_links <- map(html_tables, ~ read_html(., encoding = "UTF-8") %>% html_elements("#resultsUrl a")) %>% purrr::flatten()

### ENCODING ERROR !!!!! (concerning few relevant articles but still...)

IA_URLS <- array()
for (i in 1:length(IA_links)) {
  IA_URLS[i] <- xml_attrs(IA_links[[i]])
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

read_html(RT_links$IA_URL[1]) %>% html_elements(".captures-range-info a:nth-child(1)")


system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
rD <- rsDriver(browser = "chrome",
               chromever = "99.0.4844.51")
remDr <- rD[["client"]]

s = 5
total_pages <- nrow(RT_links)
html_pages <- vector("list", total_pages)

for (i in 15:total_pages) {
  remDr$navigate(RT_links$IA_URL[i])
  Sys.sleep(s)
  try(webElem2 <- remDr$findElement(using = "css selector", ".captures-range-info a:nth-child(1)"), 
      silent = T, outFile = cat("page", i, "failed to navigate"))
  webElem2$clickElement()
  html_pages[i] <- remDr$getPageSource()[[1]]
}
remDr$close()

# convert to data:
extract_variables_rt <- function(doc = doc){

# ideally (but hopefully unnecessary): include try/catch
  df <- data.frame() %>% bind_cols(
              doc %>%  html_elements(".ArticleView-timestamp .Timestamp-default") %>% html_text2() %>% 
                tibble() %>% rename(., "date" = "."), 
              doc %>% html_elements(".HeadLine-type_2") %>% html_text2() %>% 
                tibble() %>% rename(., "heading" = "."),
              doc %>% html_elements(".Text-type_1") %>% html_text2() %>% 
                tibble() %>% rename(., "lead" = "."),
              doc %>% html_elements(".Breadcrumbs-arrow+ .Breadcrumbs-item span") %>% html_text2() %>% 
                tibble() %>% rename(., "resort" = "."),
              doc %>% html_elements(".ViewText-root")%>% html_text2() %>% 
                tibble() %>% rename(., "text" = "."),
              id = NA,
              wordcount = NA
    ) %>% 
    select(id, heading, source, resort, wordcount, page, date, text, lead)
}

df_rt <- map_dfr(html_pages, extract_variables_rt)

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
for (i in length(candidate)){
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



### flatten list objects to df (works well, so just leave it the base-R way for now) ####

df <- tibble()
for (i in 1:length(corpus)) {
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

##### e.g. using purrr works gives nice output, but only for 3 rows...
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
