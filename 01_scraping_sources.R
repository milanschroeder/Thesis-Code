# 01 Scraping Sources

library(pacman)
p_load(tidyverse)
p_load(magrittr)

# I use the Internet Archive (IA) to scrape all articles released by RT Deutsch over the year 2021 ####

## get archive entries:

## get article URLs:

## get contents:



# For Comparison, I use the Factiva Database to scrape all articles released online by major national daily newspapers (TAZ, FR, SZ, Welt) ####

library(tm)
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
doc <- read_html("Articles/FAZ_B1.html", encoding = "UTF-8") 

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
