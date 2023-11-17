
library(tidyverse)
library(rvest)
library(tibble)
library(dplyr)
library(httr)
library(stringr)
library(xml2)
library(purrr)
library(RMariaDB)


#Datenbank
# connect

# server version:
source("00_connect_DB_newsfront.R")
mydb <- conn

#Link-Datensatz für 1 Jahr (24.02.2022 - 23.02.2023). Links wurden von Sitemaps davor gescraped. 

# all_links_newsfront = read.csv("news_front_df_all.csv") # adapt to get links from db

# Daten abrufen 

SQL_code = "SELECT * from en_newsfront;"    # adapt to get already scraped links from db
done_links_df = dbGetQuery(mydb, SQL_code)


#Artikel scrapen 

scrape_nf = function(link) {
  print(paste(Sys.time(), link))
  
  html = read_html(link)

    df = tibble (
    link = link, 
    headline = html %>% html_node(".entry-title") %>% html_text2(),
    timestamp = html %>% html_node (".article__date") %>% html_text2(),
    text = paste(html %>% html_nodes(".article__content span") %>% html_text2(),collapse = ", "),
    tags = paste(html %>% html_nodes(".article__tags .tag") %>% html_text2(), collapse = ", "),
    external_links = paste(html %>% html_nodes(".article__content a") %>% html_attr("href") , collapse = ", ")
  )
    
    #Sleep random between 1 and 3 seconds
    Sys.sleep(runif(1, min = 1, max = 3))
  
  df = df %>% 
   separate("timestamp", sep="\\s", c("date", "time")) %>%        # check for format of RT to make easily comparable
   #mutate(date = as.Date(date, format = "%d.%m.%Y"))  %>% 
   mutate(text = gsub("Due to censorship.*$", "", text)) %>% 
    
    # this could be unstable:
   mutate(text = str_replace_all(text, "(?<=\n\n\\s\n\n\\s\n\n\\s\n\n).+", "")) %>% #Überschrift vom folgenden Artikel weg
   
    mutate(text = str_squish(text)) %>% 
    
    # why this?
   mutate(text = str_replace_all(text, ".," , "")) %>%
   
    mutate(external_links= str_remove_all (external_links,"(\\,\\s)?https://en.news-front.info/.*")) %>% 
   mutate(external_links= str_remove_all (external_links,"\\,(?=\\s+)")) 
   
  #Datenbank 
   dbWriteTable(mydb, "en_newsfront", df, append=TRUE) 
  
  return(df) 
}



#For-Schleife, um alle Artikel zu scrapen

for (link in all_links_newsfront$news_front_df_all) {
 
# probably faster to do tgis as one filter at start?
   if (!(link %in% done_links_df$link)) {
    article <- scrape_nf(link)  # scrape the article
  } else {
    next  # skip this link if it has already been scraped
  }

}


#Datensatz herunterladen
write.csv(done_links_df, "en_newsfront_1year.csv", row.names=TRUE)
