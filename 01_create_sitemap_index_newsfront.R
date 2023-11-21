# 01 get sitemap_index

library(dplyr)
library(tibble)

# connect DB #####
source("00_connect_DB_newsfront.R")

# index sitemaps: #####

sitemap_index <- tibble(loc = paste0("https://", 
                                     c("", "en.", "bgr.", "de.", "es.", "srb.", "fr.", "hu.", "ge.", "sk."), 
                                     "news-front.su/sitemap_index.xml"),
                        version_nf = c("ru", "en", "bgr", "de", "es", "srb", "fr", "hu", "ge", "sk")
)

print("Created sitemap_index")

# push to DB if not exist: #####
if(!"index_sitemaps" %in% DBI::dbListTables(conn)){
  DBI::dbWriteTable(conn = conn, name = "index_sitemaps", value = sitemap_index)
  print("Succesfully written to DB")
}

