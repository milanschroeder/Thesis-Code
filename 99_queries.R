# collect data:

# join base_sitemaps & pages
# ...

# join articles & (transposed) recommendations
# ...

# get list of tags for article
# ...

# get list of categories for article
# ...

# save selected html from DB #####

get_html <- function(linklist, dest = "ignore/html_docs"){
  # linklist might be a vector of multiple links that occur in RT_DB
  
  # connect to DB: #####
  library(pacman)
  pacman::p_load(tidyverse, DBI, RSQLite)
  RT_DB <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = "ignore/Russian_Media_Database_RT.sqlite") 
  
  # get entries for links
  links <- dplyr::tbl(RT_DB, "html_pages") %>% dplyr::filter(link %in% linklist)
  
  if (!dir.exists(dest)) {
    dir.create(dest)
  }
  
  for (i in 1:nrow(links)) {
# name file with hash or version-date etc?
  # check if str_length(links$html[i] < 1000000000 (i.e. default SQLITE_MAX_LENGTH))
    write(x = links$html_doc[i], file = paste0(dest, "/", links$doc_hash[i], ".html"))
  }

}