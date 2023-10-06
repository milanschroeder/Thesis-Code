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

get_html <- function(linklist, dest = "ignore/html_docs", con = conn){
  # linklist might be a vector of multiple links that occur in RT_DB
  
  library(tidyverse)
  
  # get entries for links
  links <- dplyr::tbl(con, "html_pages") %>% dplyr::filter(link %in% linklist) %>% collect
# checks through mega table, hence very slow...
  
  
  # create destination folder if not exists:
  if (!dir.exists(dest)) {
    dir.create(dest)
  }
  
  for (i in 1:nrow(links)) {
# name file with hash or version-date etc?
  # check if str_length(links$html[i] < 1000000000 (i.e. default SQLITE_MAX_LENGTH))
    write(x = links$html_doc[i], file = paste0(dest, "/", links$doc_hash[i], ".html"))
  }

}

get_html(linklist)
