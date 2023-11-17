# save previous pages to archive.org:

library(tidyverse)

# test for not available pages (that are archived!)

# connect DB #####
source("00_connect_DB_RT.R") # returns conn

# get links:
tosave <- 
  table(conn, page_data) %>% select(link, doc_hash, available_online)

# test:
tosave <- tibble(link = (c("https://www.example.org", "https://wikipedia.de")),
                 doc_hash = c(1, 2),
                 available_online = T)

for (i in 1:nrow(tosave)) {
  
  # save to archive.org:
  archive_links <- tibble(
    doc_hash = tosave$doc_hash[i],
    archive_url = system(paste("savepagenow", tosave$link[i]), intern = T) %>% ifelse(stringr::str_starts(., "https"), ., NA)
    
#    #"savepagenow"
    # "https://web.archive.org/save/"
    # , tosave$link[i]), intern = T) %>% ifelse(stringr::str_starts(., "https"), ., NA)

    
  )
  # push to DB 
  # DBI::dbWriteTable(conn = con, name = "archive_links", 
  #                   value = archive_links %>% dplyr::mutate(across(.cols = !is.character, as.character)),
  #                   append = TRUE
  # )  # if to slow: bundle saving
  
  print(paste(Sys.time() %>% toString(), tosave$link, tosave$available_online, archive_links$archive_url))
  
}
