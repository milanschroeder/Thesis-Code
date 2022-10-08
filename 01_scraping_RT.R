# 01 Scraping RT
source("00_RT_Scraper.R")

### apply function ####

# set parameter:
# select startdate (here: 01 Jan 2021)
startdate <- "01-01-2021" 
# select time interval (here: 2 to keep small as possible)
period_end <- "01-01-2022"
# keyword & interval as default

# links_RT_search <- scrapeRT(startdate, period_end, interval_length = 2)
links_RT_search_finer <- scrapeRT(startdate, period_end, interval_length = 1)

# candidates
links_RT_search_Baerbock_new <- scrapeRT(startdate, period_end, keyword = "Baerbock", interval_length = 25, sleepmin = 5, sleepmax = 7)
# got 419/422 (soll 408 in 2021)
links_RT_search_Laschet_new <- scrapeRT(startdate, period_end, keyword = "Laschet", interval_length = 25, sleepmin = 5, sleepmax = 7)
# got 390/413 (soll 409 in 2021)
links_RT_search_Scholz_new <- scrapeRT(startdate, period_end, keyword = "Scholz", interval_length = 25, sleepmin = 5, sleepmax = 7)
# got 439/471 (soll 447 in 2021) 
links_RT_search_finest_Baerbock <- scrapeRT(startdate, period_end, keyword = "Baerbock", interval_length = 0, sleepmin = 2, folder = "RT_searches_candidates_finest")
links_RT_search_finest_Laschet <- scrapeRT(startdate, period_end, keyword = "Laschet", interval_length = 0, sleepmin = 2, folder = "RT_searches_candidates_finest")
links_RT_search_finest_Scholz <- scrapeRT(startdate, period_end, keyword = "Scholz", interval_length = 0, sleepmin = 2, folder = "RT_searches_candidates_finest")


# RT_searches_candidates_finest <- map_dfr(.x = candidate, cbind(scrapeRT(startdate, period_end, .x, 0, folder = "RT_searches_candidates_finest"), candidate = .x))

# check on 12-03/12-04 

### ToDo: FIND OUT WHICH ARE MISSING!!! ####

  # search all search result files for "Weiter-Button"
  # get all 68 missing articles

  # convert to df


base_url_rt <- "https://de.rt.com"

candidate_links <- c(links_RT_search_Baerbock_new, links_RT_search_Laschet_new, links_RT_search_Scholz_new) %>% tibble() %>% 
  distinct() %>% 
  mutate(links = paste0(base_url_rt, .))

write_xlsx(candidate_links)

# bind_rows(tibble(), tibble()) gives 3 vars with NAs for all rows of other vars... could be useful!

# look for remaining "Weiter" Buttons!
# need to filter for year!

save.image("all_data.RData")

# save list:
# library(rlist)
# rlist::list.save(links_RT_search, 'Articles/rt_links.rds')




# identical(links_RT_search, 'Articles/rt_links.rds')



## get page source ####

# data_RT <- map_dfr(links_RT$links_full, get_pages_RT)
# remDr$close()

# data_RT %<>% left_join(., links_RT, by = c("link" = "links_full"))

# # save dataset
 
# write_csv(data_RT, 'Articles/data_rt.csv')
# rio::export(data_RT, "Articles/data_rt.rds")
# 
# # import again:
# data_RT1 <- rio::import("articles/data_rt.rds")
# data_RT2 <- rio::import("articles/data_rt.csv") # seems preferable, but larger


# better: loop to save txt
{
# start server

for (i in 1:length(candidate_links$links)) {
  while (remDr$getCurrentUrl() != candidate_links$links[i]) {
    
    remDr$navigate(candidate_links$links[i])
    
    randsleep <- sample(seq(sleepmin, sleepmax, by = 0.001), 1)
    Sys.sleep(randsleep)
    
    page <- remDr$getPageSource()[[1]]
    
    writeLines(page, paste0("articles/RThtml/RTarticle", i, ".txt"), useBytes = T)
    
  }
}

  }
