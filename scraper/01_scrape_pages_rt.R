# 05_scrape_pages.R: scraping functions 

# select scraper ####

scrape_pages <- function(pages_list = updated_pages, sleeptime = .5, connection = conn, log_file = "scrapelog_rt.txt"){

  # connect DB:
  require(DBI)
  require(rvest)
  require(dplyr)
  require(stringr)
#  RT_DB <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = "ignore/Russian_Media_Database_RT.sqlite") 
  
  #  loop over pages:
  for (i in 1:nrow(pages_list)) {
    
  # select version specific scraper:
    if(pages_list$version[i] %in%  c("bk", "bk-lat")) {

      scraper_bk(pages_list$loc[i], pages_list$version[i],  con = connection)
      
    } else if(pages_list$version[i] == "de") {
      
      scraper_de(pages_list$loc[i], pages_list$version[i], con = connection)
      
    } else if(pages_list$version[i] == "es") {
      
      scraper_es(pages_list$loc[i], pages_list$version[i], con = connection)
      
    } else if(pages_list$version[i] == "en") {
      
      modified_time <- pages_list$lastmod_utc[i]
      modified_tz <- pages_list$lastmod_tz[i]
      scraper_en(pages_list$loc[i], pages_list$version[i], con = connection, lastmod = modified_time, lastmod_tz = modified_tz)
      
    } else if(pages_list$version[i] == "fr") {
      
      modified_time <- pages_list$lastmod_utc[i]
      modified_tz <- pages_list$lastmod_tz[i]
      scraper_fr(pages_list$loc[i], pages_list$version[i], con = connection, lastmod = modified_time, lastmod_tz = modified_tz)
      
    } else if(pages_list$version[i] == "ru") {
      
      modified_time <- pages_list$lastmod_utc[i]
      modified_tz <- pages_list$lastmod_tz[i]
      scraper_ru(pages_list$loc[i], pages_list$version[i], con = connection, lastmod = modified_time, lastmod_tz = modified_tz)
      
    } else { # if(pages_list$version[i] == "ar"){
      
      modified_time <- pages_list$lastmod_utc[i]
      modified_tz <- pages_list$lastmod_tz[i]
      scraper_ar(pages_list$loc[i], pages_list$version[i], con = connection, lastmod = modified_time, lastmod_tz = modified_tz)
      
    }
    
    # sleep:
    Sys.sleep(sleeptime)
    cat(i, " / ", length(pages_list$loc), ":", Sys.time() %>% toString(), pages_list$loc[i], "saved.\n", sep = " ")
    
  } # end of loop
  
  # dbDisconnect(RT_DB)
} # end of scraping function

# version specific scrapers (to be called inside function) #####


# RT Balkan:
scraper_bk <- function(link, version, con, logfile = log_file){

# hard code file to avoid weird error:
logfile = "scrapelog_rt.txt"

  
  # connect DB:
  require(DBI)
  require(rvest)
  require(dplyr)
  require(stringr)
  
  # get page:
  base_url <- ifelse(version == "bk", "https://rt.rs", "https://lat.rt.rs")
  
if("try-error" %in% class(
  doc <- try(rvest::read_html(link)    
))) {
  
  catch_not_captured(link, version, con)
  cat(Sys.time() %>% toString(), "failed to save", link, "(main)", "\n", sep = " ")
  # write in logfile:
  write(x = paste(Sys.time(), link, "main",  sep = ", "), 
        file = logfile, append = T, sep = "\n")
  
} else{
  
  capture_time <- Sys.time()
  html_doc <- toString(doc)
  doc_hash <- rlang::hash(html_doc)
  
  
  # get page_data:
  
# save full html:
  # html_pages <- tibble(
  #   link = link,
  #   doc_hash = doc_hash,
  #   html_doc = html_doc
  # )
  # # push to DB 
  # DBI::dbWriteTable(conn = con, name = "html_pages", 
  #                   value = html_pages %>% dplyr::mutate(across(.cols = !is.character, as.character)),
  #                   append = TRUE
  # ) 
  
# main df:
  page_data <- tibble(
    header = doc %>% html_elements(xpath = "//meta[@property='og:title']") %>% rvest::html_attr("content"), 
    # or use header = doc %>% html_elements(., ".HeadLine-type_2") %>% html_text2() %>% stringr::str_c(., collapse = " ")
    link = link,
    version = version,

# ToDo: check if reliable:    
    author = ifelse(class(doc %>% html_elements(., ".author-simple-card__title") %>% html_text2()) == "character",
                    stringr::str_c(doc %>% html_elements(., ".author-simple-card__title") %>% html_text2()),
                    NA) %>%  as.character(),    
    author_img = ifelse(class(doc %>% html_nodes(., ".author-simple-card__img") %>% html_attr("src")) == "character",
                         stringr::str_c(doc %>% html_nodes(., ".author-simple-card__img") %>% html_attr("src")),
                         NA) %>%  as.character(),
  
  
    date_time_utc = doc %>% html_element(".Timestamp-default") %>% html_attr("datetime") %>% lubridate::ymd_hms(., tz = "UTC") %>% as.character(),
    date = as.Date(date_time_utc %>% lubridate::ymd_hms(tz = "UTC")) %>% as.character(),
    tz_original = doc %>% html_element(".Timestamp-default") %>% html_attr("datetime") %>% str_sub(start = -6),
    
    category = doc %>% html_element(".Breadcrumbs-arrow+ .Breadcrumbs-item span") %>% html_text2(),
    
    lead = doc %>% html_elements(xpath = "//meta[@name='description']") %>% html_attr("content"), 
    # or use lead = doc %>% html_element(".p-24 .Text-type_1") %>% html_text2()
    text_article = stringr::str_c(doc %>% html_elements(".ViewText-root p") %>% html_text2(), collapse = " "), # pure article text
    text_all = stringr::str_c(doc %>% html_elements(".ViewText-root") %>% html_text2() %>%  # all text including intextlinks & embeddings
                                stringr::str_remove_all(., "noscript pattern"),
                              collapse = " "),

    cover_img_url = doc %>% html_element(".Cover-image img") %>% html_attr("data-src"),
    cover_img_alt = doc %>% html_element(".Cover-image img") %>% html_attr("alt"),
    cover_img_caption = doc %>% html_element(".Cover-footer") %>% html_text2(),
    cover_img_rights = doc %>% html_element(".Cover-imageSource") %>% html_text2(),

    available_online = 1,
    capture_time = capture_time,
    doc_hash = doc_hash

  ) # end of main df
  # push to DB  
  DBI::dbWriteTable(conn = con, name = "page_data", 
                    value = page_data %>% dplyr::mutate(across(.cols = !is.character, as.character),
                                                        available_online = as.integer(1)),
                    append = TRUE
  ) 

  
  # category_list df: 
  if("try-error" %in% class(
    
  category_list <- try(tibble(
    doc_hash = doc_hash,
    category_text = doc %>% html_elements(., ".RTLink-root.RTLink-breadcrumb") %>% html_text2(),
    category_url = paste0(base_url, doc %>% html_nodes(., ".RTLink-root.RTLink-breadcrumb") %>% html_attr("href")),
    version = version
    )
  ))) {
    cat(Sys.time() %>% toString(), "failed to save", link, "(category_list)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "category_list",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
  } else{
    
  # alternatively: create dictionary to safe space
  # push to DB
  DBI::dbWriteTable(conn = con, name = "category_list", 
                    value = category_list %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                    append = TRUE
  ) 
  } # end of category-error handling
  
  # tag_list df: 
  if("try-error" %in% class(
    tag_list <- try(tibble(
      doc_hash = doc_hash,
      tag_name = doc %>% html_elements(".Tags-link") %>% html_text2(),
      tag_url = paste0(base_url, doc %>% html_nodes(".Tags-link") %>% html_attr("href")),
      version = version
      )
  ))) {
    cat(Sys.time() %>% toString(), "failed to save", link, "(tag_lists)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "tag_lists",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
  } else{
 
    # alternatively create dictionary to safe space
  # push to DB
  DBI::dbWriteTable(conn = con, name = "tag_list", 
                    value = tag_list %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                    append = TRUE
  ) 
  } # end of tag-error handling
  
  
  if("try-error" %in% class(
    intext_links <- try(tibble(
      doc_hash = doc_hash,
      intext_link_url = ifelse(doc %>% html_nodes(".ViewText-root a") %>% html_attr("href") %>% str_starts("/"),
                            paste0(base_url, doc %>% html_nodes(".ViewText-root a") %>% html_attr("href")),
                            doc %>% html_nodes(".ViewText-root a") %>% html_attr("href")
                            ),
      intext_link_text = doc %>% html_nodes(".ViewText-root a") %>% html_text2()
      )
    ))) {
    cat(Sys.time() %>% toString(), "failed to save", link, "(intext_links)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "intext_links",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
  } else{
  
  # push to DB
  DBI::dbWriteTable(conn = con, name = "intext_links", 
                    value = intext_links %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                    append = TRUE
  ) 
  }
  
  
  if("try-error" %in% class(
    recommendations_main <- try(tibble(
      doc_hash = doc_hash,
      # RT Balkan specific:
      main_recommendations_title = doc %>% html_nodes(., ".ArticleView-crosslink a") %>% html_text2(),
      main_recommendations_link = doc %>% html_nodes(., ".ArticleView-crosslink a") %>% html_attr("href"),
      main_recommendations_img = doc %>% html_node(., ".ArticleView-crosslink img") %>% html_attr("data-src"),
      main_recommendations_alt = doc %>% html_node(., ".ArticleView-crosslink img") %>% html_attr("alt") # always == title?
    )
  ))) {
    cat(Sys.time() %>% toString(), "failed to save", link, "(recommendations_main)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "recommendations_main",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
  } else{
  # push to DB
  DBI::dbWriteTable(conn = con, name = "recommendations_main", 
                    value = recommendations_main %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                    append = TRUE
  ) 
  }
  
  
  
  if("try-error" %in% class(
     recommendations_embedded <- try(tibble(
      doc_hash = doc_hash,
      recommendation_embedded_thumbnail = doc %>% html_elements(".ReadMore-root .Picture-root img") %>% html_attr("data-src"),
      recommendation_embedded_title = doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_text2(),
      recommendation_embedded_link = ifelse(stringr::str_length(doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_attr("href")) > 0,
                                                            stringr::str_c(base_url, doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_attr("href")),
                                                            NA)
    )
  ))) {
    cat(Sys.time() %>% toString(), "failed to save", link, "(recommendations_embedded)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "recommendations_embedded",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
  } else{
  # push to DB
  DBI::dbWriteTable(conn = con, name = "recommendations_embedded", 
                    value = recommendations_embedded %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                    append = TRUE
  ) 
  }
   
  
  
  if("try-error" %in% class(
    images <- try(tibble(
      doc_hash = doc_hash,
      image_url = doc %>% html_elements(".RTImage-image.RTImage-original picture img") %>% html_attr("data-src"),
      image_caption = doc %>% html_elements(".RTImage-caption") %>% html_text2(),
      image_source = doc %>% html_elements(".RTImage-source") %>% html_text2()
    )
    ))) {
    cat(Sys.time() %>% toString(), "failed to save", link, "(images)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "images",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
  } else{
  # push to DB
  DBI::dbWriteTable(conn = con, name = "images", 
                    value = images %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                    append = TRUE
  ) 
  }
  
  
  # Embeddings:  
  not_include <- c()
  
  if("try-error" %in% class(
       youtube_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = if (length(doc %>% html_elements(".YouTubeEmbed") %>% html_attr("id")) > 0) {
            paste0("https://www.youtube.com/embed/", doc %>% html_elements(".YouTubeEmbed") %>% html_attr("id"))   
          } else{doc %>% html_elements(".YouTubeEmbed") %>% html_attr("id")},
          source = "youtube"
        )
    ))) {
    cat(Sys.time() %>% toString(), "failed to save", link, "(youtube_embeddings)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "youtube_embeddings",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
    not_include <- append(not_include, "youtube_embeddings")
  }
      
  
  if("try-error" %in% class(
     twitter_embeddings <- try(tibble(
      doc_hash = doc_hash,
      embedding_url = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']/*/a") %>% html_attr("href"),
      source = "twitter"
        # embeddings_twitter_date = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']/*/a") %>% html_text2() %>% lubridate::parse_date_time(., "%B %d, %Y") %>% as.character()
        # embeddings_twitter_text = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']") %>% html_text2()
    )
    ))) {
    cat(Sys.time() %>% toString(), "failed to save", link, "(twitter_embeddings)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "twitter_embeddings",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
    not_include <- append(not_include, "twitter_embeddings")
  }
    
           
  
  if("try-error" %in% class(
     odysee_embeddings <- try(tibble(
      doc_hash = doc_hash,
      embedding_url = doc %>% html_elements(".EmbedBlock-odysee iframe") %>% html_attr("data-src"),
      source = "odysee"
    )
    ))) {
    cat(Sys.time() %>% toString(), "failed to save", link, "(odysee_embeddings)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "odysee_embeddings",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
    not_include <- append(not_include, "odysee_embeddings")
  }
    
  
  if("try-error" %in% class(
    telegram_embeddings <- try(tibble(
      doc_hash = doc_hash,
      embedding_url = if (length(doc %>% html_elements(".TelegramEmbed script") %>% html_attr("data-telegram-post")) > 0) {
        paste0("https://t.me/", doc %>% html_elements(".TelegramEmbed script") %>% html_attr("data-telegram-post"))   
      } else{doc %>% html_elements(".TelegramEmbed script") %>% html_attr("data-telegram-post")},
      source = "telegram"
    )
    ))) {
    cat(Sys.time() %>% toString(), "failed to save", link, "(telegram_embeddings)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "telegram_embeddings",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
    not_include <- append(not_include, "telegram_embeddings")
  } 
  
  
# other (Podcast etc.):
  if("try-error" %in% class(
     vk_embeddings <- try(tibble(
      doc_hash = doc_hash,
      embedding_url = doc %>% html_elements("iframe") %>% html_attr("data-src"),
      source = ifelse(doc %>% html_elements("iframe") %>% as.character() %>% str_detect("vk.com"),
                      "vk",
                      "other")
    )
  ))) {
    cat(Sys.time() %>% toString(), "failed to save", link, "(vk_embeddings)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "vk_embeddings",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
    not_include <- append(not_include, "vk_embeddings")
  } 
    
      

  if("try-error" %in% class(
     podbean_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".AllEmbed iframe") %>% html_attr("src"),
        source = ifelse(doc %>% html_elements(".AllEmbed iframe") %>% as.character() %>% str_detect("podbean"),
                        "podbean",
                        "other")
        )
  ))) {
    cat(Sys.time() %>% toString(), "failed to save", link, "(podbean_embeddings)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "podbean_embeddings",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
    not_include <- append(not_include, "podbean_embeddings")
  }     

  
  # check which embeddings are no error:
  all_embeddings <- c("youtube_embeddings", "twitter_embeddings", "odysee_embeddings", "telegram_embeddings", "vk_embeddings", "podbean_embeddings")  
  valid_embeddings <- all_embeddings[!all_embeddings %in% not_include]
  paste0("dplyr::bind_rows(", 
         str_flatten(valid_embeddings, collapse = ", "),
         ")")
  
  if("try-error" %in% class(
    embeddings <- try(eval(parse(text = valid_embeddings))) # %>% unique()
  )) {
    cat(Sys.time() %>% toString(), "failed to save", link, "(embeddings)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "embeddings",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
  } else{
    # push to DB
    DBI::dbWriteTable(conn = con, name = "embeddings", 
                      value = embeddings %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                      append = TRUE
    ) 
    }
  } # end of error-handling
} # end of scraper_bk


# RT Deutsch:
scraper_de <- function(link, version, con, logfile = log_file){

# hard code to avoid weird error:
logfile = "scrapelog_rt.txt"
  
  require(DBI)
  require(rvest)
  require(dplyr)
  require(stringr)
  
  # get page:
  base_url <- "https://de.rt.com"
 
  # try and record as missing if not available
if("try-error" %in% class(
    doc <- try(rvest::read_html(link)
))) {
  
  catch_not_captured(link, version, con)
  cat(Sys.time() %>% toString(), "failed to save", link, "(main)", "\n", sep = " ")
  # write in logfile:
  write(x = paste(Sys.time(), link, "main",  sep = ", "), 
        file = logfile, append = T, sep = "\n")

  } else{ # if page available
    
    capture_time <- Sys.time()
    html_doc <- toString(doc)
    doc_hash <- rlang::hash(html_doc)
    
 
# get page_data:
  
    # save full html:
    # html_pages <- tibble(
    #   link = link,
    #   doc_hash = doc_hash,
    #   html_doc = html_doc
    # )
    # # push to DB 
    # DBI::dbWriteTable(conn = con, name = "html_pages", 
    #                   value = html_pages %>% dplyr::mutate(across(.cols = !is.character, as.character)),
    #                   append = TRUE
    # ) 
  
    
  # main df:
  page_data <- tibble(
    header = doc %>% html_elements(xpath = "//meta[@property='og:title']") %>% html_attr("content"), 
    # or use header = doc %>% html_elements(., ".HeadLine-type_2") %>% html_text2() %>% stringr::str_c(., collapse = " ")
    link = link,
    version = version,
  
    author = doc %>% html_element("p:nth-child(1) em") %>% html_text2() %>% stringr::str_remove(., pattern = "[v]on|[V]on "), # regex: "(?<=[v|V]on\s).*"
    author_img = NA,

    # date_time_utc = doc %>% html_elements(xpath = "//meta[@name='publish-date']") %>% html_attr("content") # same for tz_original
    date_time_utc = doc %>% html_element(".Timestamp-default") %>% html_attr("datetime") %>% lubridate::ymd_hms(., tz = "UTC") %>% as.character(),
    date = as.Date(date_time_utc %>% lubridate::ymd_hms(tz = "UTC")) %>% as.character(),
    tz_original = doc %>% html_element(".Timestamp-default") %>% html_attr("datetime") %>% str_sub(start = -6),
    
    category = doc %>% html_element(".Breadcrumbs-arrow+ .Breadcrumbs-item span") %>% html_text2(),
    
    lead = doc %>% html_elements(xpath = "//meta[@name='description']") %>% html_attr("content"), 
    # or use lead = doc %>% html_element(".p-24 .Text-type_1") %>% html_text2()
    text_article = stringr::str_c(doc %>% html_elements(".ViewText-root p") %>% html_text2(), collapse = " "), # pure article text
    text_all = stringr::str_c(doc %>% html_elements(".ViewText-root") %>% html_text2() %>%  # all text including intextlinks & embeddings
                                stringr::str_remove_all(., "noscript pattern"),
                              collapse = " "),
    
    cover_img_url = doc %>% html_element(".Cover-image img") %>% html_attr("data-src"),
    cover_img_alt = doc %>% html_element(".Cover-image img") %>% html_attr("alt"),
    cover_img_caption = doc %>% html_element(".Cover-footer") %>% html_text2(),
    cover_img_rights = doc %>% html_element(".Cover-imageSource") %>% html_text2(),
    
    available_online = 1,
    capture_time = capture_time,
    doc_hash = doc_hash
    
  ) # end of main df
  # push to DB  
  DBI::dbWriteTable(conn = con, name = "page_data", 
                    value = page_data %>% dplyr::mutate(across(.cols = !is.character, as.character),
                                                        available_online = as.integer(1)),
                    append = TRUE
  )
  
  
  # category_list df: 
  if("try-error" %in% class(
    category_list <- try(tibble(
      doc_hash = doc_hash,
      category_text = doc %>% html_elements(., ".RTLink-root.RTLink-breadcrumb") %>% html_text2(),
      category_url = paste0(base_url, doc %>% html_nodes(., ".RTLink-root.RTLink-breadcrumb") %>% html_attr("href")),
      version = version
    )
    ))) {
    cat(Sys.time() %>% toString(), "failed to save", link, "(category_list)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "category_list",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
  } else{
  # alternatively: create dictionary to safe space
  # push to DB
  DBI::dbWriteTable(conn = con, name = "category_list", 
                    value = category_list %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                    append = TRUE
  ) 
  } # end of category-error handling
  
  
  # tag_list df: 
  if("try-error" %in% class(
    tag_list <- try(tibble(
      doc_hash = doc_hash,
      tag_name = doc %>% html_elements(".Tags-link") %>% html_text2(),
      tag_url = paste0(base_url, doc %>% html_nodes(".Tags-link") %>% html_attr("href")),
      version = version
    )
    ))) {
    cat(Sys.time() %>% toString(), "failed to save", link, "(tag_lists)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "tag_lists",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
  } else{
    
    # alternatively create dictionary to safe space
    # push to DB
    DBI::dbWriteTable(conn = con, name = "tag_list", 
                      value = tag_list %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                      append = TRUE
    ) 
  } # end of tag-error handling
  
  
  if("try-error" %in% class(
    intext_links <- try(tibble(
      doc_hash = doc_hash,
      intext_link_url = ifelse(doc %>% html_nodes(".ViewText-root a") %>% html_attr("href") %>% str_starts("/"),
                               paste0(base_url, doc %>% html_nodes(".ViewText-root a") %>% html_attr("href")),
                               doc %>% html_nodes(".ViewText-root a") %>% html_attr("href")
      ),
      intext_link_text = doc %>% html_nodes(".ViewText-root a") %>% html_text2()
    )
    ))) {
    cat(Sys.time() %>% toString(), "failed to save", link, "(intext_links)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "intext_links",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
  } else{
    
    # push to DB
    DBI::dbWriteTable(conn = con, name = "intext_links", 
                      value = intext_links %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                      append = TRUE
    ) 
  }
  
  
  if("try-error" %in% class(
    recommendations_main <- try(tibble(
      doc_hash = doc_hash,
      main_recommendations_title = doc %>% html_elements("strong+ a") %>% html_text2(),
      main_recommendations_link = doc %>% html_elements("strong+ a") %>% html_attr("href"),
      main_recommendations_img = NA,
      main_recommendations_alt = NA
    )
    ))) {
    cat(Sys.time() %>% toString(), "failed to save", link, "(recommendations_main)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "recommendations_main",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
  } else{
    # push to DB
    DBI::dbWriteTable(conn = con, name = "recommendations_main", 
                      value = recommendations_main %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                      append = TRUE
    ) 
  }
  
  
  
  if("try-error" %in% class(
    recommendations_embedded <- try(tibble(
      doc_hash = doc_hash,
      recommendation_embedded_thumbnail = doc %>% html_elements(".Card-imageWrap noscript img") %>%  html_attr("src"),
      recommendation_embedded_title = doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_text2(),
      recommendation_embedded_link = ifelse(stringr::str_length(doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_attr("href")) > 0,
                                            stringr::str_c(base_url, doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_attr("href")),
                                            NA)
    )
    ))) {
    cat(Sys.time() %>% toString(), "failed to save", link, "(recommendations_embedded)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "recommendations_embedded",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
  } else{
    # push to DB
    DBI::dbWriteTable(conn = con, name = "recommendations_embedded", 
                      value = recommendations_embedded %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                      append = TRUE
    ) 
  }
  
  
# still works or is this en version only now?
  if("try-error" %in% class(
    images <- try(tibble(
      doc_hash = doc_hash,
      image_url = doc %>% html_elements(".RTImage-image.RTImage-original picture img") %>% html_attr("data-src"),
      image_caption = doc %>% html_elements(".RTImage-caption") %>% html_text2(),
      image_source = doc %>% html_elements(".RTImage-source") %>% html_text2() 
  # %>% stringr::str_remove(., pattern = "(?<=\u00A9).*")
    )
    ))) {
    cat(Sys.time() %>% toString(), "failed to save", link, "(images)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "images",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
  } else{
    # push to DB
    DBI::dbWriteTable(conn = con, name = "images", 
                      value = images %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                      append = TRUE
    ) 
  }
  
  
  # Embeddings:  
  not_include <- c()
  
  if("try-error" %in% class(
    youtube_embeddings <- try(tibble(
      doc_hash = doc_hash,
      embedding_url = if (length(doc %>% html_elements(".YouTubeEmbed") %>% html_attr("id")) > 0) {
        paste0("https://www.youtube.com/embed/", doc %>% html_elements(".YouTubeEmbed") %>% html_attr("id"))   
      } else{doc %>% html_elements(".YouTubeEmbed") %>% html_attr("id")},
      source = "youtube"
    )
    ))) {
    cat(Sys.time() %>% toString(), "failed to save", link, "(youtube_embeddings)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "youtube_embeddings",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
    not_include <- append(not_include, "youtube_embeddings")
  }
  
  
  if("try-error" %in% class(
    twitter_embeddings <- try(tibble(
      doc_hash = doc_hash,
      embedding_url = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']/*/a") %>% html_attr("href"),
      source = "twitter"
      # embeddings_twitter_date = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']/*/a") %>% html_text2() %>% lubridate::parse_date_time(., "%B %d, %Y") %>% as.character()
      # embeddings_twitter_text = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']") %>% html_text2()
    )
    ))) {
    cat(Sys.time() %>% toString(), "failed to save", link, "(twitter_embeddings)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "twitter_embeddings",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
    not_include <- append(not_include, "twitter_embeddings")
  }
  
  
  
  if("try-error" %in% class(
    odysee_embeddings <- try(tibble(
      doc_hash = doc_hash,
      embedding_url = doc %>% html_elements(".EmbedBlock-odysee iframe") %>% html_attr("data-src"),
      source = "odysee"
    )
    ))) {
    cat(Sys.time() %>% toString(), "failed to save", link, "(odysee_embeddings)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "odysee_embeddings",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
    not_include <- append(not_include, "odysee_embeddings")
  }
  
  
  if("try-error" %in% class(
    telegram_embeddings <- try(tibble(
      doc_hash = doc_hash,
      embedding_url = if (length(doc %>% html_elements(".TelegramEmbed script") %>% html_attr("data-telegram-post")) > 0) {
        paste0("https://t.me/", doc %>% html_elements(".TelegramEmbed script") %>% html_attr("data-telegram-post"))   
      } else{doc %>% html_elements(".TelegramEmbed script") %>% html_attr("data-telegram-post")},
      source = "telegram"
    )
    ))) {
    cat(Sys.time() %>% toString(), "failed to save", link, "(telegram_embeddings)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "telegram_embeddings",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
    not_include <- append(not_include, "telegram_embeddings")
  } 
  
  
  # other (Podcast etc.):
  if("try-error" %in% class(
    vk_embeddings <- try(tibble(
      doc_hash = doc_hash,
      embedding_url = doc %>% html_elements("iframe") %>% html_attr("data-src"),
      source = ifelse(doc %>% html_elements("iframe") %>% as.character() %>% str_detect("vk.com"),
                      "vk",
                      "other")
    )
    ))) {
    cat(Sys.time() %>% toString(), "failed to save", link, "(telegram_embeddings)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "telegram_embeddings",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
    not_include <- append(not_include, "telegram_embeddings")
  } 
  
  
  # other (Podcast etc.):
  if("try-error" %in% class(
    podbean_embeddings <- try(tibble(
      doc_hash = doc_hash,
      embedding_url = doc %>% html_elements(".AllEmbed iframe") %>% html_attr("src"),
      source = ifelse(doc %>% html_elements(".AllEmbed iframe") %>% as.character() %>% str_detect("podbean"),
                      "podbean",
                      "other")
    )
    ))) {
    cat(Sys.time() %>% toString(), "failed to save", link, "(podbean_embeddings)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "podbean_embeddings",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
    not_include <- append(not_include, "podbean_embeddings")
  }     
  
  
  # check which embeddings are no error:
  all_embeddings <- c("youtube_embeddings", "twitter_embeddings", "odysee_embeddings", "telegram_embeddings", "vk_embeddings", "podbean_embeddings")  
  valid_embeddings <- all_embeddings[!all_embeddings %in% not_include]
  paste0("dplyr::bind_rows(", 
         str_flatten(valid_embeddings, collapse = ", "),
         ")")
  
  if("try-error" %in% class(
    embeddings <- try(eval(parse(text = valid_embeddings))) # %>% unique()
  )) {
    cat(Sys.time() %>% toString(), "failed to save", link, "(embeddings)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "embeddings",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
  } else{
    # push to DB
    DBI::dbWriteTable(conn = con, name = "embeddings", 
                      value = embeddings %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                      append = TRUE
      ) 
    }
  } # end of error-handling
} # end of scraper_de


# RT espanol:
scraper_es <- function(link, version, con, logfile = log_file){
  
  require(DBI)
  require(rvest)
  require(dplyr)
  require(stringr)

# hard code to avoid weird error:
logfile = "scrapelog_rt.txt"

  
  # get page:
  base_url <- "https://actualidad.rt.com/"
  
  # try and record as missing if not available
  if("try-error" %in% class(
    doc <- try(rvest::read_html(link)
    ))) {
    
    catch_not_captured(link, version, con)
    cat(Sys.time() %>% toString(), "failed to save", link, "(main)", "\n", sep = " ")
    # write in logfile:
    write(x = paste(Sys.time(), link, "main",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
    
  } else{ # if page available
    
    capture_time <- Sys.time()
    html_doc <- toString(doc)
    doc_hash <- rlang::hash(html_doc)
    
    
    # get page_data:
    
    # save full html:
    # html_pages <- tibble(
    #   link = link,
    #   doc_hash = doc_hash,
    #   html_doc = html_doc
    # )
    # # push to DB 
    # DBI::dbWriteTable(conn = con, name = "html_pages", 
    #                   value = html_pages %>% dplyr::mutate(across(.cols = !is.character, as.character)),
    #                   append = TRUE
    # ) 
    # 
    
    # main df:
    page_data <- tibble(
      header = doc %>% html_elements(xpath = "//meta[@property='og:title']") %>% html_attr("content"), 
      # or use header = doc %>% html_elements(., ".HeadLine-type_2") %>% html_text2() %>% stringr::str_c(., collapse = " ")
      link = link,
      version = version,
      
      author = doc %>% html_element("p:nth-child(1) em") %>% html_text2() %>% stringr::str_remove(., pattern = "[v]on|[V]on "), # regex: "(?<=[v|V]on\s).*"
      author_img = NA,
      
      date_time_utc = doc %>% html_element(xpath = "//meta[@name='publish-date']") %>% html_attr("content"), # same for tz_original
      # date_time_utc = doc %>% html_element(".Timestamp-default") %>% html_attr("datetime") %>% lubridate::ymd_hms(., tz = "UTC") %>% as.character(),
      date = as.Date(date_time_utc %>% lubridate::ymd_hms(tz = "UTC")) %>% as.character(),
      tz_original = doc %>% html_element(".Timestamp-default") %>% html_attr("datetime") %>% str_sub(start = -6),
      
      category = doc %>% html_element(".Breadcrumbs-arrow+ .Breadcrumbs-item span") %>% html_text2(),
      
      lead = doc %>% html_elements(xpath = "//meta[@name='description']") %>% html_attr("content"), 
      # or use lead = doc %>% html_element(".p-24 .Text-type_1") %>% html_text2()
      text_article = stringr::str_c(doc %>% html_elements(".ViewText-root p") %>% html_text2(), collapse = " "), # pure article text
      text_all = stringr::str_c(doc %>% html_elements(".ViewText-root") %>% html_text2() %>%  # all text including intextlinks & embeddings
                                  stringr::str_remove_all(., "noscript pattern"),
                                collapse = " "),
      
      cover_img_url = doc %>% html_element(".Cover-image img") %>% html_attr("data-src"),
      cover_img_alt = doc %>% html_element(".Cover-image img") %>% html_attr("alt"),
      cover_img_caption = doc %>% html_element(".Cover-caption") %>% html_text2(),
      cover_img_rights = doc %>% html_element(".Cover-source") %>% html_text2(),
      
      available_online = 1,
      capture_time = capture_time,
      doc_hash = doc_hash
      
    ) # end of main df
    # push to DB  
    DBI::dbWriteTable(conn = con, name = "page_data", 
                      value = page_data %>% dplyr::mutate(across(.cols = !is.character, as.character),
                                                          available_online = as.integer(1)),
                      append = TRUE
    )
    
    
    # category_list df: 
    if("try-error" %in% class(
      category_list <- try(tibble(
        doc_hash = doc_hash,
        category_text = doc %>% html_elements(., ".RTLink-root.RTLink-breadcrumb") %>% html_text2(),
        category_url = paste0(base_url, doc %>% html_nodes(., ".RTLink-root.RTLink-breadcrumb") %>% html_attr("href")),
        version = version
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(category_list)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "category_list",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # alternatively: create dictionary to safe space
      # push to DB
      DBI::dbWriteTable(conn = con, name = "category_list", 
                        value = category_list %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    } # end of category-error handling
    
    
    # tag_list df: 
    if("try-error" %in% class(
      tag_list <- try(tibble(
        doc_hash = doc_hash,
        tag_name = doc %>% html_elements(".Tags-link") %>% html_text2(),
        tag_url = paste0(base_url, doc %>% html_nodes(".Tags-link") %>% html_attr("href")),
        version = version
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(tag_lists)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "tag_lists",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      
      # alternatively create dictionary to safe space
      # push to DB
      DBI::dbWriteTable(conn = con, name = "tag_list", 
                        value = tag_list %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    } # end of tag-error handling
    
    
    if("try-error" %in% class(
      intext_links <- try(tibble(
        doc_hash = doc_hash,
        intext_link_url = ifelse(doc %>% html_nodes(".ViewText-root a") %>% html_attr("href") %>% str_starts("/"),
                                 paste0(base_url, doc %>% html_nodes(".ViewText-root a") %>% html_attr("href")),
                                 doc %>% html_nodes(".ViewText-root a") %>% html_attr("href")
        ),
        intext_link_text = doc %>% html_nodes(".ViewText-root a") %>% html_text2()
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(intext_links)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "intext_links",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      
      # push to DB
      DBI::dbWriteTable(conn = con, name = "intext_links", 
                        value = intext_links %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    }
    
# seems like main recommendations not existent! (but might capture sth else)
    # if("try-error" %in% class(
    #   recommendations_main <- try(tibble(
    #     doc_hash = doc_hash,
    #     main_recommendations_title = doc %>% html_elements("strong+ a") %>% html_text2(),
    #     main_recommendations_link = doc %>% html_elements("strong+ a") %>% html_attr("href"),
    #     main_recommendations_img = NA,
    #     main_recommendations_alt = NA
    #   )
    #   ))) {
    #   cat(Sys.time() %>% toString(), "failed to save", link, "(recommendations_main)", "\n", sep = " ")
    #   write(x = paste(Sys.time(), link, "recommendations_main",  sep = ", "), 
    #         file = logfile, append = T, sep = "\n")
    # } else{
    #   # push to DB
    #   DBI::dbWriteTable(conn = con, name = "recommendations_main", 
    #                     value = recommendations_main %>% dplyr::mutate(across(.cols = !is.character, as.character)),
    #                     append = TRUE
    #   ) 
    # }
    
    
    
    if("try-error" %in% class(
      recommendations_embedded <- try(tibble(
        doc_hash = doc_hash,
# sometimes thumbnail/title have different length!
        recommendation_embedded_thumbnail = doc %>% html_elements(".Card-imageWrap noscript img") %>%  html_attr("src"),
        recommendation_embedded_title = doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_text2(),
        recommendation_embedded_link = ifelse(stringr::str_length(doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_attr("href")) > 0,
                                              stringr::str_c(base_url, doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_attr("href")),
                                              NA)
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(recommendations_embedded)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "recommendations_embedded",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # push to DB
      DBI::dbWriteTable(conn = con, name = "recommendations_embedded", 
                        value = recommendations_embedded %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    }
    
    
    
    if("try-error" %in% class(
      images <- try(tibble(
        doc_hash = doc_hash,
        image_url = doc %>% html_elements(".RTImage-image.RTImage-original picture img") %>% html_attr("data-src"),
        image_caption = doc %>% html_elements(".RTImage-caption") %>% html_text2(),
        image_source = doc %>% html_elements(".RTImage-source") %>% html_text2()
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(images)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "images",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # push to DB
      DBI::dbWriteTable(conn = con, name = "images", 
                        value = images %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    }
    
    
    # Embeddings:  
    not_include <- c()
    
    if("try-error" %in% class(
      youtube_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = if (length(doc %>% html_elements(".YouTubeEmbed") %>% html_attr("id")) > 0) {
          paste0("https://www.youtube.com/embed/", doc %>% html_elements(".YouTubeEmbed") %>% html_attr("id"))   
        } else{doc %>% html_elements(".YouTubeEmbed") %>% html_attr("id")},
        source = "youtube"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(youtube_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "youtube_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "youtube_embeddings")
    }
    
    
    if("try-error" %in% class(
      twitter_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']/*/a") %>% html_attr("href"),
        source = "twitter"
        # embeddings_twitter_date = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']/*/a") %>% html_text2() %>% lubridate::parse_date_time(., "%B %d, %Y") %>% as.character()
        # embeddings_twitter_text = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']") %>% html_text2()
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(twitter_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "twitter_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "twitter_embeddings")
    }
    
    
    
    if("try-error" %in% class(
      odysee_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".EmbedBlock-odysee iframe") %>% html_attr("data-src"),
        source = "odysee"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(odysee_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "odysee_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "odysee_embeddings")
    }
    
    
    if("try-error" %in% class(
      telegram_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = if (length(doc %>% html_elements(".TelegramEmbed script") %>% html_attr("data-telegram-post")) > 0) {
          paste0("https://t.me/", doc %>% html_elements(".TelegramEmbed script") %>% html_attr("data-telegram-post"))   
        } else{doc %>% html_elements(".TelegramEmbed script") %>% html_attr("data-telegram-post")},
        source = "telegram"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(telegram_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "telegram_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "telegram_embeddings")
    } 
    
    
    # other (Podcast etc.):
    if("try-error" %in% class(
      vk_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".VkEmbed iframe") %>% html_attr("data-src"),
        source = "vk")
      )
      )) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(telegram_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "telegram_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "telegram_embeddings")
    } 
    
    
    # other (Podcast etc.):
    if("try-error" %in% class(
      podbean_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".AllEmbed iframe") %>% html_attr("src"),
        source = ifelse(doc %>% html_elements(".AllEmbed iframe") %>% as.character() %>% str_detect("podbean"),
                        "podbean", # seems not be used
                        "other")
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(podbean_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "podbean_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "podbean_embeddings")
    }     
    
    
    # check which embeddings are no error:
    all_embeddings <- c("youtube_embeddings", "twitter_embeddings", "odysee_embeddings", "telegram_embeddings", "vk_embeddings", "podbean_embeddings")  
    valid_embeddings <- all_embeddings[!all_embeddings %in% not_include]
    paste0("dplyr::bind_rows(", 
           str_flatten(valid_embeddings, collapse = ", "),
           ")")
    
    if("try-error" %in% class(
      embeddings <- try(eval(parse(text = valid_embeddings))) # %>% unique()
    )) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # push to DB
      DBI::dbWriteTable(conn = con, name = "embeddings", 
                        value = embeddings %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    }
  } # end of error-handling
} # end of scraper_es


# RT english:
scraper_en <- function(link, version, con, 
                       lastmod, lastmod_tz, logfile = log_file){
  # ~ scraper_fr ~ scraper_ru

# hard code to avoid weird error:
logfile = "scrapelog_rt.txt"

  
  require(DBI)
  require(rvest)
  require(dplyr)
  require(stringr)  
  
    # get page:
    base_url <- "https://www.rt.com"
    
    # try and record as missing if not available
    if("try-error" %in% class(
      doc <- try(rvest::read_html(link)
      ))) {
      
      catch_not_captured(link, version, con)
      cat(Sys.time() %>% toString(), "failed to save", link, "(main)", "\n", sep = " ")
      # write in logfile:
      write(x = paste(Sys.time(), link, "main",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      
    } else{ # if page available
      
      capture_time <- Sys.time()
      html_doc <- toString(doc)
      doc_hash <- rlang::hash(html_doc)
      
      
      # get page_data:
      
      # save full html:
      # html_pages <- tibble(
      #   link = link,
      #   doc_hash = doc_hash,
      #   html_doc = html_doc
      # )
      # # push to DB 
      # DBI::dbWriteTable(conn = con, name = "html_pages", 
      #                   value = html_pages %>% dplyr::mutate(across(.cols = !is.character, as.character)),
      #                   append = TRUE
      # ) 
      
      
      # main df:
      page_data <- tibble(
        header = doc %>% html_elements(xpath = "//meta[@property='og:title']") %>% html_attr("content"), 
        # or use header = doc %>% html_elements(., ".HeadLine-type_2") %>% html_text2() %>% stringr::str_c(., collapse = " ")
        link = link,
        version = version,
        
        author = doc %>% html_elements(".article__author-text em strong") %>% html_text2() %>% str_flatten_comma(.) %>% 
          ifelse(rlang::is_empty(.) | . == "",
                 doc %>% html_elements(xpath = "//meta[@name='article:author']") %>% html_attr("content"),
                 .),
        author_img = doc %>% html_elements(xpath = "//*[contains(./@class, 'log-aut')]//*//img") %>% html_attr("src") %>% ifelse(rlang::is_empty(.), NA , .),

#possible errors:                
        date_time_utc = ifelse(rlang::is_empty(doc %>% html_elements(xpath = "//meta[@name='published_time_telegram']") %>% html_attr("content")  %>% lubridate::ymd_hms(., tz = "UTC")),
            lastmod %>% as.character(),
            doc %>% html_elements(xpath = "//meta[@name='published_time_telegram']") %>% html_attr("content")  %>% lubridate::ymd_hms(., tz = "UTC") %>% as.character()
            ),
  
        # doc %>% html_elements(xpath = "//script[@type='application/ld+json']") %>% html_text2() %>%
        #                         str_extract(., '\"datePublished\": \"(.*)\"') %>% na.omit() %>% str_sub(19, 43) %>%
        #                         lubridate::ymd_hms(., tz = "UTC"),
        #         
        # ,
 #       date_time_utc = doc %>% html_element(".Timestamp-default") %>% html_attr("datetime") %>% lubridate::ymd_hms(., tz = "UTC") %>% as.character(),
        date = as.Date(date_time_utc %>% lubridate::ymd_hms(tz = "UTC")) %>% as.character(),
        tz_original = doc %>% html_elements(xpath = "//meta[@name='published_time_telegram']") %>% html_attr("content") %>% str_sub(start = -6) %>% 
          ifelse(rlang::is_empty(.),
            lastmod_tz,
            .),
        
        category = doc %>% html_element(".breadcrumbs__links:last-child") %>% html_text2(),
        
        lead = doc %>% html_elements(xpath = "//meta[@name='description']") %>% html_attr("content"), 
        # or use lead = doc %>% html_element(".p-24 .Text-type_1") %>% html_text2()
        text_article = stringr::str_c(doc %>% html_elements(".article__text p") %>% html_text2(), collapse = " "), # pure article text
        
 
 # 
        text_all = stringr::str_c(doc %>% html_elements(".article__text") %>% html_text2(), collapse = " ") %>%  # all text including intextlinks & embeddings
                                    stringr::str_remove_all(., "noscript pattern"),
        
        cover_img_url = doc %>% html_element(".article__cover img") %>% html_attr("data-src"),
        cover_img_alt = doc %>% html_element(".article__cover img") %>% html_attr("alt"),
        cover_img_caption = doc %>% html_elements(xpath = "//div[contains(./@class, 'media__title_arcticle')]//span[@data-role='title']") %>% html_text2() %>% 
          ifelse(rlang::is_empty(.), NA, .),
        cover_img_rights = paste0(
          doc %>% html_elements(xpath = "//div[contains(./@class, 'media__title_arcticle')]//span[@data-role='source']") %>% html_text2(),
          doc %>% html_elements(xpath = "//div[contains(./@class, 'media__title_arcticle')]//span[@data-role='copyright']") %>% html_text2()
        )  %>% ifelse(rlang::is_empty(.), NA , .),
        
        available_online = 1,
        capture_time = capture_time,
        doc_hash = doc_hash
        
      ) # end of main df
      # push to DB  
      DBI::dbWriteTable(conn = con, name = "page_data", 
                        value = page_data %>% dplyr::mutate(across(.cols = !is.character, as.character),
                                                            available_online = as.integer(1)),
                        append = TRUE
      )
      
      
      # category_list df: 
      if("try-error" %in% class(
        category_list <- try(tibble(
          doc_hash = doc_hash,
          category_text = doc %>% html_elements(".breadcrumbs__links") %>% html_text2(),
          category_url = paste0(base_url, doc %>% html_elements(".breadcrumbs__links") %>% html_attr("href")),
          version = version
        )
        ))) {
        cat(Sys.time() %>% toString(), "failed to save", link, "(category_list)", "\n", sep = " ")
        write(x = paste(Sys.time(), link, "category_list",  sep = ", "), 
              file = logfile, append = T, sep = "\n")
      } else{
        # alternatively: create dictionary to safe space
        # push to DB
        DBI::dbWriteTable(conn = con, name = "category_list", 
                          value = category_list %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                          append = TRUE
        ) 
      } # end of category-error handling
      
      
      # tag_list df: 
      if("try-error" %in% class(
        tag_list <- try(tibble(
          doc_hash = doc_hash,
          tag_name = doc %>% html_elements(xpath = "//a[contains(./@class, 'tags-')]") %>% html_text2(),
          tag_url = paste0(base_url, doc %>% html_elements(xpath = "//a[contains(./@class, 'tags-')]") %>% html_attr("href")),
          version = version
        )
        ))) {
        cat(Sys.time() %>% toString(), "failed to save", link, "(tag_lists)", "\n", sep = " ")
        write(x = paste(Sys.time(), link, "tag_lists",  sep = ", "), 
              file = logfile, append = T, sep = "\n")
      } else{
        
        # alternatively create dictionary to safe space
        # push to DB
        DBI::dbWriteTable(conn = con, name = "tag_list", 
                          value = tag_list %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                          append = TRUE
        ) 
      } # end of tag-error handling
      
      
      if("try-error" %in% class(
        intext_links <- try(tibble(
          doc_hash = doc_hash,
          intext_link_url = ifelse(doc %>% html_nodes(".article__text p a") %>% html_attr("href") %>% str_starts("/"),
                                   paste0(base_url, doc %>% html_nodes(".article__text p a") %>% html_attr("href")),
                                   doc %>% html_nodes(".article__text p a") %>% html_attr("href")
          ),
          intext_link_text = doc %>% html_nodes(".article__text p a") %>% html_text2()
        )
        ))) {
        cat(Sys.time() %>% toString(), "failed to save", link, "(intext_links)", "\n", sep = " ")
        write(x = paste(Sys.time(), link, "intext_links",  sep = ", "), 
              file = logfile, append = T, sep = "\n")
      } else{
        
        # push to DB
        DBI::dbWriteTable(conn = con, name = "intext_links", 
                          value = intext_links %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                          append = TRUE
        ) 
      }
      
    
    # .read-more-big with img ( ; https://www.rt.com/sport/527239-ordabasy-kazakhstan-football-team-bus-video/ ; https://www.rt.com/sport/535717-climber-johanna-farber-instagram-buttocks-row/)  
  #ToDo: sometimes incompatible sizes @ img!    
      
      if("try-error" %in% class(
        recommendations_main_big <- try(tibble(
          doc_hash = doc_hash,
          main_recommendations_title = doc %>% html_elements(".read-more-big__title") %>% html_text2(),
          main_recommendations_link = doc %>% html_elements(".Read-more-big") %>% html_attr("href") %>% ifelse(str_starts(., "/"), paste0(base_url, .), .),
          main_recommendations_img = doc %>% html_elements(".read-more-big__cover") %>% html_attr("data-bgset") %>% str_extract(., "https.*.jpg"),
          main_recommendations_alt = NA
        )
        ))) {
        cat(Sys.time() %>% toString(), "failed to save", link, "(recommendations_main_big)", "\n", sep = " ")
        write(x = paste(Sys.time(), link, "recommendations_main_big",  sep = ", "), 
              file = logfile, append = T, sep = "\n")
      } else{
        # push to DB
        DBI::dbWriteTable(conn = con, name = "recommendations_main", 
                          value = recommendations_main_big %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                          append = TRUE
        ) 
      }
      
      # main big:
      if("try-error" %in% class(
        recommendations_main <- try(tibble(
          doc_hash = doc_hash,
          main_recommendations_title = doc %>% html_elements(".Read-more-text-only a") %>% html_text2(),
          main_recommendations_link = doc %>% html_elements(".Read-more-text-only a") %>% html_attr("href") %>% ifelse(str_starts(., "/"), paste0(base_url, .), .),
          main_recommendations_img = NA,
          main_recommendations_alt = NA
        )
        ))) {
        cat(Sys.time() %>% toString(), "failed to save", link, "(recommendations_main)", "\n", sep = " ")
        write(x = paste(Sys.time(), link, "recommendations_main",  sep = ", "), 
              file = logfile, append = T, sep = "\n")
      } else{
        # push to DB
        DBI::dbWriteTable(conn = con, name = "recommendations_main", 
                          value = recommendations_main %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                          append = TRUE
        ) 
      }
      
## continue here!      
      
      if("try-error" %in% class(
        recommendations_embedded <- try(tibble(
          doc_hash = doc_hash,
          recommendation_embedded_thumbnail = doc %>% html_elements(".read-more noscript img") %>%  html_attr("src"),
          recommendation_embedded_title = doc %>% html_elements(".read-more span") %>% html_text2(),
          recommendation_embedded_link = doc %>% html_elements(".read-more a") %>% html_attr("href") %>% stringr::str_c(base_url, .)
        )
        ))) {
        cat(Sys.time() %>% toString(), "failed to save", link, "(recommendations_embedded)", "\n", sep = " ")
        write(x = paste(Sys.time(), link, "recommendations_embedded",  sep = ", "), 
              file = logfile, append = T, sep = "\n")
      } else{
        # push to DB
        DBI::dbWriteTable(conn = con, name = "recommendations_embedded", 
                          value = recommendations_embedded %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                          append = TRUE
        ) 
      }
      
      
      
      if("try-error" %in% class(
        images <- try(tibble(
          doc_hash = doc_hash,
          image_url = doc %>% html_elements(".media .read-more__cover") %>% html_attr("data-src"),
          image_caption = doc %>% html_elements(".media__title_footer") %>% html_text2() %>% str_remove(., "\u00A9.*") %>% str_trim(),
          image_source = doc %>% html_elements(".media__title_footer") %>% html_text2() %>% str_replace(., "(.*?)(\u00A9.*?)", "\\2") %>% str_trim()
        )
        ))) {
        cat(Sys.time() %>% toString(), "failed to save", link, "(images)", "\n", sep = " ")
        write(x = paste(Sys.time(), link, "images",  sep = ", "), 
              file = logfile, append = T, sep = "\n")
      } else{
        # push to DB
        DBI::dbWriteTable(conn = con, name = "images", 
                          value = images %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                          append = TRUE
        ) 
      }
      
      
      # Embeddings:  
      not_include <- c()
      
      if("try-error" %in% class(
        youtube_embeddings <- try(tibble(
          doc_hash = doc_hash,
          embedding_url = doc %>% html_elements(".media__youtube-frame") %>% html_attr("data-src"),
          source = "youtube"
        )
        ))) {
        cat(Sys.time() %>% toString(), "failed to save", link, "(youtube_embeddings)", "\n", sep = " ")
        write(x = paste(Sys.time(), link, "youtube_embeddings",  sep = ", "), 
              file = logfile, append = T, sep = "\n")
        not_include <- append(not_include, "youtube_embeddings")
      }
      
      
      if("try-error" %in% class(
        twitter_embeddings <- try(tibble(
          doc_hash = doc_hash,
          embedding_url = doc %>% html_elements(".twitter-tweet") %>% html_children() %>%  html_attr("href") %>% na.omit(),
          source = "twitter"
          # embeddings_twitter_date = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']/*/a") %>% html_text2() %>% lubridate::parse_date_time(., "%B %d, %Y") %>% as.character()
          # embeddings_twitter_text = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']") %>% html_text2()
        )
        ))) {
        cat(Sys.time() %>% toString(), "failed to save", link, "(twitter_embeddings)", "\n", sep = " ")
        write(x = paste(Sys.time(), link, "twitter_embeddings",  sep = ", "), 
              file = logfile, append = T, sep = "\n")
        not_include <- append(not_include, "twitter_embeddings")
      }
      
      
      
      if("try-error" %in% class(
        odysee_embeddings <- try(tibble(
          doc_hash = doc_hash,
          embedding_url = doc %>% html_elements(".EmbedBlock-odysee iframe") %>% html_attr("data-src"),
          source = "odysee"
        )
        ))) {
        cat(Sys.time() %>% toString(), "failed to save", link, "(odysee_embeddings)", "\n", sep = " ")
        write(x = paste(Sys.time(), link, "odysee_embeddings",  sep = ", "), 
              file = logfile, append = T, sep = "\n")
        not_include <- append(not_include, "odysee_embeddings")
      }
      
      
      if("try-error" %in% class(
        telegram_embeddings <- try(tibble(
          doc_hash = doc_hash,
          embedding_url = doc %>% html_elements(".rtcode script") %>% html_attr("data-telegram-post") %>% paste0("https://t.me/", .),
          source = "telegram"
        )
        ))) {
        cat(Sys.time() %>% toString(), "failed to save", link, "(telegram_embeddings)", "\n", sep = " ")
        write(x = paste(Sys.time(), link, "telegram_embeddings",  sep = ", "), 
              file = logfile, append = T, sep = "\n")
        not_include <- append(not_include, "telegram_embeddings")
      } 
      
      # Instagram:
      if("try-error" %in% class(
        instagram_embeddings1 <- try(tibble(
          doc_hash = doc_hash,
          embedding_url = doc %>% html_elements(".instagram-media") %>% html_attr("data-instgrm-permalink"),
          source = "instagram"
        )
        ))) {
        cat(Sys.time() %>% toString(), "failed to save", link, "(instagram_embeddings1)", "\n", sep = " ")
        write(x = paste(Sys.time(), link, "instagram_embeddings1",  sep = ", "), 
              file = logfile, append = T, sep = "\n")
        not_include <- append(not_include, "instagram_embeddings1")
      } 
      
      # Instagram:
      if("try-error" %in% class(
        instagram_embeddings2 <- try(tibble(
          doc_hash = doc_hash,
          embedding_url = doc %>% html_elements(".instagram-media a") %>% html_attr("href"),
          source = "instagram"
        )
        ))) {
        cat(Sys.time() %>% toString(), "failed to save", link, "(instagram_embeddings2)", "\n", sep = " ")
        write(x = paste(Sys.time(), link, "instagram_embeddings2",  sep = ", "), 
              file = logfile, append = T, sep = "\n")
        not_include <- append(not_include, "instagram_embeddings2")
      } 
      
      # Tiktok:
      if("try-error" %in% class(
        tiktok_embeddings <- try(tibble(
          doc_hash = doc_hash,
          embedding_url = doc %>% html_elements(".tiktok-embed") %>% html_attr("cite"),
          source = "tiktok"
        )
        ))) {
        cat(Sys.time() %>% toString(), "failed to save", link, "(tiktok_embeddings)", "\n", sep = " ")
        write(x = paste(Sys.time(), link, "tiktok_embeddings",  sep = ", "), 
              file = logfile, append = T, sep = "\n")
        not_include <- append(not_include, "tiktok_embeddings")
      } 
      
      
      # other (Podcast etc.):
      if("try-error" %in% class(
        vk_embeddings <- try(tibble(
          doc_hash = doc_hash,
          embedding_url = doc %>% html_elements("iframe") %>% html_attr("src"),
          source = case_when(doc %>% html_elements("iframe") %>% as.character() %>% str_detect("vk.com") ~ "vk",
                             doc %>% html_elements("iframe") %>% as.character() %>% str_detect("youtube.com") ~ "youtube",
                             T ~ "other")
        )
        ))) {
        cat(Sys.time() %>% toString(), "failed to save", link, "(telegram_embeddings)", "\n", sep = " ")
        write(x = paste(Sys.time(), link, "telegram_embeddings",  sep = ", "), 
              file = logfile, append = T, sep = "\n")
        not_include <- append(not_include, "telegram_embeddings")
      } 
      
      # RT Podcast:
      if("try-error" %in% class(
        rt_podcast_embeddings <- try(tibble(
          doc_hash = doc_hash,
          embedding_url = doc %>% html_elements(".podcast-view-card") %>% html_attr("data-src"),
          source = "rt_podcast"
        )
        ))) {
        cat(Sys.time() %>% toString(), "failed to save", link, "(rt_podcast_embeddings)", "\n", sep = " ")
        write(x = paste(Sys.time(), link, "rt_podcast_embeddings",  sep = ", "), 
              file = logfile, append = T, sep = "\n")
        not_include <- append(not_include, "rt_podcast_embeddings")
      }     
      
      # other podcast (e.g. soundcloud)
      if("try-error" %in% class(
        other_podcast_embeddings <- try(tibble(
          doc_hash = doc_hash,
          embedding_url = doc %>% html_elements(".article__podcast iframe") %>% html_attr("data-src"),
          source = ifelse(doc %>% html_elements(".article__podcast iframe") %>% html_attr("data-src") %>% str_detect("soundcloud"),
                          "soundcloud",
                          "other_podcast")
        )
        ))) {
        cat(Sys.time() %>% toString(), "failed to save", link, "(other_podcast_embeddings)", "\n", sep = " ")
        write(x = paste(Sys.time(), link, "other_podcast_embeddings",  sep = ", "), 
              file = logfile, append = T, sep = "\n")
        not_include <- append(not_include, "other_podcast_embeddings")
      }     
      
      
      # other (Podcast etc.):
      if("try-error" %in% class(
        podbean_embeddings <- try(tibble(
          doc_hash = doc_hash,
          embedding_url = doc %>% html_elements(".AllEmbed iframe") %>% html_attr("src"),
          source = ifelse(doc %>% html_elements(".AllEmbed iframe") %>% as.character() %>% str_detect("podbean"),
                          "podbean",
                          "other")
        )
        ))) {
        cat(Sys.time() %>% toString(), "failed to save", link, "(podbean_embeddings)", "\n", sep = " ")
        write(x = paste(Sys.time(), link, "podbean_embeddings",  sep = ", "), 
              file = logfile, append = T, sep = "\n")
        not_include <- append(not_include, "podbean_embeddings")
      }     
      
      
      # check which embeddings are no error:
      all_embeddings <- c("youtube_embeddings", "twitter_embeddings", "odysee_embeddings", "telegram_embeddings", "tiktok_embeddings", "vk_embeddings", "instagram_embeddings1", "instagram_embeddings2", "rt_podcast_embeddings", "podbean_embeddings", "other_podcast_embeddings")  
      valid_embeddings <- all_embeddings[!all_embeddings %in% not_include]
      paste0("dplyr::bind_rows(", 
             str_flatten(valid_embeddings, collapse = ", "),
             ")")
      
      if("try-error" %in% class(
        embeddings <- try(eval(parse(text = valid_embeddings))) # %>% unique()
      )) {
        cat(Sys.time() %>% toString(), "failed to save", link, "(embeddings)", "\n", sep = " ")
        write(x = paste(Sys.time(), link, "embeddings",  sep = ", "), 
              file = logfile, append = T, sep = "\n")
      } else{
        # push to DB
        DBI::dbWriteTable(conn = con, name = "embeddings", 
                          value = embeddings %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                          append = TRUE
        ) 
      }
    } # end of error-handling
  } # end of scraper_en


#RT francais: 
scraper_fr <- function(link, version, con, 
                       lastmod, lastmod_tz, logfile = log_file){
  # ~ scraper_en ~ scraper_ru

# hard code to avoid weird error:
logfile = "scrapelog_rt.txt"

  require(DBI)
  require(rvest)
  require(dplyr)
  require(stringr)
  require(anytime)
  
  # get page:
  base_url <- "https://francais.rt.com"
  
  # try and record as missing if not available
  if("try-error" %in% class(
    doc <- try(rvest::read_html(link)
    ))) {
    
    catch_not_captured(link, version, con)
    cat(Sys.time() %>% toString(), "failed to save", link, "(main)", "\n", sep = " ")
    # write in logfile:
    write(x = paste(Sys.time(), link, "main",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
    
  } else{ # if page available
    
    capture_time <- Sys.time()
    html_doc <- toString(doc)
    doc_hash <- rlang::hash(html_doc)
    
    
    # get page_data:
    
    # save full html:
    # html_pages <- tibble(
    #   link = link,
    #   doc_hash = doc_hash,
    #   html_doc = html_doc
    # )
    # # push to DB 
    # DBI::dbWriteTable(conn = con, name = "html_pages", 
    #                   value = html_pages %>% dplyr::mutate(across(.cols = !is.character, as.character)),
    #                   append = TRUE
    # ) 
    
    
    # main df:
    page_data <- tibble(
      header = doc %>% html_elements(xpath = "//meta[@property='og:title']") %>% html_attr("content"), 
      # or use header = doc %>% html_elements(., ".HeadLine-type_2") %>% html_text2() %>% stringr::str_c(., collapse = " ")
      link = link,
      version = version,
      
      author = doc %>% html_elements(xpath = "//span[@class='blog-autor__name']/text()") %>% html_text2() %>% 
           ifelse(rlang::is_empty(.), NA , .),
      author_img = doc %>% html_elements(".blog-autor__image") %>% html_attr("style")  %>% str_extract(., "https.*.[jpg|JPG]") %>% 
        ifelse(rlang::is_empty(.), NA , .),
      
      date_time_utc = ifelse(rlang::is_empty(
                            doc %>% html_elements(xpath = "//meta[@name='publish-date']") %>% html_attr("content")),
                            lastmod %>% as.character(),
                            doc %>% html_elements(xpath = "//meta[@name='publish-date']") %>% html_attr("content") %>% anytime()  %>% lubridate::ymd_hms(., tz = "UTC") %>% as.character()
      ),
      
      # doc %>% html_elements(xpath = "//script[@type='application/ld+json']") %>% html_text2() %>%
      #                         str_extract(., '\"datePublished\": \"(.*)\"') %>% na.omit() %>% str_sub(19, 43) %>%
      #                         lubridate::ymd_hms(., tz = "UTC"),
      #         
      # ,
      #       date_time_utc = doc %>% html_element(".Timestamp-default") %>% html_attr("datetime") %>% lubridate::ymd_hms(., tz = "UTC") %>% as.character(),
      date = as.Date(date_time_utc %>% lubridate::ymd_hms(tz = "UTC")) %>% as.character(),
      tz_original = doc %>% html_elements(xpath = "//meta[@name='publish-date']") %>% html_attr("content") %>% str_sub(., start = -5) %>% gsub(paste0("^(.{", 3, "})(.*)$"), paste0("\\1", ":", "\\2"), .) %>% 
        ifelse(rlang::is_empty(.),
               lastmod_tz,
               .),
      
      category = doc %>% html_elements(xpath = "//span[@class='breadcrumbs-item'][last()]") %>% html_text2(),
      
      lead = doc %>% html_elements(xpath = "//meta[@name='description']") %>% html_attr("content"), 
      # or use lead = doc %>% html_element(".p-24 .Text-type_1") %>% html_text2()
      text_article = stringr::str_c(doc %>% html_elements(xpath = "//div[@class='article__text']//p[not(@class='text__summary')]") %>% html_text2(), collapse = " "), # pure article text
      
      
      text_all = stringr::str_c(doc %>% html_elements(".article__text") %>% html_text2(), collapse = " ") %>%  # all text including intextlinks & embeddings
        stringr::str_remove_all(., "noscript pattern"),
      
      cover_img_url = doc %>% html_element(".article__cover img") %>% html_attr("data-src") %>% 
        ifelse(rlang::is_empty(.), NA , .),
      cover_img_alt = doc %>% html_element(".article__cover img") %>% html_attr("alt") %>% 
        ifelse(rlang::is_empty(.), NA , .),
      cover_img_caption = doc %>% html_element(".media__title") %>% html_text2() %>% 
        ifelse(rlang::is_empty(.), NA, .),
      cover_img_rights = 
        doc %>% html_element(xpath = "//small[contains(./@class, 'media__copyright')]") %>% html_text2() %>% 
        ifelse(rlang::is_empty(.) || !str_detect(., "Source"), NA , .), # remove everything until source
      
      available_online = 1,
      capture_time = capture_time,
      doc_hash = doc_hash
      
    ) # end of main df
    # push to DB  
    DBI::dbWriteTable(conn = con, name = "page_data", 
                      value = page_data %>% dplyr::mutate(across(.cols = !is.character, as.character),
                                                          available_online = as.integer(1)),
                      append = TRUE
    )
    
    
    # category_list df: 
    if("try-error" %in% class(
      category_list <- try(tibble(
        doc_hash = doc_hash,
        category_text = doc %>% html_elements(".breadcrumbs-item span") %>% html_text2(),
        category_url = paste0(base_url, doc %>% html_elements(".breadcrumbs-item a") %>% html_attr("href")),
        version = version
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(category_list)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "category_list",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # alternatively: create dictionary to safe space
      # push to DB
      DBI::dbWriteTable(conn = con, name = "category_list", 
                        value = category_list %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    } # end of category-error handling
    
    
   # tag_list df: 
    if("try-error" %in% class(
      tag_list <- try(tibble(
        doc_hash = doc_hash,
        tag_name = doc %>% html_elements(".article__category") %>% html_text2(),
        tag_url = paste0(base_url, doc %>% html_elements(".article__category a") %>% html_attr("href")),
        version = version
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(tag_lists)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "tag_lists",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      
      # alternatively create dictionary to safe space
      # push to DB
      DBI::dbWriteTable(conn = con, name = "tag_list", 
                        value = tag_list %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    } # end of tag-error handling    
    
   # intext_links 
    if("try-error" %in% class(
      intext_links <- try(tibble(
        doc_hash = doc_hash,
        intext_link_url = ifelse(doc %>% html_nodes(".article__text p a") %>% html_attr("href") %>% str_starts("/"),
                                 paste0(base_url, doc %>% html_nodes(".article__text p a") %>% html_attr("href")),
                                 doc %>% html_nodes(".article__text p a") %>% html_attr("href")
        ),
        intext_link_text = doc %>% html_nodes(".article__text p a") %>% html_text2()
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(intext_links)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "intext_links",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      
      # push to DB
      DBI::dbWriteTable(conn = con, name = "intext_links", 
                        value = intext_links %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    }
    
    
    # .read-more-big with img ( ; https://www.rt.com/sport/527239-ordabasy-kazakhstan-football-team-bus-video/ ; https://www.rt.com/sport/535717-climber-johanna-farber-instagram-buttocks-row/)  
    #ToDo: sometimes incompatible sizes @ img!    
    
    if("try-error" %in% class(
      recommendations_main_big <- try(tibble(
        doc_hash = doc_hash,
        main_recommendations_title = doc %>% html_elements(".read-more-big__title") %>% html_text2(),
        main_recommendations_link = doc %>% html_elements(".read-more-big") %>% html_attr("href") %>% ifelse(str_starts(., "/"), paste0(base_url, .), .),
        main_recommendations_img = doc %>% html_elements(".read-more-big__cover") %>% html_attr("style") %>% str_extract(., "https.*.[jpg|JPG]"),
        main_recommendations_alt = NA
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(recommendations_main_big)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "recommendations_main_big",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # push to DB
      DBI::dbWriteTable(conn = con, name = "recommendations_main", 
                        value = recommendations_main_big %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    }
    
    # main big:
    if("try-error" %in% class(
      recommendations_main <- try(tibble(
        doc_hash = doc_hash,
        main_recommendations_title = doc %>% html_elements(".read-more-text-only a") %>% html_text2(),
        main_recommendations_link = doc %>% html_elements(".read-more-text-only a") %>% html_attr("href") %>% ifelse(str_starts(., "/"), paste0(base_url, .), .),
        main_recommendations_img = NA,
        main_recommendations_alt = NA
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(recommendations_main)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "recommendations_main",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # push to DB
      DBI::dbWriteTable(conn = con, name = "recommendations_main", 
                        value = recommendations_main %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    }
    
    # main unframed text:
    if("try-error" %in% class(
      recommendations_main_text <- try(tibble(
        doc_hash = doc_hash,
        main_recommendations_title = doc %>% html_elements(xpath = "//strong[contains(text(), 'Lire aussi :')]/parent::a") %>% html_text2(),
        main_recommendations_link = doc %>% html_elements(xpath = "//strong[contains(text(), 'Lire aussi :')]/parent::a") %>% html_text2() %>% str_remove("Lire aussi :") %>% str_trim(),
        main_recommendations_img = NA,
        main_recommendations_alt = NA
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(recommendations_main_text)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "recommendations_main_text",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # push to DB
      DBI::dbWriteTable(conn = con, name = "recommendations_main", 
                        value = recommendations_main_text %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    }
    
    if("try-error" %in% class(
      recommendations_embedded <- try(tibble(
        doc_hash = doc_hash,
        recommendation_embedded_thumbnail = doc %>% html_elements(".read-more__cover") %>%  html_attr("src"),
        recommendation_embedded_title = doc %>% html_elements(".read-more__footer") %>% html_text2(),
        recommendation_embedded_link = doc %>% html_elements(".article__read-more a") %>% html_attr("href") %>% stringr::str_c(base_url, .)
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(recommendations_embedded)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "recommendations_embedded",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # push to DB
      DBI::dbWriteTable(conn = con, name = "recommendations_embedded", 
                        value = recommendations_embedded %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    }
    
    
   # images    
    if("try-error" %in% class(
      images <- try(tibble(
        doc_hash = doc_hash,
        image_url = doc %>% html_elements(".article__text .media__image .media__item") %>% html_attr("src"),
        image_caption = doc %>% html_elements(xpath = "//div[contains(@class, 'article__text')]//div[contains(@class, 'media__image')]/parent::*") %>% html_text2() %>% gsub(pattern = paste0(c(doc %>% html_elements(xpath = "//div[contains(@class, 'article__text')]//div[contains(@class, 'media__image')]") %>% html_text2() %>% unique() %>% stringi::stri_remove_empty(), "[\r\n]"), collapse = "|"), replacement = "", x = .), 
        image_source = doc %>% html_elements(xpath = "//div[contains(@class, 'article__text')]//div[contains(@class, 'media__image')]") %>% html_text2()
      ) %>% dplyr::mutate(across(.cols = !is.character, as.character))
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(images)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "images",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # push to DB
      DBI::dbWriteTable(conn = con, name = "images", 
                        value = images ,
                        append = TRUE
      ) 
    }
    
    # Embeddings:  
    
    not_include <- c()
    
    # rt videos:  
    if("try-error" %in% class(
      rtvideos_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".media__video script") %>% html_text2() %>% str_extract(., "https.*.mp4") %>% na.omit() %>% unique(),
        source = "rt_video"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(rtvideos_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "rtvideos_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "rtvideos_embeddings")
    }
    
    
    if("try-error" %in% class(
      youtube_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".media__youtube-frame") %>% html_attr("src"),
        source = "youtube"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(youtube_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "youtube_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "youtube_embeddings")
    }
    
    # catches different kinds of twitter embeddings (see https://francais.rt.com/france/80747-jvais-te-faire-samuel-paty-mila-nouveau-menacee-mort-video-anti-islam) 
    if("try-error" %in% class(
      twitter_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".twitter-tweet") %>% html_children() %>%  html_attr("href") %>% na.omit(),
        source = "twitter"
        # embeddings_twitter_date = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']/*/a") %>% html_text2() %>% lubridate::parse_date_time(., "%B %d, %Y") %>% as.character()
        # embeddings_twitter_text = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']") %>% html_text2()
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(twitter_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "twitter_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "twitter_embeddings")
    }
    
    
# probably not existent:    
    if("try-error" %in% class(
      odysee_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".EmbedBlock-odysee iframe") %>% html_attr("data-src"),
        source = "odysee"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(odysee_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "odysee_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "odysee_embeddings")
    }
    
    
    if("try-error" %in% class(
      telegram_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".rtcode script") %>% html_attr("data-telegram-post") %>% paste0("https://t.me/", .),
        source = "telegram"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(telegram_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "telegram_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "telegram_embeddings")
    } 
    
    # Instagram:
    if("try-error" %in% class(
      instagram_embeddings1 <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".instagram-media") %>% html_attr("data-instgrm-permalink"),
        source = "instagram"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(instagram_embeddings1)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "instagram_embeddings1",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "instagram_embeddings1")
    } 
    
    # Tiktok:
    if("try-error" %in% class(
      tiktok_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".tiktok-embed") %>% html_attr("cite"),
        source = "tiktok"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(tiktok_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "tiktok_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "tiktok_embeddings")
    } 
    
    
  # other (Podcast etc.):
    # probably not used:
    if("try-error" %in% class(
      vk_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements("iframe") %>% html_attr("src"),
        source = case_when(embedding_url %>% str_detect("facebook") ~ "facebook",
                           embedding_url %>% str_detect("soundcloud") ~ "soundcloud",
                           embedding_url %>% str_detect("vk.com") ~ "vk",
                           embedding_url %>% str_detect("t.me") ~ "telegram",
                           embedding_url %>% str_detect("youtube") ~ "youtube",
                           embedding_url %>% str_detect("twitter.com") ~ "twitter",
                           embedding_url %>% str_detect("podbean") ~ "podbean",
                           embedding_url %>% str_detect("odysee") ~ "odysee",
                           TRUE ~ "other")
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(telegram_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "telegram_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "telegram_embeddings")
    } 
    
    # RT Podcast:
    if("try-error" %in% class(
      rt_podcast_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".podcast-view-card") %>% html_attr("data-src"),
        source = "rt_podcast"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(rt_podcast_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "rt_podcast_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "rt_podcast_embeddings")
    }     
    
    # other podcast (e.g. soundcloud)
    if("try-error" %in% class(
      other_podcast_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".article__podcast iframe") %>% html_attr("data-src"),
        source = ifelse(doc %>% html_elements(".article__podcast iframe") %>% html_attr("data-src") %>% str_detect("soundcloud"),
                        "soundcloud",
                        "other_podcast")
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(other_podcast_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "other_podcast_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "other_podcast_embeddings")
    }     

  # rtcode (e.g. facebook/soundcloud)
    if("try-error" %in% class(
      rtcode_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".rtcode iframe") %>% html_attr("src"),
        source = case_when(embedding_url %>% str_detect("facebook") ~ "facebook",
                           embedding_url %>% str_detect("soundcloud") ~ "soundcloud",
                           embedding_url %>% str_detect("vk.com") ~ "vk",
                           embedding_url %>% str_detect("t.me") ~ "telegram",
                           embedding_url %>% str_detect("youtube") ~ "youtube",
                           embedding_url %>% str_detect("twitter.com") ~ "twitter",
                           embedding_url %>% str_detect("podbean") ~ "podbean",
                           embedding_url %>% str_detect("odysee") ~ "odysee",
                           embedding_url %>% str_detect("vine.co") ~ "vine",
                           TRUE ~ "other")
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(rtcode_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "rtcode_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "rtcode_embeddings")
    }     
    
    
    # other (Podcast etc.):
    if("try-error" %in% class(
      podbean_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".AllEmbed iframe") %>% html_attr("src"),
        source = case_when(embedding_url %>% str_detect("facebook") ~ "facebook",
                           embedding_url %>% str_detect("soundcloud") ~ "soundcloud",
                           embedding_url %>% str_detect("vk.com") ~ "vk",
                           embedding_url %>% str_detect("t.me") ~ "telegram",
                           embedding_url %>% str_detect("youtube") ~ "youtube",
                           embedding_url %>% str_detect("twitter.com") ~ "twitter",
                           embedding_url %>% str_detect("podbean") ~ "podbean",
                           embedding_url %>% str_detect("odysee") ~ "odysee",
                           embedding_url %>% str_detect("vine.co") ~ "vine",
                           TRUE ~ "other")
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(podbean_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "podbean_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "podbean_embeddings")
    }     
    
    
    # check which embeddings are no error:
    all_embeddings <- c("rtvideos_embeddings", "youtube_embeddings", "twitter_embeddings", "odysee_embeddings", "telegram_embeddings", "tiktok_embeddings", "vk_embeddings", "instagram_embeddings1", "rt_podcast_embeddings", "podbean_embeddings", "other_podcast_embeddings", "rtcode_embeddings")  
    valid_embeddings <- all_embeddings[!all_embeddings %in% not_include]
    paste0("dplyr::bind_rows(", 
           str_flatten(valid_embeddings, collapse = ", "),
           ")")
    
    if("try-error" %in% class(
      embeddings <- try(eval(parse(text = valid_embeddings))) 
    )) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # push to DB
      DBI::dbWriteTable(conn = con, name = "embeddings", 
                        value = embeddings %>% dplyr::mutate(across(.cols = !is.character, as.character)) %>% unique(),
                        append = TRUE
      ) 
    }
  } # end of error-handling
} # end of scraper_fr


# RT russian:
scraper_ru <- function(link, version, con, 
                       lastmod, lastmod_tz, logfile = log_file
                       ){
  # ~ scraper_en ~ scraper_fr

# hard code to avoid weird error:
logfile = "scrapelog_rt.txt"
    
  require(DBI)
  require(rvest)
  require(dplyr)
  require(stringr)
  require(anytime)
  
  # get page:
  base_url <- "https://russian.rt.com"
  
  # try and record as missing if not available
  if("try-error" %in% class(
    doc <- try(rvest::read_html(link))
  )) {
    
    catch_not_captured(link, version, con)
    cat(Sys.time() %>% toString(), "failed to save", link, "(main)", "\n", sep = " ")
    # write in logfile:
    write(x = paste(Sys.time(), link, "main",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
    
  } else{ # if page available
    
    capture_time <- Sys.time()
    html_doc <- toString(doc)
    doc_hash <- rlang::hash(html_doc)
    
    
    # get page_data:
    
    # save full html:
    # html_pages <- tibble(
    #   link = link,
    #   doc_hash = doc_hash,
    #   html_doc = html_doc
    # )
    # # push to DB 
    # DBI::dbWriteTable(conn = con, name = "html_pages", 
    #                   value = html_pages %>% dplyr::mutate(across(.cols = !is.character, as.character)),
    #                   append = TRUE
    # ) 
    # 
    
    # main df:
    page_data <- tibble(
      header = doc %>% html_elements(xpath = "//meta[@property='og:title']") %>% html_attr("content"), 
      # or use header = doc %>% html_elements(., ".HeadLine-type_2") %>% html_text2() %>% stringr::str_c(., collapse = " ")
      link = link,
      version = version,
      
      author = doc %>% html_elements(".article__author") %>% html_text2() %>% 
        ifelse(rlang::is_empty(.), NA , .),
      author_img = NA,
      
      date_time_utc = (if(!rlang::is_empty(doc %>% html_elements(xpath = "//meta[@name='mediator_published_time']") %>% html_attr("content"))){doc %>% html_elements(xpath = "//meta[@name='mediator_published_time']") %>% html_attr("content")
        } else if (!rlang::is_empty(doc %>% html_elements(xpath = "//meta[@name='published_time_telegram']") %>% html_attr("content"))) {
          doc %>% html_elements(xpath = "//meta[@name='published_time_telegram']") %>% html_attr("content")
        } else{lastmod %>% as.character()}
        # case_when(!rlang::is_empty(doc %>% html_elements(xpath = "//meta[@name='mediator_published_time']") %>% html_attr("content")) ~ doc %>% html_elements(xpath = "//meta[@name='mediator_published_time']") %>% html_attr("content"),
        #                         !rlang::is_empty(doc %>% html_elements(xpath = "//meta[@name='published_time_telegram']") %>% html_attr("content")) ~ doc %>% html_elements(xpath = "//meta[@name='published_time_telegram']") %>% html_attr("content"),
        #                         TRUE ~ lastmod %>% as.character()
                               ) %>% lubridate::ymd_hms(., tz = "UTC") %>% as.character(),  
    
      # doc %>% html_elements(xpath = "//script[@type='application/ld+json']") %>% html_text2() %>%
      #                         str_extract(., '\"datePublished\": \"(.*)\"') %>% na.omit() %>% str_sub(19, 43) %>%
      #                         lubridate::ymd_hms(., tz = "UTC"),
      #         
      # ,
      #       date_time_utc = doc %>% html_element(".Timestamp-default") %>% html_attr("datetime") %>% lubridate::ymd_hms(., tz = "UTC") %>% as.character(),
      date = as.Date(date_time_utc %>% lubridate::ymd_hms(tz = "UTC")) %>% as.character(),
      tz_original = case_when(!rlang::is_empty(doc %>% html_elements(xpath = "//meta[@name='mediator_published_time']") %>% html_attr("content")) ~ doc %>% html_elements(xpath = "//meta[@name='mediator_published_time']") %>% html_attr("content"),
                              !rlang::is_empty(doc %>% html_elements(xpath = "//meta[@name='published_time_telegram']") %>% html_attr("content")) ~ doc %>% html_elements(xpath = "//meta[@name='published_time_telegram']") %>% html_attr("content"),
                              TRUE ~ lastmod %>% as.character()) %>% 
        str_sub(., start = -6)  %>% 
        ifelse(rlang::is_empty(.),
               lastmod_tz,
               .),
      
      category = doc %>% html_elements(xpath = paste0("//a[contains(@class, 'nav__link_popular-trends') and @data-trends-link='", doc %>% html_elements("script") %>% html_text2() %>% str_extract(., "var list = \\[\\'(.*?)\\'") %>% na.omit() %>% str_sub(start = 14, end = -2), "']")) %>% html_text2() %>% ifelse(rlang::is_empty(.), NA, .),
      
      lead = doc %>% html_elements(xpath = "//meta[@property='og:description']") %>% html_attr("content"), 
      # or use lead = doc %>% html_element(".p-24 .Text-type_1") %>% html_text2()
      text_article = stringr::str_c(doc %>% html_elements(xpath = "//div[contains(@class,'article__text')]/p") %>% html_text2(), collapse = " "), # pure article text
      
      
      text_all = stringr::str_c(doc %>% html_elements(".article__text") %>% html_text2(), collapse = " ") %>%  # all text including intextlinks & embeddings
        stringr::str_remove_all(., "noscript pattern|[\n]"),
      
      cover_img_url = doc %>% html_element(".article__cover img") %>% html_attr("src") %>% 
        ifelse(rlang::is_empty(.), NA , .),
      cover_img_alt = doc %>% html_element(".article__cover img") %>% html_attr("alt") %>% 
        ifelse(rlang::is_empty(.), NA , .),
      cover_img_caption = doc %>% html_element(".article__cover-description") %>% html_text2() %>% 
        ifelse(rlang::is_empty(.), NA, .) %>% str_replace_all(pattern = "\n", replacement = " / "),
      cover_img_rights = doc %>% html_element(".article__cover-copyright") %>% html_text2() %>% 
        ifelse(rlang::is_empty(.), NA , .), 
      
      available_online = 1,
      capture_time = capture_time,
      doc_hash = doc_hash
      
    ) # end of main df
    # push to DB  
    DBI::dbWriteTable(conn = con, name = "page_data", 
                      value = page_data %>% dplyr::mutate(across(.cols = !is.character, as.character),
                                                          available_online = as.integer(1)),
                      append = TRUE
    )
    
    
    # category_list df: 
    if("try-error" %in% class(
      category_list <- try(tibble(
        doc_hash = doc_hash,
        category_text = c(doc %>% html_elements(".nav__link_header_active") %>% html_text2(), 
                          doc %>% html_elements(xpath = paste0("//a[contains(@class, 'nav__link_popular-trends') and @data-trends-link='", doc %>% html_elements("script") %>% html_text2() %>% str_extract(., "var list = \\[\\'(.*?)\\'") %>% na.omit() %>% str_sub(start = 14, end = -2), "']")) %>% html_text2()),
        category_url = c(doc %>% html_elements(".nav__link_header_active") %>% html_attr("href"), 
                         doc %>% html_elements(xpath = paste0("//a[contains(@class, 'nav__link_popular-trends') and @data-trends-link='", doc %>% html_elements("script") %>% html_text2() %>% str_extract(., "var list = \\[\\'(.*?)\\'") %>% na.omit() %>% str_sub(start = 14, end = -2), "']")) %>% html_attr("href")),
        version = version
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(category_list)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "category_list",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # alternatively: create dictionary to safe space
      # push to DB
      DBI::dbWriteTable(conn = con, name = "category_list", 
                        value = category_list %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    } # end of category-error handling
    
    
    # tag_list df: 
    if("try-error" %in% class(
      tag_list <- try(tibble(
        doc_hash = doc_hash,
        tag_name = doc %>% html_elements(".tags-trends__link") %>% html_text2(),
        tag_url = paste0(base_url, doc %>% html_elements(".tags-trends__link") %>% html_attr("href")),
        version = version
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(tag_lists)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "tag_lists",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      
      # alternatively create dictionary to safe space
      # push to DB
      DBI::dbWriteTable(conn = con, name = "tag_list", 
                        value = tag_list %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    } # end of tag-error handling
    
    
    if("try-error" %in% class(
      intext_links <- try(tibble(
        doc_hash = doc_hash,
        intext_link_url = ifelse(doc %>% html_nodes(".article__text p a") %>% html_attr("href") %>% str_starts("/"),
                                 paste0(base_url, doc %>% html_nodes(".article__text p a") %>% html_attr("href")),
                                 doc %>% html_nodes(".article__text p a") %>% html_attr("href")
        ),
        intext_link_text = doc %>% html_nodes(".article__text p a") %>% html_text2()
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(intext_links)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "intext_links",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      
      # push to DB
      DBI::dbWriteTable(conn = con, name = "intext_links", 
                        value = intext_links %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    }
    
    
    if("try-error" %in% class(
      recommendations_main_big <- try(tibble(
        doc_hash = doc_hash,
        main_recommendations_title = doc %>% html_elements(".read-more-big__title") %>% html_text2(),
        main_recommendations_link = doc %>% html_elements(".read-more-big") %>% html_attr("href") %>% ifelse(str_starts(., "/"), paste0(base_url, .), .),
        main_recommendations_img = doc %>% html_elements(".read-more-big__cover") %>% html_attr("style") %>% str_extract(., "https.*.[jpg|JPG]"),
        main_recommendations_alt = NA
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(recommendations_main_big)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "recommendations_main_big",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # push to DB
      DBI::dbWriteTable(conn = con, name = "recommendations_main", 
                        value = recommendations_main_big %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    }
    
    # text only:
    if("try-error" %in% class(
      recommendations_main <- try(tibble(
        doc_hash = doc_hash,
        main_recommendations_title = doc %>% html_elements(".read-more-text-only a") %>% html_text2(),
        main_recommendations_link = doc %>% html_elements(".read-more-text-only a") %>% html_attr("href") %>% ifelse(str_starts(., "/"), paste0(base_url, .), .),
        main_recommendations_img = NA,
        main_recommendations_alt = NA
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(recommendations_main)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "recommendations_main",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # push to DB
      DBI::dbWriteTable(conn = con, name = "recommendations_main", 
                        value = recommendations_main %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    }
    
    # main unframed text:
    if("try-error" %in% class(
      recommendations_main_text <- try(tibble(
        doc_hash = doc_hash,
        main_recommendations_title = doc %>% html_elements(".crosslinks__link") %>% html_text2(),
        main_recommendations_link = doc %>% html_elements(".crosslinks__link") %>% html_attr("href") %>% ifelse(str_starts(., "/"), paste0(base_url, .), .),
        main_recommendations_img = NA,
        main_recommendations_alt = NA
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(recommendations_main_text)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "recommendations_main_text",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # push to DB
      DBI::dbWriteTable(conn = con, name = "recommendations_main", 
                        value = recommendations_main_text %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    }
    
    if("try-error" %in% class(
      recommendations_embedded <- try(tibble(
        doc_hash = doc_hash,
        recommendation_embedded_thumbnail = doc %>% html_elements(".read-more__cover") %>%  html_attr("src"),
        recommendation_embedded_title = doc %>% html_elements(".read-more__link") %>% html_text2() %>% stringi::stri_remove_empty(),
        recommendation_embedded_link = doc %>% html_elements(".read-more__link") %>% html_attr("href") %>% stringr::str_c(base_url, .) %>% unique()
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(recommendations_embedded)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "recommendations_embedded",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # push to DB
      DBI::dbWriteTable(conn = con, name = "recommendations_embedded", 
                        value = recommendations_embedded %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    }
    
    
    
    if("try-error" %in% class(
      images <- try(tibble(
        doc_hash = doc_hash,
        image_url = doc %>% html_elements(".article__cover img") %>% html_attr("src"), # %>% ifelse(rlang::is_empty(.), NA , .),
        image_caption = doc %>% html_elements(".article__cover-description") %>% html_text2(), 
        image_source = doc %>% html_element(".article__cover-copyright") %>% html_text2() %>% str_replace_all(pattern = "\n", replacement = " / ")
      ) # %>% filter(., image_url != page_data$cover_img_url) # for some reason causes issues, so just scrape them again...
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(images)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "images",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # push to DB
      DBI::dbWriteTable(conn = con, name = "images", 
                        value = images %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    }
    
    
    # Embeddings:  
    
    not_include <- c()
    
    # rt videos:  
    if("try-error" %in% class(
      rtvideos_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = c(doc %>% html_elements(".cover_video") %>% html_attr("src"), 
                          doc %>% html_elements(".cover_video iframe") %>% html_attr("src")) %>% na.omit(),
        source = "rt_video"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(rtvideos_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "rtvideos_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "rtvideos_embeddings")
    }
    
# probably none of those:
    if("try-error" %in% class(
      youtube_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".media__youtube-frame") %>% html_attr("src"),
        source = "youtube"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(youtube_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "youtube_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "youtube_embeddings")
    }
    
    # catches different kinds of twitter embeddings (see https://francais.rt.com/france/80747-jvais-te-faire-samuel-paty-mila-nouveau-menacee-mort-video-anti-islam) 
    if("try-error" %in% class(
      twitter_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = c(doc %>% html_elements(xpath = "//*[@class = 'twitter-tweet']/*[last()]") %>% html_attr("href"),
                          doc %>% html_elements(xpath = "//*[@class = 'twitter-tweet']/*[last()]/a") %>% html_attr("href")
                        ) %>% na.omit(),
#        doc %>% html_elements(".twitter-tweet") %>% html_children() %>%  html_attr("href") %>% na.omit(),
        source = "twitter"
        # embeddings_twitter_date = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']/*/a") %>% html_text2() %>% lubridate::parse_date_time(., "%B %d, %Y") %>% as.character()
        # embeddings_twitter_text = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']") %>% html_text2()
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(twitter_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "twitter_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "twitter_embeddings")
    }
    
    
    # probably not existent:    
    if("try-error" %in% class(
      odysee_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".EmbedBlock-odysee iframe") %>% html_attr("data-src"),
        source = "odysee"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(odysee_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "odysee_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "odysee_embeddings")
    }
    
# probably none of those:    
    if("try-error" %in% class(
      telegram_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".rtcode script") %>% html_attr("data-telegram-post") %>% paste0("https://t.me/", .),
        source = "telegram"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(telegram_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "telegram_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "telegram_embeddings")
    } 

# probably none of those:    
    # Instagram:
    if("try-error" %in% class(
      instagram_embeddings1 <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".instagram-media") %>% html_attr("data-instgrm-permalink"),
        source = "instagram"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(instagram_embeddings1)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "instagram_embeddings1",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "instagram_embeddings1")
    } 

# probably none of those:    
    # Tiktok:
    if("try-error" %in% class(
      tiktok_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".tiktok-embed") %>% html_attr("cite"),
        source = "tiktok"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(tiktok_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "tiktok_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "tiktok_embeddings")
    } 
    
    
    # other (Podcast etc.):
    # probably not used:
    if("try-error" %in% class(
      vk_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements("iframe") %>% html_attr("src"),
        source = case_when(embedding_url %>% str_detect("facebook") ~ "facebook",
                           embedding_url %>% str_detect("soundcloud") ~ "soundcloud",
                           embedding_url %>% str_detect("vk.com") ~ "vk",
                           embedding_url %>% str_detect("t.me") ~ "telegram",
                           embedding_url %>% str_detect("youtube") ~ "youtube",
                           embedding_url %>% str_detect("twitter.com") ~ "twitter",
                           embedding_url %>% str_detect("podbean") ~ "podbean",
                           embedding_url %>% str_detect("odysee") ~ "odysee",
                           embedding_url %>% str_detect("vine.co") ~ "vine",
                           TRUE ~ "other")
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(telegram_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "telegram_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "telegram_embeddings")
    } 
    
    # RT Podcast:
    if("try-error" %in% class(
      rt_podcast_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".podcast-view-card") %>% html_attr("data-src"),
        source = "rt_podcast"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(rt_podcast_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "rt_podcast_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "rt_podcast_embeddings")
    }     
    
    # other podcast (e.g. soundcloud)
    if("try-error" %in% class(
      other_podcast_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".article__podcast iframe") %>% html_attr("data-src"),
        source = ifelse(doc %>% html_elements(".article__podcast iframe") %>% html_attr("data-src") %>% str_detect("soundcloud"),
                        "soundcloud",
                        "other_podcast")
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(other_podcast_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "other_podcast_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "other_podcast_embeddings")
    }     
    
    # rtcode (e.g. facebook/soundcloud)
    if("try-error" %in% class(
      rtcode_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".rtcode iframe") %>% html_attr("src"),
        source = case_when(embedding_url %>% str_detect("facebook") ~ "facebook",
                           embedding_url %>% str_detect("soundcloud") ~ "soundcloud",
                           embedding_url %>% str_detect("vk.com") ~ "vk",
                           embedding_url %>% str_detect("t.me") ~ "telegram",
                           embedding_url %>% str_detect("youtube") ~ "youtube",
                           embedding_url %>% str_detect("twitter.com") ~ "twitter",
                           embedding_url %>% str_detect("podbean") ~ "podbean",
                           embedding_url %>% str_detect("odysee") ~ "odysee",
                           TRUE ~ "other")
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(rtcode_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "rtcode_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "rtcode_embeddings")
    }     
    
    
    # other (Podcast etc.):
    if("try-error" %in% class(
      podbean_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".AllEmbed iframe") %>% html_attr("src"),
        source = case_when(embedding_url %>% str_detect("facebook") ~ "facebook",
                           embedding_url %>% str_detect("soundcloud") ~ "soundcloud",
                           embedding_url %>% str_detect("vk.com") ~ "vk",
                           embedding_url %>% str_detect("t.me") ~ "telegram",
                           embedding_url %>% str_detect("youtube") ~ "youtube",
                           embedding_url %>% str_detect("twitter.com") ~ "twitter",
                           embedding_url %>% str_detect("podbean") ~ "podbean",
                           embedding_url %>% str_detect("odysee") ~ "odysee",
                           TRUE ~ "other")
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(podbean_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "podbean_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "podbean_embeddings")
    }     
    
    
    # check which embeddings are no error:
    all_embeddings <- c("rtvideos_embeddings", "youtube_embeddings", "twitter_embeddings", "odysee_embeddings", "telegram_embeddings", "tiktok_embeddings", "vk_embeddings", "instagram_embeddings1", "rt_podcast_embeddings", "podbean_embeddings", "other_podcast_embeddings", "rtcode_embeddings")  
    valid_embeddings <- all_embeddings[!all_embeddings %in% not_include]
    paste0("dplyr::bind_rows(", 
           str_flatten(valid_embeddings, collapse = ", "),
           ")")
    
    if("try-error" %in% class(
      embeddings <- try(eval(parse(text = valid_embeddings))) 
    )) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # push to DB
      DBI::dbWriteTable(conn = con, name = "embeddings", 
                        value = embeddings %>% dplyr::mutate(across(.cols = !is.character, as.character)) %>% unique(),
                        append = TRUE
      ) 
    }
    
  } # end of error-handling
} # end of scraper_ru



# RT arabic:
# RT russian:
scraper_ru <- function(link, version, con, 
                       lastmod, lastmod_tz, logfile = log_file
                       ){
  # ~ scraper_en ~ scraper_fr

# hard code to avoid weird error:
logfile = "scrapelog_rt.txt"

  require(DBI)
  require(rvest)
  require(dplyr)
  require(stringr)
  require(anytime)
  
  # get page:
  base_url <- "https://russian.rt.com"
  
  # try and record as missing if not available
  if("try-error" %in% class(
    doc <- try(rvest::read_html(link))
  )) {
    
    catch_not_captured(link, version, con)
    cat(Sys.time() %>% toString(), "failed to save", link, "(main)", "\n", sep = " ")
    # write in logfile:
    write(x = paste(Sys.time(), link, "main",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
    
  } else{ # if page available
    
    capture_time <- Sys.time()
    html_doc <- toString(doc)
    doc_hash <- rlang::hash(html_doc)
    
    
    # get page_data:
    
    # save full html:
    # html_pages <- tibble(
    #   link = link,
    #   doc_hash = doc_hash,
    #   html_doc = html_doc
    # )
    # # push to DB 
    # DBI::dbWriteTable(conn = con, name = "html_pages", 
    #                   value = html_pages %>% dplyr::mutate(across(.cols = !is.character, as.character)),
    #                   append = TRUE
    # ) 
    
    
    # main df:
    page_data <- tibble(
      header = doc %>% html_element(xpath = "//meta[@property='og:title']") %>% html_attr("content"), 
      # or use header = doc %>% html_elements(., ".HeadLine-type_2") %>% html_text2() %>% stringr::str_c(., collapse = " ")
      link = link,
      version = version,
      
      author = doc %>% html_element(".article__author") %>% html_text2() %>% 
        ifelse(rlang::is_empty(.), NA , .),
      author_img = NA,
      
      date_time_utc = (if(!rlang::is_empty(doc %>% html_element(xpath = "//meta[@name='mediator_published_time']") %>% html_attr("content"))){doc %>% html_elements(xpath = "//meta[@name='mediator_published_time']") %>% html_attr("content")
        } else if (!rlang::is_empty(doc %>% html_element(xpath = "//meta[@name='published_time_telegram']") %>% html_attr("content"))) {
          doc %>% html_element(xpath = "//meta[@name='published_time_telegram']") %>% html_attr("content")
        } else{lastmod %>% as.character()}
        # case_when(!rlang::is_empty(doc %>% html_elements(xpath = "//meta[@name='mediator_published_time']") %>% html_attr("content")) ~ doc %>% html_elements(xpath = "//meta[@name='mediator_published_time']") %>% html_attr("content"),
        #                         !rlang::is_empty(doc %>% html_elements(xpath = "//meta[@name='published_time_telegram']") %>% html_attr("content")) ~ doc %>% html_elements(xpath = "//meta[@name='published_time_telegram']") %>% html_attr("content"),
        #                         TRUE ~ lastmod %>% as.character()
                               ) %>% lubridate::ymd_hms(., tz = "UTC") %>% as.character(),
      
      # doc %>% html_elements(xpath = "//script[@type='application/ld+json']") %>% html_text2() %>%
      #                         str_extract(., '\"datePublished\": \"(.*)\"') %>% na.omit() %>% str_sub(19, 43) %>%
      #                         lubridate::ymd_hms(., tz = "UTC"),
      #         
      # ,
      #       date_time_utc = doc %>% html_element(".Timestamp-default") %>% html_attr("datetime") %>% lubridate::ymd_hms(., tz = "UTC") %>% as.character(),
      date = as.Date(date_time_utc %>% lubridate::ymd_hms(tz = "UTC")) %>% as.character(),
      tz_original = case_when(!rlang::is_empty(doc %>% html_element(xpath = "//meta[@name='mediator_published_time']") %>% html_attr("content")) ~ doc %>% html_elements(xpath = "//meta[@name='mediator_published_time']") %>% html_attr("content"),
                              !rlang::is_empty(doc %>% html_element(xpath = "//meta[@name='published_time_telegram']") %>% html_attr("content")) ~ doc %>% html_elements(xpath = "//meta[@name='published_time_telegram']") %>% html_attr("content"),
                              TRUE ~ lastmod %>% as.character()) %>% 
        str_sub(., start = -6)  %>% 
        ifelse(rlang::is_empty(.),
               lastmod_tz,
               .),
      
      category = doc %>% html_element(xpath = paste0("//a[contains(@class, 'nav__link_popular-trends') and @data-trends-link='", doc %>% html_element("script") %>% html_text2() %>% str_extract(., "var list = \\[\\'(.*?)\\'") %>% na.omit() %>% str_sub(start = 14, end = -2), "']")) %>% html_text2() %>% 
        ifelse(rlang::is_empty(.), 
               NA,
               .),
                        
      
      lead = doc %>% html_element(xpath = "//meta[@property='og:description']") %>% html_attr("content"), 
      # or use lead = doc %>% html_element(".p-24 .Text-type_1") %>% html_text2()
      text_article = stringr::str_c(doc %>% html_elements(xpath = "//div[contains(@class,'article__text')]/p") %>% html_text2(), collapse = " "), # pure article text
      
      
      text_all = stringr::str_c(doc %>% html_elements(".article__text") %>% html_text2(), collapse = " ") %>%  # all text including intextlinks & embeddings
        stringr::str_remove_all(., "noscript pattern|[\n]"),
      
      cover_img_url = doc %>% html_element(".article__cover img") %>% html_attr("src") %>% 
        ifelse(rlang::is_empty(.), NA , .),
      cover_img_alt = doc %>% html_element(".article__cover img") %>% html_attr("alt") %>% 
        ifelse(rlang::is_empty(.), NA , .),
      cover_img_caption = doc %>% html_element(".article__cover-description") %>% html_text2() %>% 
        ifelse(rlang::is_empty(.), NA, .) %>% str_replace_all(pattern = "\n", replacement = " / "),
      cover_img_rights = doc %>% html_element(".article__cover-copyright") %>% html_text2() %>% 
        ifelse(rlang::is_empty(.), NA , .), 
      
      available_online = 1,
      capture_time = capture_time,
      doc_hash = doc_hash

    ) # end of main df
    # push to DB  
    DBI::dbWriteTable(conn = con, name = "page_data", 
                      value = page_data %>% dplyr::mutate(across(.cols = !is.character, as.character),
                                                          available_online = as.integer(1)),
                      append = TRUE
    )
    
    
    # category_list df: 
    if("try-error" %in% class(
      category_list <- try(tibble(
        doc_hash = doc_hash,
        category_text = c(doc %>% html_elements(".nav__link_header_active") %>% html_text2(), 
                          doc %>% html_elements(xpath = paste0("//a[contains(@class, 'nav__link_popular-trends') and @data-trends-link='", doc %>% html_elements("script") %>% html_text2() %>% str_extract(., "var list = \\[\\'(.*?)\\'") %>% na.omit() %>% str_sub(start = 14, end = -2), "']")) %>% html_text2()),
        category_url = c(doc %>% html_elements(".nav__link_header_active") %>% html_attr("href"), 
                         doc %>% html_elements(xpath = paste0("//a[contains(@class, 'nav__link_popular-trends') and @data-trends-link='", doc %>% html_elements("script") %>% html_text2() %>% str_extract(., "var list = \\[\\'(.*?)\\'") %>% na.omit() %>% str_sub(start = 14, end = -2), "']")) %>% html_attr("href")),
        version = version
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(category_list)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "category_list",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # alternatively: create dictionary to safe space
      # push to DB
      DBI::dbWriteTable(conn = con, name = "category_list", 
                        value = category_list %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    } # end of category-error handling
    
    
    # tag_list df: 
    if("try-error" %in% class(
      tag_list <- try(tibble(
        doc_hash = doc_hash,
        tag_name = doc %>% html_elements(".tags-trends__link") %>% html_text2(),
        tag_url = paste0(base_url, doc %>% html_elements(".tags-trends__link") %>% html_attr("href")),
        version = version
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(tag_lists)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "tag_lists",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      
      # alternatively create dictionary to safe space
      # push to DB
      DBI::dbWriteTable(conn = con, name = "tag_list", 
                        value = tag_list %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    } # end of tag-error handling
    
    
    if("try-error" %in% class(
      intext_links <- try(tibble(
        doc_hash = doc_hash,
        intext_link_url = ifelse(doc %>% html_nodes(".article__text p a") %>% html_attr("href") %>% str_starts("/"),
                                 paste0(base_url, doc %>% html_nodes(".article__text p a") %>% html_attr("href")),
                                 doc %>% html_nodes(".article__text p a") %>% html_attr("href")
        ),
        intext_link_text = doc %>% html_nodes(".article__text p a") %>% html_text2()
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(intext_links)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "intext_links",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      
      # push to DB
      DBI::dbWriteTable(conn = con, name = "intext_links", 
                        value = intext_links %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    }
    
    
    if("try-error" %in% class(
      recommendations_main_big <- try(tibble(
        doc_hash = doc_hash,
        main_recommendations_title = doc %>% html_elements(".read-more-big__title") %>% html_text2(),
        main_recommendations_link = doc %>% html_elements(".read-more-big") %>% html_attr("href") %>% ifelse(str_starts(., "/"), paste0(base_url, .), .),
        main_recommendations_img = doc %>% html_elements(".read-more-big__cover") %>% html_attr("style") %>% str_extract(., "https.*.[jpg|JPG]"),
        main_recommendations_alt = NA
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(recommendations_main_big)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "recommendations_main_big",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # push to DB
      DBI::dbWriteTable(conn = con, name = "recommendations_main", 
                        value = recommendations_main_big %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    }
    
    # text only:
    if("try-error" %in% class(
      recommendations_main <- try(tibble(
        doc_hash = doc_hash,
        main_recommendations_title = doc %>% html_elements(".read-more-text-only a") %>% html_text2(),
        main_recommendations_link = doc %>% html_elements(".read-more-text-only a") %>% html_attr("href") %>% ifelse(str_starts(., "/"), paste0(base_url, .), .),
        main_recommendations_img = NA,
        main_recommendations_alt = NA
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(recommendations_main)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "recommendations_main",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # push to DB
      DBI::dbWriteTable(conn = con, name = "recommendations_main", 
                        value = recommendations_main %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    }
    
    # main unframed text:
    if("try-error" %in% class(
      recommendations_main_text <- try(tibble(
        doc_hash = doc_hash,
        main_recommendations_title = doc %>% html_elements(".crosslinks__link") %>% html_text2(),
        main_recommendations_link = doc %>% html_elements(".crosslinks__link") %>% html_attr("href") %>% ifelse(str_starts(., "/"), paste0(base_url, .), .),
        main_recommendations_img = NA,
        main_recommendations_alt = NA
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(recommendations_main_text)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "recommendations_main_text",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # push to DB
      DBI::dbWriteTable(conn = con, name = "recommendations_main", 
                        value = recommendations_main_text %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    }
    
    if("try-error" %in% class(
      recommendations_embedded <- try(tibble(
        doc_hash = doc_hash,
        recommendation_embedded_thumbnail = doc %>% html_elements(".read-more__cover") %>%  html_attr("src"),
        recommendation_embedded_title = doc %>% html_elements(".read-more__link") %>% html_text2() %>% stringi::stri_remove_empty(),
        recommendation_embedded_link = doc %>% html_elements(".read-more__link") %>% html_attr("href") %>% stringr::str_c(base_url, .) %>% unique()
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(recommendations_embedded)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "recommendations_embedded",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # push to DB
      DBI::dbWriteTable(conn = con, name = "recommendations_embedded", 
                        value = recommendations_embedded %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    }
    
    
    
    if("try-error" %in% class(
      images <- try(tibble(
        doc_hash = doc_hash,
        image_url = doc %>% html_elements(".article__cover img") %>% html_attr("src"), # %>% ifelse(rlang::is_empty(.), NA , .),
        image_caption = doc %>% html_elements(".article__cover-description") %>% html_text2(), 
        image_source = doc %>% html_element(".article__cover-copyright") %>% html_text2() %>% str_replace_all(pattern = "\n", replacement = " / ")
      ) # %>% filter(., image_url != page_data$cover_img_url) # for some reason causes issues, so just scrape them again...
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(images)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "images",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # push to DB
      DBI::dbWriteTable(conn = con, name = "images", 
                        value = images %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    }
    
    
    # Embeddings:  
    
    not_include <- c()
    
    # rt videos:  
    if("try-error" %in% class(
      rtvideos_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = c(doc %>% html_elements(".cover_video") %>% html_attr("src"), 
                          doc %>% html_elements(".cover_video iframe") %>% html_attr("src")) %>% na.omit(),
        source = "rt_video"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(rtvideos_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "rtvideos_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "rtvideos_embeddings")
    }
    
# probably none of those:
    if("try-error" %in% class(
      youtube_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".media__youtube-frame") %>% html_attr("src"),
        source = "youtube"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(youtube_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "youtube_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "youtube_embeddings")
    }
    
    # catches different kinds of twitter embeddings (see https://francais.rt.com/france/80747-jvais-te-faire-samuel-paty-mila-nouveau-menacee-mort-video-anti-islam) 
    if("try-error" %in% class(
      twitter_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = c(doc %>% html_elements(xpath = "//*[@class = 'twitter-tweet']/*[last()]") %>% html_attr("href"),
                          doc %>% html_elements(xpath = "//*[@class = 'twitter-tweet']/*[last()]/a") %>% html_attr("href")
                        ) %>% na.omit(),
#        doc %>% html_elements(".twitter-tweet") %>% html_children() %>%  html_attr("href") %>% na.omit(),
        source = "twitter"
        # embeddings_twitter_date = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']/*/a") %>% html_text2() %>% lubridate::parse_date_time(., "%B %d, %Y") %>% as.character()
        # embeddings_twitter_text = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']") %>% html_text2()
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(twitter_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "twitter_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "twitter_embeddings")
    }
    
    
    # probably not existent:    
    if("try-error" %in% class(
      odysee_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".EmbedBlock-odysee iframe") %>% html_attr("data-src"),
        source = "odysee"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(odysee_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "odysee_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "odysee_embeddings")
    }
    
# probably none of those:    
    if("try-error" %in% class(
      telegram_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".rtcode script") %>% html_attr("data-telegram-post") %>% paste0("https://t.me/", .),
        source = "telegram"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(telegram_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "telegram_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "telegram_embeddings")
    } 

# probably none of those:    
    # Instagram:
    if("try-error" %in% class(
      instagram_embeddings1 <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".instagram-media") %>% html_attr("data-instgrm-permalink"),
        source = "instagram"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(instagram_embeddings1)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "instagram_embeddings1",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "instagram_embeddings1")
    } 

# probably none of those:    
    # Tiktok:
    if("try-error" %in% class(
      tiktok_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".tiktok-embed") %>% html_attr("cite"),
        source = "tiktok"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(tiktok_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "tiktok_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "tiktok_embeddings")
    } 
    
    
    # other (Podcast etc.):
    # probably not used:
    if("try-error" %in% class(
      vk_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements("iframe") %>% html_attr("src"),
        source = case_when(embedding_url %>% str_detect("facebook") ~ "facebook",
                           embedding_url %>% str_detect("soundcloud") ~ "soundcloud",
                           embedding_url %>% str_detect("vk.com") ~ "vk",
                           embedding_url %>% str_detect("t.me") ~ "telegram",
                           embedding_url %>% str_detect("youtube") ~ "youtube",
                           embedding_url %>% str_detect("twitter.com") ~ "twitter",
                           embedding_url %>% str_detect("podbean") ~ "podbean",
                           embedding_url %>% str_detect("odysee") ~ "odysee",
                           embedding_url %>% str_detect("vine.co") ~ "vine",
                           TRUE ~ "other")
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(telegram_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "telegram_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "telegram_embeddings")
    } 
    
    # RT Podcast:
    if("try-error" %in% class(
      rt_podcast_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".podcast-view-card") %>% html_attr("data-src"),
        source = "rt_podcast"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(rt_podcast_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "rt_podcast_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "rt_podcast_embeddings")
    }     
    
    # other podcast (e.g. soundcloud)
    if("try-error" %in% class(
      other_podcast_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".article__podcast iframe") %>% html_attr("data-src"),
        source = ifelse(doc %>% html_elements(".article__podcast iframe") %>% html_attr("data-src") %>% str_detect("soundcloud"),
                        "soundcloud",
                        "other_podcast")
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(other_podcast_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "other_podcast_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "other_podcast_embeddings")
    }     
    
    # rtcode (e.g. facebook/soundcloud)
    if("try-error" %in% class(
      rtcode_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".rtcode iframe") %>% html_attr("src"),
        source = case_when(embedding_url %>% str_detect("facebook") ~ "facebook",
                           embedding_url %>% str_detect("soundcloud") ~ "soundcloud",
                           embedding_url %>% str_detect("vk.com") ~ "vk",
                           embedding_url %>% str_detect("t.me") ~ "telegram",
                           embedding_url %>% str_detect("youtube") ~ "youtube",
                           embedding_url %>% str_detect("twitter.com") ~ "twitter",
                           embedding_url %>% str_detect("podbean") ~ "podbean",
                           embedding_url %>% str_detect("odysee") ~ "odysee",
                           TRUE ~ "other")
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(rtcode_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "rtcode_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "rtcode_embeddings")
    }     
    
    
    # other (Podcast etc.):
    if("try-error" %in% class(
      podbean_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".AllEmbed iframe") %>% html_attr("src"),
        source = case_when(embedding_url %>% str_detect("facebook") ~ "facebook",
                           embedding_url %>% str_detect("soundcloud") ~ "soundcloud",
                           embedding_url %>% str_detect("vk.com") ~ "vk",
                           embedding_url %>% str_detect("t.me") ~ "telegram",
                           embedding_url %>% str_detect("youtube") ~ "youtube",
                           embedding_url %>% str_detect("twitter.com") ~ "twitter",
                           embedding_url %>% str_detect("podbean") ~ "podbean",
                           embedding_url %>% str_detect("odysee") ~ "odysee",
                           TRUE ~ "other")
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(podbean_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "podbean_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "podbean_embeddings")
    }     
    
    
    # check which embeddings are no error:
    all_embeddings <- c("rtvideos_embeddings", "youtube_embeddings", "twitter_embeddings", "odysee_embeddings", "telegram_embeddings", "tiktok_embeddings", "vk_embeddings", "instagram_embeddings1", "rt_podcast_embeddings", "podbean_embeddings", "other_podcast_embeddings", "rtcode_embeddings")  
    valid_embeddings <- all_embeddings[!all_embeddings %in% not_include]
    paste0("dplyr::bind_rows(", 
           str_flatten(valid_embeddings, collapse = ", "),
           ")")
    
    if("try-error" %in% class(
      embeddings <- try(eval(parse(text = valid_embeddings))) 
    )) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # push to DB
      DBI::dbWriteTable(conn = con, name = "embeddings", 
                        value = embeddings %>% dplyr::mutate(across(.cols = !is.character, as.character)) %>% unique(),
                        append = TRUE
      ) 
    }
    
  } # end of error-handling
} # end of scraper_ru


# RT arabic:
scraper_ar <- function(link, version, con, logfile = log_file,
                       lastmod, lastmod_tz
){
  
# hard code file to avoid weird error:
logfile = "scrapelog_rt.txt"

  require(DBI)
  require(rvest)
  require(dplyr)
  require(stringr)
  require(anytime)
  
  # get page:
  base_url <- "https://arabic.rt.com"
  
  # try and record as missing if not available
  if("try-error" %in% class(
    doc <- try(rvest::read_html(link))
  )) {
    
    catch_not_captured(link, version, con)
    cat(Sys.time() %>% toString(), "failed to save", link, "(main)", "\n", sep = " ")
    # write in logfile:
    write(x = paste(Sys.time(), link, "main",  sep = ", "), 
          file = logfile, append = T, sep = "\n")
    
  } else{ # if page available
    
    capture_time <- Sys.time()
    html_doc <- toString(doc)
    doc_hash <- rlang::hash(html_doc)
    
    
    # get page_data:
    
    # save full html:
    # html_pages <- tibble(
    #   link = link,
    #   doc_hash = doc_hash,
    #   html_doc = html_doc
    # )
    # # push to DB 
    # DBI::dbWriteTable(conn = con, name = "html_pages", 
    #                   value = html_pages %>% dplyr::mutate(across(.cols = !is.character, as.character)),
    #                   append = TRUE
    # ) 
    
    
    # main df:
    page_data <- tibble(
      header = doc %>% html_element(xpath = "//meta[@property='og:title']") %>% html_attr("content"), 
      # or use header = doc %>% html_elements(., ".HeadLine-type_2") %>% html_text2() %>% stringr::str_c(., collapse = " ")
      link = link,
      version = version,
      
      author = doc %>% html_element(xpath = "//meta[@name='mediator_author']") %>% html_attr("content") %>% 
        ifelse(rlang::is_empty(.), NA , .),
      author_img = NA,
      
      date_time_utc = (if(!rlang::is_empty(doc %>% html_element(xpath = "//meta[@name='publish-date']") %>% html_attr("content"))){doc %>% html_elements(xpath = "//meta[@name='publish-date']") %>% html_attr("content") %>% anytime()
      } else if (!rlang::is_empty(doc %>% html_element(xpath = "//meta[@name='published_time_telegram']") %>% html_attr("content"))) {
        doc %>% html_element(xpath = "//meta[@name='published_time_telegram']") %>% html_attr("content")
      } else{lastmod %>% as.character()}
      # case_when(!rlang::is_empty(doc %>% html_elements(xpath = "//meta[@name='mediator_published_time']") %>% html_attr("content")) ~ doc %>% html_elements(xpath = "//meta[@name='mediator_published_time']") %>% html_attr("content"),
      #                         !rlang::is_empty(doc %>% html_elements(xpath = "//meta[@name='published_time_telegram']") %>% html_attr("content")) ~ doc %>% html_elements(xpath = "//meta[@name='published_time_telegram']") %>% html_attr("content"),
      #                         TRUE ~ lastmod %>% as.character()
      ) %>% lubridate::ymd_hms(., tz = "UTC") %>% as.character(),
      
      # doc %>% html_elements(xpath = "//script[@type='application/ld+json']") %>% html_text2() %>%
      #                         str_extract(., '\"datePublished\": \"(.*)\"') %>% na.omit() %>% str_sub(19, 43) %>%
      #                         lubridate::ymd_hms(., tz = "UTC"),
      #         
      # ,
      #       date_time_utc = doc %>% html_element(".Timestamp-default") %>% html_attr("datetime") %>% lubridate::ymd_hms(., tz = "UTC") %>% as.character(),
      date = as.Date(date_time_utc %>% lubridate::ymd_hms(tz = "UTC")) %>% as.character(),
      tz_original = case_when(!rlang::is_empty(doc %>% html_element(xpath = "//meta[@name='publish-date']") %>% html_attr("content")) ~ doc %>% html_element(xpath = "//meta[@name='published-date']") %>% html_attr("content"),
                              !rlang::is_empty(doc %>% html_element(xpath = "//meta[@name='published_time_telegram']") %>% html_attr("content")) ~ doc %>% html_element(xpath = "//meta[@name='published_time_telegram']") %>% html_attr("content"),
                              TRUE ~ lastmod %>% as.character()) %>% 
        str_sub(., start = -5) %>% gsub(paste0("^(.{", 3, "})(.*)$"), paste0("\\1", ":", "\\2"), .) %>% 
        ifelse(rlang::is_empty(.),
               lastmod_tz,
               .),
      
      category = doc %>% html_element(xpath = "//meta[@name='category']") %>% html_attr("content") %>% 
        ifelse(stringi::stri_isempty(.), doc %>% html_element(".time-public a") %>% html_attr("href") %>% stringr::str_remove_all(., "/"), .),
      
      lead = doc %>% html_element(xpath = "//meta[@name='description']") %>% html_attr("content"), 
#      # or use lead = doc %>% html_element(".p-24 .Text-type_1") %>% html_text2()
      text_article = stringr::str_c(doc %>% html_elements(xpath = "//div[contains(@class,'js-mediator-article')]/p[not(@class='intro')]") %>% html_text2(), collapse = " "), # pure article text
      
      
      text_all = stringr::str_c(doc %>% html_elements(xpath = "//div[contains(@class,'js-mediator-article')]//p[not(@class='intro')]") %>% html_text2(), collapse = " ") %>%  # all text including intextlinks & embeddings
        stringr::str_remove_all(., "noscript pattern|[\n]"),
      
      cover_img_url = doc %>% html_element(".photo img") %>% html_attr("data-src") %>% 
        ifelse(rlang::is_empty(.), NA , .),
      cover_img_alt = doc %>% html_element(".photo img") %>% html_attr("alt") %>% 
        ifelse(rlang::is_empty(.), NA , .),
      cover_img_caption = doc %>% html_element(".photo .caption") %>% html_text2() %>% 
        ifelse(rlang::is_empty(.), NA, .),
      cover_img_rights = doc %>% html_element(".photo .copyright") %>% html_text2() %>% 
        ifelse(rlang::is_empty(.), NA , .), 
      
      available_online = 1,
      capture_time = capture_time,
      doc_hash = doc_hash
      
    ) # end of main df
    # push to DB  
    DBI::dbWriteTable(conn = con, name = "page_data", 
                      value = page_data %>% dplyr::mutate(across(.cols = !is.character, as.character),
                                                          available_online = as.integer(1)),
                      append = TRUE
    )
    
    
    # category_list df: 
    if("try-error" %in% class(
      category_list <- try(tibble(
        doc_hash = doc_hash,
        category_text = doc %>% html_element(".time-public a") %>% html_text2(),
        category_url = paste0(base_url, doc %>% html_element(".time-public a") %>% html_attr("href")),
        version = version
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(category_list)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "category_list",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # alternatively: create dictionary to safe space
      # push to DB
      DBI::dbWriteTable(conn = con, name = "category_list", 
                        value = category_list %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    } # end of category-error handling
    
    
    # tag_list df: 
    if("try-error" %in% class(
      tag_list <- try(tibble(
        doc_hash = doc_hash,
        tag_name = doc %>% html_elements(".news-tags a") %>% html_text2(),
        tag_url = paste0(base_url, doc %>% html_elements(".news-tags a") %>% html_attr("href")),
        version = version
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(tag_lists)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "tag_lists",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      
      # alternatively create dictionary to safe space
      # push to DB
      DBI::dbWriteTable(conn = con, name = "tag_list", 
                        value = tag_list %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    } # end of tag-error handling
    
  # includes links inside iframes etc.
    if("try-error" %in% class(
      intext_links <- try(tibble(
        doc_hash = doc_hash,
        intext_link_url = ifelse(doc %>% html_nodes(".js-mediator-article p a") %>% html_attr("href") %>% str_starts("/"),
                                 paste0(base_url, doc %>% html_nodes(".js-mediator-article p a") %>% html_attr("href")),
                                 doc %>% html_nodes(".js-mediator-article p a") %>% html_attr("href")
        ),
        intext_link_text = doc %>% html_nodes(".js-mediator-article p a") %>% html_text2()
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(intext_links)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "intext_links",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      
      # push to DB
      DBI::dbWriteTable(conn = con, name = "intext_links", 
                        value = intext_links %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    }
    
    
    if("try-error" %in% class(
      recommendations_main_big <- try(tibble(
        doc_hash = doc_hash,
        main_recommendations_title = doc %>% html_elements(".read-more-big__title") %>% html_text2(),
        main_recommendations_link = doc %>% html_elements(".read-more-big") %>% html_attr("href") %>% ifelse(str_starts(., "/"), paste0(base_url, .), .),
        main_recommendations_img = doc %>% html_elements(".read-more-big__cover") %>% html_attr("style") %>% str_extract(., "https.*.[jpg|JPG]"),
        main_recommendations_alt = NA
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(recommendations_main_big)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "recommendations_main_big",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # push to DB
      DBI::dbWriteTable(conn = con, name = "recommendations_main", 
                        value = recommendations_main_big %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    }
    
    # text only:
    if("try-error" %in% class(
      recommendations_main <- try(tibble(
        doc_hash = doc_hash,
        main_recommendations_title = doc %>% html_elements(".read-more-text-only a") %>% html_text2(),
        main_recommendations_link = doc %>% html_elements(".read-more-text-only a") %>% html_attr("href") %>% ifelse(str_starts(., "/"), paste0(base_url, .), .),
        main_recommendations_img = NA,
        main_recommendations_alt = NA
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(recommendations_main)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "recommendations_main",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # push to DB
      DBI::dbWriteTable(conn = con, name = "recommendations_main", 
                        value = recommendations_main %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    }
    
    # main unframed text:
    if("try-error" %in% class(
      recommendations_main_text <- try(tibble(
        doc_hash = doc_hash,
        main_recommendations_title = doc %>% html_elements(".crosslinks__link") %>% html_text2(),
        main_recommendations_link = doc %>% html_elements(".crosslinks__link") %>% html_attr("href") %>% ifelse(str_starts(., "/"), paste0(base_url, .), .),
        main_recommendations_img = NA,
        main_recommendations_alt = NA
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(recommendations_main_text)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "recommendations_main_text",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # push to DB
      DBI::dbWriteTable(conn = con, name = "recommendations_main", 
                        value = recommendations_main_text %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    }
    
    if("try-error" %in% class(
      recommendations_embedded <- try(tibble(
        doc_hash = doc_hash,
        recommendation_embedded_thumbnail = doc %>% html_elements(".read-more__cover") %>%  html_attr("data-src"),
        recommendation_embedded_title = doc %>% html_elements(".read-more__footer") %>% html_text2(),
        recommendation_embedded_link = doc %>% html_elements(".read-more__link") %>% html_attr("href") %>% stringr::str_c(base_url, .)
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(recommendations_embedded)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "recommendations_embedded",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # push to DB
      DBI::dbWriteTable(conn = con, name = "recommendations_embedded", 
                        value = recommendations_embedded %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    }
    
    
# ToDo: length issue @ caption/copyright possible !!  -> xpath sth like noscript/following-sibling::div[@class='copyright']
  # needs test cases!
    if("try-error" %in% class(
      images <- try(tibble(
        doc_hash = doc_hash,
        image_url = doc %>% html_elements(".js-mediator-article .photo noscript img") %>% html_attr("src"), # %>% ifelse(rlang::is_empty(.), NA , .),
        image_caption = doc %>% html_elements(xpath = "//div[contains(@class, 'js-mediator-article')]//div[contains(@class, 'photo')]//noscript//following-sibling::span[@class='caption'] | //div[contains(@class, 'js-mediator-article')]//div[@class='photo'][not(descendant::span[@class='caption'])]//noscript") %>% html_text2() %>% stringr::str_remove("noscript pattern"), 
        image_source = doc %>% html_elements(xpath = "//div[contains(@class, 'js-mediator-article')]//div[contains(@class, 'photo')]//noscript//following-sibling::div[@class='copyright'] | //div[contains(@class, 'js-mediator-article')]//div[@class='photo'][not(descendant::div[@class='copyright'])]//noscript") %>% html_text2() %>% stringr::str_remove("noscript pattern")
        ) 
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(images)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "images",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # push to DB
      DBI::dbWriteTable(conn = con, name = "images", 
                        value = images %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                        append = TRUE
      ) 
    }
     
    
  # Embeddings:  
    
    not_include <- c()
    
    # rt videos:  
    if("try-error" %in% class(
      rtvideos_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = c(doc %>% html_elements(".video-iframe iframe") %>% html_attr("src"),
                          doc %>% html_elements(".cover_video") %>% html_attr("src"), 
                          doc %>% html_elements(".cover_video iframe") %>% html_attr("src")) %>% na.omit(),
        source = case_when(embedding_url %>% str_detect("facebook") ~ "facebook",
                           embedding_url %>% str_detect("soundcloud") ~ "soundcloud",
                           embedding_url %>% str_detect("vk.com") ~ "vk",
                           embedding_url %>% str_detect("t.me") ~ "telegram",
                           embedding_url %>% str_detect("youtube") ~ "youtube",
                           embedding_url %>% str_detect("twitter.com") ~ "twitter",
                           embedding_url %>% str_detect("podbean") ~ "podbean",
                           embedding_url %>% str_detect("odysee") ~ "odysee",
                           embedding_url %>% str_detect("vine.co") ~ "vine",
                           TRUE ~ "rt_video")
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(rtvideos_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "rtvideos_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "rtvideos_embeddings")
    }
    
    # probably none of those:
    if("try-error" %in% class(
      youtube_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".media__youtube-frame") %>% html_attr("src"),
        source = "youtube"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(youtube_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "youtube_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "youtube_embeddings")
    }
    
    # catches different kinds of twitter embeddings (see https://francais.rt.com/france/80747-jvais-te-faire-samuel-paty-mila-nouveau-menacee-mort-video-anti-islam) 
    if("try-error" %in% class(
      twitter_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = c(doc %>% html_elements(xpath = "//*[@class = 'twitter-tweet']/*[last()]") %>% html_attr("href"),
                          doc %>% html_elements(xpath = "//*[@class = 'twitter-tweet']/*[last()]/a") %>% html_attr("href")
        ) %>% na.omit(),
        source = "twitter"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(twitter_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "twitter_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "twitter_embeddings")
    }
    
    
    # probably not existent:    
    if("try-error" %in% class(
      odysee_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".EmbedBlock-odysee iframe") %>% html_attr("data-src"),
        source = "odysee"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(odysee_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "odysee_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "odysee_embeddings")
    }
    
    # probably none of those:    
    if("try-error" %in% class(
      telegram_embeddings <- try(tibble(
        doc_hash = doc_hash,
# use this in ther versions, too:
        embedding_url = doc %>% html_elements(xpath = "//div[@class='rtcode']/script[contains(@src, 'telegram')]") %>% html_attr("data-telegram-post") %>% paste0("https://t.me/", .),
        source = "telegram"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(telegram_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "telegram_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "telegram_embeddings")
    } 
    
    # probably none of those:    
    # Instagram:
    if("try-error" %in% class(
      instagram_embeddings1 <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".instagram-media") %>% html_attr("data-instgrm-permalink"), # type @me or @rt ?!?
        source = "instagram"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(instagram_embeddings1)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "instagram_embeddings1",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "instagram_embeddings1")
    } 
    
    # probably none of those:    
    # Tiktok:
    if("try-error" %in% class(
      tiktok_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".tiktok-embed") %>% html_attr("cite"),
        source = "tiktok"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(tiktok_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "tiktok_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "tiktok_embeddings")
    } 
    
    
    # other (Podcast etc.):
    # probably not used:
    if("try-error" %in% class(
      vk_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements("iframe") %>% html_attr("src"),
        source = case_when(embedding_url %>% str_detect("facebook") ~ "facebook",
                           embedding_url %>% str_detect("soundcloud") ~ "soundcloud",
                           embedding_url %>% str_detect("vk.com") ~ "vk",
                           embedding_url %>% str_detect("t.me") ~ "telegram",
                           embedding_url %>% str_detect("youtube") ~ "youtube",
                           embedding_url %>% str_detect("twitter.com") ~ "twitter",
                           embedding_url %>% str_detect("podbean") ~ "podbean",
                           embedding_url %>% str_detect("odysee") ~ "odysee",
                           embedding_url %>% str_detect("vine.co") ~ "vine",
                           TRUE ~ "other")
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(telegram_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "telegram_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "telegram_embeddings")
    } 
    
    # RT Podcast:
    if("try-error" %in% class(
      rt_podcast_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".podcast-view-card") %>% html_attr("data-src"),
        source = "rt_podcast"
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(rt_podcast_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "rt_podcast_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "rt_podcast_embeddings")
    }     
    
    # other podcast (e.g. soundcloud)
    if("try-error" %in% class(
      other_podcast_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".article__podcast iframe") %>% html_attr("data-src"),
        source = ifelse(doc %>% html_elements(".article__podcast iframe") %>% html_attr("data-src") %>% str_detect("soundcloud"),
                        "soundcloud",
                        "other_podcast")
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(other_podcast_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "other_podcast_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "other_podcast_embeddings")
    }     
    
    # rtcode (e.g. facebook/soundcloud)
    if("try-error" %in% class(
      rtcode_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".rtcode iframe") %>% html_attr("src"),
        source = case_when(embedding_url %>% str_detect("facebook") ~ "facebook",
                           embedding_url %>% str_detect("soundcloud") ~ "soundcloud",
                           embedding_url %>% str_detect("vk.com") ~ "vk",
                           embedding_url %>% str_detect("t.me") ~ "telegram",
                           embedding_url %>% str_detect("youtube") ~ "youtube",
                           embedding_url %>% str_detect("twitter.com") ~ "twitter",
                           embedding_url %>% str_detect("podbean") ~ "podbean",
                           embedding_url %>% str_detect("odysee") ~ "odysee",
                           TRUE ~ "other")
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(rtcode_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "rtcode_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "rtcode_embeddings")
    }     
    
    
    # other (Podcast etc.):
    if("try-error" %in% class(
      podbean_embeddings <- try(tibble(
        doc_hash = doc_hash,
        embedding_url = doc %>% html_elements(".AllEmbed iframe") %>% html_attr("src"),
        source = case_when(embedding_url %>% str_detect("facebook") ~ "facebook",
                           embedding_url %>% str_detect("soundcloud") ~ "soundcloud",
                           embedding_url %>% str_detect("vk.com") ~ "vk",
                           embedding_url %>% str_detect("t.me") ~ "telegram",
                           embedding_url %>% str_detect("youtube") ~ "youtube",
                           embedding_url %>% str_detect("twitter.com") ~ "twitter",
                           embedding_url %>% str_detect("podbean") ~ "podbean",
                           embedding_url %>% str_detect("odysee") ~ "odysee",
                           TRUE ~ "other")
      )
      ))) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(podbean_embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "podbean_embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
      not_include <- append(not_include, "podbean_embeddings")
    }     
    
    
    # check which embeddings are no error:
    all_embeddings <- c("rtvideos_embeddings", "youtube_embeddings", "twitter_embeddings", "odysee_embeddings", "telegram_embeddings", "tiktok_embeddings", "vk_embeddings", "instagram_embeddings1", "rt_podcast_embeddings", "podbean_embeddings", "other_podcast_embeddings", "rtcode_embeddings")  
    valid_embeddings <- all_embeddings[!all_embeddings %in% not_include]
    paste0("dplyr::bind_rows(", 
           str_flatten(valid_embeddings, collapse = ", "),
           ")")
    
    if("try-error" %in% class(
      embeddings <- try(eval(parse(text = valid_embeddings))) 
    )) {
      cat(Sys.time() %>% toString(), "failed to save", link, "(embeddings)", "\n", sep = " ")
      write(x = paste(Sys.time(), link, "embeddings",  sep = ", "), 
            file = logfile, append = T, sep = "\n")
    } else{
      # push to DB
      DBI::dbWriteTable(conn = con, name = "embeddings", 
                        value = embeddings %>% dplyr::mutate(across(.cols = !is.character, as.character)) %>% unique(),
                        append = TRUE
      ) 
    }
    
  } # end of error-handling
} # end of scraper_ar


# error handling ####

catch_not_captured <- function(link, version, con){
 
  require(DBI)
  require(dplyr)
  
  # main df:
  page_data <- tibble(
    header = NA, 
    link = link,
    version = version,
    author = NA,
    author_img = NA,
    date_time_utc = NA,
    date = NA,
    tz_original = NA,
    category = NA,
    lead = NA, 
    text_article = NA,
    text_all = NA,
    cover_img_url = NA,
    cover_img_alt = NA,
    cover_img_caption = NA,
    cover_img_rights = NA,
    available_online = 0,
    capture_time = Sys.time(),
    doc_hash = NA
  ) # end of main df
  
  # push to DB 
  DBI::dbWriteTable(conn = con, name = "page_data", 
                    value = page_data %>% dplyr::mutate(across(.cols = !is.character, as.character),
                                                        available_online = as.integer(0)),
                    append = TRUE
  ) 
}
