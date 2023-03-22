# Article Scraper: 

# initialize tbls:
initialize_page_tbls <- function(con = RT_DB){

# create empty dfs to push: 
  # save full html:
  html_pages <- tibble(
    link = character(0),
    doc_hash = character(0),
    html_doc = character(0)
  )
  # push to DB 
  dplyr::copy_to(dest = con,
                  df = html_pages %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                  name = "html_pages", 
                  temporary = F, 
                  types = c(link = "TEXT", doc_hash = "TEXT", html_doc = "TEXT"),
                  indexes = list("link", "doc_hash"), 
                  overwrite = F
  )
  
  # main df:
  page_data <- tibble(
    header = character(0), 
    link = character(0),
    version = character(0),
    author = character(0),    
    author_img = character(0),
    date_time_utc = character(0),
    date = character(0),
    tz_original = character(0),
    category = character(0),
    lead = character(0), 
    text_article = character(0),
    text_all = character(0),
    cover_img_url = character(0),
    cover_img_alt = character(0),
    cover_img_caption = character(0),
    cover_img_rights = character(0),
    available_online = logical(0),
    capture_time = character(0),
    doc_hash = character(0)
  )
  # push to DB  
  dplyr::copy_to(dest = con,
                  df = page_data %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                  name = "page_data", 
                  temporary = F, 
                  types = c(
                    header = "TEXT", 
                    link = "TEXT",
                    version = "TEXT",
                    author = "TEXT",    
                    author_img = "TEXT",
                    date_time_utc = "TIMESTAMP",
                    date = "DATE",
                    tz_original = "TEXT",
                    category = "TEXT",
                    lead = "TEXT", 
                    text_article = "TEXT",
                    text_all = "TEXT",
                    cover_img_url = "TEXT",
                    cover_img_alt = "TEXT",
                    cover_img_caption = "TEXT",
                    cover_img_rights = "TEXT",
                    available_online = "BOOLEAN",
                    capture_time = "TIMESTAMP",
                    doc_hash = "TEXT"),
                  indexes = list("link", "version", "doc_hash"), 
                  overwrite = F
  )
  
  # category_list df: 
  category_list <- tibble(
    doc_hash = character(0),
    category_text = character(0),
    category_url = character(0),
    version = character(0)
  )
  # alternatively: create dictionary to safe space
  # push to DB
  dplyr::copy_to(dest = con,
                  df = category_list %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                  name = "category_list", 
                  temporary = F, 
                  types = c(doc_hash = "TEXT", 
                            category_text = "TEXT",
                            category_url = "TEXT", 
                            version = "TEXT"),
                  indexes = list("doc_hash", "category_text", "version"), 
                  overwrite = F
  )
  
  # tag_list df: 
  tag_list <- tibble(
    doc_hash = character(0),
    tag_name = character(0),
    tag_url = character(0), 
    version = character(0)
  )
  # alternatively create dictionary to safe space
  # push to DB
  dplyr::copy_to(dest = con,
                  df = tag_list %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                  name = "tag_list", 
                  temporary = F, 
                  types = c(doc_hash = "TEXT", 
                            tag_name = "TEXT",
                            tag_url = "TEXT", 
                            version = "TEXT"),
                  indexes = list("doc_hash", "tag_name", "version"), 
                  overwrite = F
  )
  
  intext_links <- tibble(
    doc_hash = character(0),
    intext_link_url = character(0),
    intext_link_text = character(0)
  )
  # push to DB
  dplyr::copy_to(dest = con,
                  df = intext_links %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                  name = "intext_links", 
                  temporary = F, 
                  types = c(doc_hash = "TEXT", 
                            intext_link_url = "TEXT",
                            intext_link_text = "TEXT"),
                  indexes = list("doc_hash"), 
                  overwrite = F
  )
  
  recommendations_main <- tibble(
    doc_hash = character(0),
    main_recommendations_title = character(0),
    main_recommendations_link = character(0),
    main_recommendations_img = character(0),
    main_recommendations_alt = character(0)
  )
  # push to DB
  dplyr::copy_to(dest = con,
                  df = recommendations_main %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                  name = "recommendations_main", 
                  temporary = F, 
                  types = c(doc_hash = "TEXT",
                            main_recommendations_title = "TEXT",
                            main_recommendations_link = "TEXT",
                            main_recommendations_img = "TEXT",
                            main_recommendations_alt = "TEXT"),
                  indexes = list("doc_hash"), 
                  overwrite = F
  )
  
  recommendations_embedded <- tibble(
    doc_hash = character(0),
    recommendation_embedded_thumbnail = character(0),
    recommendation_embedded_title = character(0),
    recommendation_embedded_link = character(0)
  )
  # push to DB
  dplyr::copy_to(dest = con,
                  df = recommendations_embedded %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                  name = "recommendations_embedded", 
                  temporary = F, 
                  types = c(doc_hash = "TEXT",
                            recommendation_embedded_thumbnail = "TEXT",
                            recommendation_embedded_title = "TEXT",
                            recommendation_embedded_link = "TEXT"),
                  indexes = list("doc_hash"), 
                  overwrite = F
  )
  
  images <- tibble(
    doc_hash = character(0),
    image_url = character(0),
    image_caption = character(0),
    image_source = character(0)
  )
  # push to DB
  dplyr::copy_to(dest = con,
                  df = images %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                  name = "images", 
                  temporary = F, 
                  types = c(doc_hash = "TEXT",
                            image_url = "TEXT",
                            image_caption = "TEXT",
                            image_source = "TEXT"),
                  indexes = list("doc_hash"), 
                  overwrite = F
  )
  
  # Embeddings:  
  embeddings <- tibble(
    doc_hash = character(0),
    embedding_url = character(0),
    source = character(0)
  )
  # push to DB
  dplyr::copy_to(dest = con,
                  df = embeddings %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                  name = "embeddings", 
                  temporary = F, 
                  types = c(doc_hash = "TEXT",
                            embedding_url = "TEXT",
                            source = "TEXT"),
                  indexes = list("doc_hash", "source"), 
                  overwrite = F
  )
 DBI::dbDisconnect(con)
}

# just updating: 

scrape_pages <- function(pages_list = updated_pages, sleeptime = .5){

  # connect DB:
  pacman::p_load(DBI, RSQLite, tidyverse)
  RT_DB <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = "ignore/Russian_Media_Database_RT.sqlite") 
  
  #  loop over pages:
  for (i in 1:nrow(pages_list)) {
    
  # select version specific scraper:
    if(pages_list$version[i] %in%  c("bk", "bk-lat")) {

      scraper_bk(pages_list$loc[i], pages_list$version[i], RT_DB)
      
    } else if(pages_list$version[i] == "de") {
      
      if(!"try-error" %in% class(try(scraper_de(pages_list$loc[i], pages_list$version[i], RT_DB)))) {
        cat(i, ":", Sys.time(), pages_list$loc[i], "successfully saved.\n", sep = " ") # print in every run?
      } else {
        catch_not_captured(pages_list$loc[i], pages_list$version[i], RT_DB)
        cat(i, ":", Sys.time(), "failed to save", pages_list$loc[i], "\n", sep = " ") # print in every run?
      }
      
    } else if(pages_list$version[i] == "es") {
      
      if(!"try-error" %in% class(try(scraper_es(pages_list$loc[i], pages_list$version[i], RT_DB)))) {
        cat(i, ":", Sys.time(), pages_list$loc[i], "successfully saved.\n", sep = " ") # print in every run?
      } else {
        catch_not_captured(pages_list$loc[i], pages_list$version[i], RT_DB)
        cat(i, ":", Sys.time(), "failed to save", pages_list$loc[i], "\n", sep = " ") # print in every run?
      }
      
    } else if(pages_list$version[i] == "en") {
      
      if(!"try-error" %in% class(try(scraper_en(pages_list$loc[i], pages_list$version[i], RT_DB)))) {
        cat(i, ":", Sys.time(), pages_list$loc[i], "successfully saved.\n", sep = " ") # print in every run?
      } else {
        catch_not_captured(pages_list$loc[i], pages_list$version[i], RT_DB)
        cat(i, ":", Sys.time(), "failed to save", pages_list$loc[i], "\n", sep = " ") # print in every run?
      }
      
    } else if(pages_list$version[i] == "fr") {
      
      if(!"try-error" %in% class(try(scraper_fr(pages_list$loc[i], pages_list$version[i], RT_DB)))) {
        cat(i, ":", Sys.time(), pages_list$loc[i], "successfully saved.\n", sep = " ") # print in every run?
      } else {
        catch_not_captured(pages_list$loc[i], pages_list$version[i], RT_DB)
        cat(i, ":", Sys.time(), "failed to save", pages_list$loc[i], "\n", sep = " ") # print in every run?
      }
      
    } else if(pages_list$version[i] == "ru") {
      
      if(!"try-error" %in% class(try(scraper_ru(pages_list$loc[i], pages_list$version[i], RT_DB)))) {
        cat(i, ":", Sys.time(), pages_list$loc[i], "successfully saved.\n", sep = " ") # print in every run?
      } else {
        catch_not_captured(pages_list$loc[i], pages_list$version[i], RT_DB)
        cat(i, ":", Sys.time(), "failed to save", pages_list$loc[i], "\n", sep = " ") # print in every run?
      }
      
    } else { # if(pages_list$version[i] == "ar"){
      
      if(!"try-error" %in% class(try(scraper_ar(pages_list$loc[i], pages_list$version[i], RT_DB)))) {
        cat(i, ":", Sys.time(), pages_list$loc[i], "successfully saved.\n", sep = " ") # print in every run?
      } else {
        catch_not_captured(pages_list$loc[i], pages_list$version[i], RT_DB)
        cat(i, ":", Sys.time(), "failed to save", pages_list$loc[i], "\n", sep = " ") # print in every run?
        }
    }
    
    # sleep:
    Sys.sleep(sleeptime)
    cat(i, ":", Sys.time(), pages_list$loc[i], "saved.\n", sep = " ")
    
  } # end of loop
  
  dbDisconnect(RT_DB)
} # end of scraping function

# version specific scrapers (to be called inside function) #####

# RT Balkan:
scraper_bk <- function(link, version, con = RT_DB){
  
  pacman::p_load(tidyverse, DBI, RSQLite, rvest)
  
  # get page:
  base_url <- ifelse(version == "bk", "https://rt.rs", "https://lat.rt.rs")
  
if("try-error" %in% class(
  doc <- try(rvest::read_html(link)    
))) {
  catch_not_captured(link, version, con)
  cat(Sys.time(), "failed to save", link, "(main)", "\n", sep = " ")
  # write in logfile:
  write(x = paste(Sys.time(), link, "main",  sep = ", "), 
        file = "ignore/scrape_log.txt", append = T, sep = "\n")
} else{
  
  capture_time <- Sys.time()
  html_doc <- toString(doc)
  doc_hash <- rlang::hash(html_doc)
  
  
  # get page_data:
  
# save full html:
  html_pages <- tibble(
    link = link,
    doc_hash = doc_hash,
    html_doc = html_doc
  )
  # push to DB 
  DBI::dbWriteTable(conn = con, name = "html_pages", 
                    value = html_pages %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                    append = TRUE
  ) 
  
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

    available_online = T,
    capture_time = capture_time,
    doc_hash = doc_hash

  ) # end of main df
  # push to DB  
  DBI::dbWriteTable(conn = con, name = "page_data", 
                    value = page_data %>% dplyr::mutate(across(.cols = !is.character, as.character)),
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
    cat(Sys.time(), "failed to save", link, "(category_list)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "category_list",  sep = ", "), 
          file = "ignore/scrape_log.txt", append = T, sep = "\n")
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
    cat(Sys.time(), "failed to save", link, "(tag_lists)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "tag_lists",  sep = ", "), 
          file = "ignore/scrape_log.txt", append = T, sep = "\n")
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
    cat(Sys.time(), "failed to save", link, "(intext_links)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "intext_links",  sep = ", "), 
          file = "ignore/scrape_log.txt", append = T, sep = "\n")
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
    cat(Sys.time(), "failed to save", link, "(recommendations_main)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "recommendations_main",  sep = ", "), 
          file = "ignore/scrape_log.txt", append = T, sep = "\n")
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
    cat(Sys.time(), "failed to save", link, "(recommendations_embedded)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "recommendations_embedded",  sep = ", "), 
          file = "ignore/scrape_log.txt", append = T, sep = "\n")
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
    cat(Sys.time(), "failed to save", link, "(images)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "images",  sep = ", "), 
          file = "ignore/scrape_log.txt", append = T, sep = "\n")
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
    cat(Sys.time(), "failed to save", link, "(youtube_embeddings)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "youtube_embeddings",  sep = ", "), 
          file = "ignore/scrape_log.txt", append = T, sep = "\n")
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
    cat(Sys.time(), "failed to save", link, "(twitter_embeddings)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "twitter_embeddings",  sep = ", "), 
          file = "ignore/scrape_log.txt", append = T, sep = "\n")
    not_include <- append(not_include, "twitter_embeddings")
  }
    
           
  
  if("try-error" %in% class(
     odysee_embeddings <- try(tibble(
      doc_hash = doc_hash,
      embedding_url = doc %>% html_elements(".EmbedBlock-odysee iframe") %>% html_attr("data-src"),
      source = "odysee"
    )
    ))) {
    cat(Sys.time(), "failed to save", link, "(odysee_embeddings)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "odysee_embeddings",  sep = ", "), 
          file = "ignore/scrape_log.txt", append = T, sep = "\n")
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
    cat(Sys.time(), "failed to save", link, "(telegram_embeddings)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "telegram_embeddings",  sep = ", "), 
          file = "ignore/scrape_log.txt", append = T, sep = "\n")
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
    cat(Sys.time(), "failed to save", link, "(vk_embeddings)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "vk_embeddings",  sep = ", "), 
          file = "ignore/scrape_log.txt", append = T, sep = "\n")
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
    cat(Sys.time(), "failed to save", link, "(podbean_embeddings)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "podbean_embeddings",  sep = ", "), 
          file = "ignore/scrape_log.txt", append = T, sep = "\n")
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
    cat(Sys.time(), "failed to save", link, "(embeddings)", "\n", sep = " ")
    write(x = paste(Sys.time(), link, "embeddings",  sep = ", "), 
          file = "ignore/scrape_log.txt", append = T, sep = "\n")
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
scraper_de <- function(link, version, con = RT_DB){
  pacman::p_load(tidyverse, rvest)
  
  # try and record as missing if not available
  try(doc <- rvest::read_html(link))    
  
  base_url <- "https://de.rt.com"
  html_doc <- toString(doc)
  doc_hash <- rlang::hash(html_doc)
  capture_time <- Sys.time()
  
  if ("try-error" %in% class(doc)){
    catch_deleted(link, version, capture_time)
  }
  
  # get page_data:
  
  # save full html seperately:
  html_pages <- tibble(
    link = link,
    doc_hash = doc_hash,
    html_doc = html_doc
  )
  # push to DB 
  DBI::dbWriteTable(conn = con, name = "html_pages", 
                    value = html_pages %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                    append = TRUE
  ) 
  
  # main df:
  page_data <- tibble(
    header = doc %>% html_elements(xpath = "//meta[@property='og:title']") %>% html_attr("content"), 
    # or use header = doc %>% html_elements(., ".HeadLine-type_2") %>% html_text2() %>% stringr::str_c(., collapse = " ")
    link = link,
    version = version,
  
    author = doc %>% html_element("p:nth-child(1) em") %>% html_text2() %>% stringr::str_remove(., pattern = "[v]on|[V]on "),
    author_img = NA,
    
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
    
    available_online = T,
    capture_time = capture_time,
    doc_hash = doc_hash
    
  ) # end of main df
  # push to DB  
  DBI::dbWriteTable(conn = con, name = "page_data", 
                    value = page_data %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                    append = TRUE
  )
  
  
  # category_list df: 
  category_list <- tibble(
    doc_hash = doc_hash,
    category_text = doc %>% html_elements(., ".RTLink-root.RTLink-breadcrumb") %>% html_text2(),
    category_url = paste0(base_url, doc %>% html_nodes(., ".RTLink-root.RTLink-breadcrumb") %>% html_attr("href")),
    version = version
  )
  # alternatively: create dictionary to safe space
  # push to DB
  DBI::dbWriteTable(conn = con, name = "category_list", 
                    value = category_list %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                    append = TRUE
  ) 
  
  
  # tag_list df: 
  tag_list <- tibble(
    doc_hash = doc_hash,
    tag_name = doc %>% html_elements(".Tags-link") %>% html_text2(),
    tag_url = paste0(base_url, doc %>% html_nodes(".Tags-link") %>% html_attr("href")),
    version = version
  )
  # alternatively create dictionary to safe space
  # push to DB
  DBI::dbWriteTable(conn = con, name = "tag_list", 
                    value = tag_list %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                    append = TRUE
  ) 
  
  # intext_links df:
  intext_links <- tibble(
    doc_hash = doc_hash,
    intext_link_url = ifelse(doc %>% html_nodes(".ViewText-root a") %>% html_attr("href") %>% str_starts("/"),
                             paste0(base_url, doc %>% html_nodes(".ViewText-root a") %>% html_attr("href")),
                             doc %>% html_nodes(".ViewText-root a") %>% html_attr("href")
    ),
    intext_link_text = doc %>% html_nodes(".ViewText-root a") %>% html_text2()
  )
  # push to DB
  DBI::dbWriteTable(conn = con, name = "intext_links", 
                    value = intext_links %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                    append = TRUE
  ) 
  
  # recommendations_main df:
  recommendations_main <- tibble(
    doc_hash = doc_hash,
    main_recommendations_url = doc %>% html_elements("strong+ a") %>% html_attr("href"),
    main_recommendations_title = doc %>% html_elements("strong+ a") %>% html_text2(),
    main_recommendations_img = NA,
    main_recommendations_alt = NA
  )
  # push to DB
  DBI::dbWriteTable(conn = con, name = "recommendations_main", 
                    value = recommendations_main %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                    append = TRUE
  ) 
  
  # recommendations_embedded df:
  recommendations_embedded <- tibble(
    doc_hash = doc_hash,
    recommendation_embedded_thumbnail = doc %>% html_elements(".ReadMore-root .Picture-root img") %>% html_attr("data-src"),
    recommendation_embedded_title = doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_text2(),
    recommendation_embedded_url = ifelse(stringr::str_length(doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_attr("href")) > 0,
                                          stringr::str_c(base_url, doc %>% html_elements(".Card-title .Link-isFullCard") %>% html_attr("href")),
                                          NA)
  )
  # push to DB
  DBI::dbWriteTable(conn = con, name = "recommendations_embedded", 
                    value = recommendations_embedded %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                    append = TRUE
  ) 
  
  
  # images df:
  images <- tibble(
    doc_hash = doc_hash,
    image_url = doc %>% html_elements(".RTImage-image.RTImage-original picture img") %>% html_attr("data-src"),
    image_caption = doc %>% html_elements(".RTImage-caption") %>% html_text2(),
    image_source = doc %>% html_elements(".RTImage-source") %>% html_text2()
  )
  # push to DB
  DBI::dbWriteTable(conn = con, name = "images", 
                    value = images %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                    append = TRUE
  ) 
  
  
  # Embeddings:  
  youtube_embeddings <- tibble(
    doc_hash = doc_hash,
    embedding_url = if (length(doc %>% html_elements(".YouTubeEmbed") %>% html_attr("id")) > 0) {
      paste0("https://www.youtube.com/embed/", doc %>% html_elements(".YouTubeEmbed") %>% html_attr("id"))   
    } else{doc %>% html_elements(".YouTubeEmbed") %>% html_attr("id")},
    source = "youtube"
  )

    twitter_embeddings <- tibble(
    doc_hash = doc_hash,
    embedding_url = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']/*/a") %>% html_attr("href"),
    source = "twitter"
    # embeddings_twitter_date = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']/*/a") %>% html_text2() %>% lubridate::parse_date_time(., "%B %d, %Y") %>% as.character()
    # embeddings_twitter_text = doc %>% html_elements(xpath = "//div[@class='TwitterEmbed']") %>% html_text2()
  )
  
  odysee_embeddings <- tibble(
    doc_hash = doc_hash,
    embedding_url = doc %>% html_elements(".EmbedBlock-odysee iframe") %>% html_attr("data-src"),
    source = "odysee"
  )
  
  telegram_embeddings <- tibble(
    doc_hash = doc_hash,
    embedding_url = if (length(doc %>% html_elements(".TelegramEmbed script") %>% html_attr("data-telegram-post")) > 0) {
      paste0("https://t.me/", doc %>% html_elements(".TelegramEmbed script") %>% html_attr("data-telegram-post"))   
    } else{doc %>% html_elements(".TelegramEmbed script") %>% html_attr("data-telegram-post")},
    source = "telegram"
  )
  
  vk_embeddings <- tibble(
    doc_hash = doc_hash,
    embedding_url = doc %>% html_elements("iframe") %>% html_attr("data-src"),
    source = ifelse(doc %>% html_elements("iframe") %>% as.character() %>% str_detect("vk.com"),
                    "vk",
                    "other")
  )
  
  # other (Podcast etc.):
  podbean_embeddings <- tibble(
    doc_hash = doc_hash,
    embedding_url = doc %>% html_elements(".AllEmbed iframe") %>% html_attr("src"),
    source = ifelse(doc %>% html_elements(".AllEmbed iframe") %>% as.character() %>% str_detect("podbean"),
                    "podbean",
                    "other")
  )
  
  embeddings <- rbind(youtube_embeddings, twitter_embeddings, odysee_embeddings, telegram_embeddings, vk_embeddings, podbean_embeddings) # %>% unique()

  # push to DB
  DBI::dbWriteTable(conn = con, name = "embeddings", 
                    value = embeddings %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                    append = TRUE
  )
}

# RT espanol:
scraper_es <- function(link, version, con = RT_DB){
  # =~ scraper_de
}

# RT english:
scraper_en <- function(link, version, con = RT_DB){
  # ~ scraper_fr ~ scraper_ru
}

#RT francais: 
scraper_fr <- function(link, version, con = RT_DB){
  # ~ scraper_en ~ scraper_ru
}

# RT russian:
scraper_ru <- function(link, version, con = RT_DB){
  # ~ scraper_fr ~ scraper_en
}

# RT arabic:
scraper_ar <- function(link, version, con = RT_DB){
  
}

catch_not_captured <- function(link, version, con = con, capture_time = Sys.time()){
 
  pacman::p_load(tidyverse, DBI, RSQLite)
  
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
    available_online = F,
    capture_time = capture_time,
    doc_hash = NA
  ) # end of main df
  
  # push to DB 
  DBI::dbWriteTable(conn = con, name = "page_data", 
                    value = page_data %>% dplyr::mutate(across(.cols = !is.character, as.character)),
                    append = TRUE
  ) 
}

