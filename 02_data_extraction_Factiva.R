# 02 Data extraction factiva

# library(tm.plugin.factiva)
library(xml2)
library(tidyverse)
library(magrittr)







content = xml_find_all(read_html(paste0("articles/Factivahtml/factiva1.txt"), encoding = "UTF-8"), "//div[starts-with(@class, 'article ')]")
factiva_fulltexts = tibble(text = list(html_elements(content, ".articleParagraph") %>% html_text2()) %>% str_replace_all("[\n\r]", "") %>%
                             str_sub(., 4, -3) %>% str_remove_all(., '\\"') %>% str_remove_all(., "\n") %>% str_remove_all(., "\\\\"),
                           resort = html_element(content, "div") %>% html_text2(),
                           header = html_element(content, ".deHeadline") %>% html_text2(),
                          # author = xml_child(content[[1]], 5) %>% html_text2(),
                           wordcount = xml_find_all(content, "div")[str_detect(as.vector(xml_find_all(content, "div")), "Wörter")][1] %>% 
                             html_text2() %>% str_extract("^[[:digit:]]+") %>% as.integer(),
                           date = xml_find_all(content, "div")[str_detect(as.vector(xml_find_all(content, "div")), "2021</div>")][1] %>% 
                             html_text2() %>% as.Date(format = "%d %B %Y"),
                           source = xml_find_all(content, "div")[str_detect(xml_find_all(content, "div"), "DWELT") | 
                                                                   str_detect(xml_find_all(content, "div"), "SDDZ")| 
                                                                   str_detect(xml_find_all(content, "div"), "TAZ")| 
                                                                   str_detect(xml_find_all(content, "div"), "ZBILD")| 
                                                                   str_detect(xml_find_all(content, "div"), "FRARUN")] %>% 
                            html_text2(),
                         #  page = xml_child(content[[1]], 11) %>% html_text2(),
                           document = "delete")

for (article in dir("articles/Factivahtml/")) {

content = xml_find_all(read_html(paste0("articles/Factivahtml/", article), encoding = "UTF-8"), "//div[starts-with(@class, 'article ')]")

new_article = tibble(
  text = list(html_elements(content, ".articleParagraph") %>% html_text2()) %>% str_replace_all("[\n\r]", "") %>%
    str_sub(., 4, -3) %>% str_remove_all(., '\\"') %>% str_remove_all(., "\n") %>% str_remove_all(., "\\\\"),
  resort = html_element(content, "div") %>% html_text2(),
  header = html_element(content, ".deHeadline") %>% html_text2(),
  # author = xml_child(content[[1]], 5) %>% html_text2(),
  wordcount = xml_find_all(content, "div")[str_detect(as.vector(xml_find_all(content, "div")), "Wörter")][1] %>% 
    html_text2() %>% str_extract("^[[:digit:]]+") %>% as.integer(),
  date = xml_find_all(content, "div")[str_detect(as.vector(xml_find_all(content, "div")), "2021</div>")][1] %>% 
    html_text2() %>% as.Date(format = "%d %B %Y"),
  source = xml_find_all(content, "div")[str_detect(xml_find_all(content, "div"), "DWELT") | 
                                          str_detect(xml_find_all(content, "div"), "SDDZ")| 
                                          str_detect(xml_find_all(content, "div"), "TAZ")| 
                                          str_detect(xml_find_all(content, "div"), "ZBILD")| 
                                          str_detect(xml_find_all(content, "div"), "FRARUN")] %>% 
    html_text2(),document = article
  )

factiva_fulltexts = add_row(factiva_fulltexts, new_article) 
}
# for FRARUN detection of "Deutschlandausgabe" could be helpful to filter out local news

factiva_fulltexts %<>% filter(document != "delete") %>% 
  mutate(id = str_extract(document, "[:digit:]+") %>% as.integer()) %>%
  rename("datetime" = "date") %>% 
  select(-source) %>%
  arrange(id) 
  

# somethings wrong with [1:2] here!
df_candidates %<>% bind_cols(., factiva_fulltexts)

corpus_factiva <- left_join(df_factiva, 
                  df_candidates %>%  select(-c(source, lead, person, result_no, heading, header, dd, mm, month, date)), 
                  by = "links") %>% 
  left_join(., 
            df_Scholz %>% select(links, result_no),
            by = "links",
            suffix = c("", "_Scholz")) %>% 
  mutate(
    fulltext = ifelse(!is.na(text), str_c(heading, text, sep = " "), NA),
    link = links %>% str_sub(., start = 61),
    intextlinks = list(character(0)),
    mehrlink = list(character(0)),
    baerbock = ifelse(person == "Baerbock", 1, 0),
    laschet = ifelse(person == "Laschet", 1, 0),
    scholz = ifelse(person == "Scholz", 1, 0), 
    mentions_baerbock = str_count(fulltext, "Baerbock"),
    mentions_laschet = str_count(fulltext, "Laschet"),
    mentions_scholz = str_count(fulltext, "Scholz")
  ) %>% 
  rename(
    "saved" = "result_no",
    "saved_scholz" = "result_no_Scholz",
    "saved_baerbock" = "result_no_Baerbock",
    "saved_laschet" = "result_no_Laschet",
    "header" = "heading"
  ) %>% 
#  select(-c(dd, mm, month, person, person_Laschet, person_Baerbock, links)) %>% 
  arrange(id) %>% 
  distinct(link, .keep_all = T) %>%   
  select(names(corpus_RT))


# by candidate: 
corpus_factiva_candidate <- 
  pivot_longer(corpus_factiva, 
               cols = c("scholz", "baerbock", "laschet"), 
               names_to = "candidate") %>% 
  filter(value == 1)


# create corpus-object
library(quanteda)
factiva_corpus <- corpus(corpus_factiva, 
                    docid_field = "link", 
                    text_field = "fulltext", 
                    meta = list("resort", "date", "scholz", "baerbock", "laschet", "mentions_baerbock", "mentions_scholz", "mentions_laschet", "wordcount", "source"), 
                    unique_docnames = T)

factiva_corpus_candidate <- corpus(corpus_factiva_candidate, 
                              docid_field = "id", 
                              text_field = "fulltext", 
                              meta = list("resort", "date", "candidate", "mentions_baerbock", "mentions_scholz", "mentions_laschet", "wordcount", "source"), 
                              unique_docnames = F)



# save
openxlsx::write.xlsx(corpus_factiva, "articles/corpus_factiva.xlsx", asTable = T, overwrite = T)
save(list = c("corpus_factiva", 
              "corpus_factiva_candidate",
              "factiva_corpus",
              "factiva_corpus_candidate"), 
     file = "articles/corpus_factiva.RData") 


# # get missed dates
# test <- filter(factiva_fulltexts, is.na(factiva_fulltexts$date))
# i = 1
# content = xml_find_all(read_html(paste0("articles/Factivahtml/", test$document[i]), encoding = "UTF-8"), "//div[starts-with(@class, 'article ')]")
# 
# factiva_fulltexts_missed = tibble(text = list(html_elements(content, ".articleParagraph") %>% html_text2()) %>% str_replace_all("[\n\r]", "") %>%
#                              str_sub(., 4, -3) %>% str_remove_all(., '\\"') %>% str_remove_all(., "\n") %>% str_remove_all(., "\\\\"),
#                            resort = html_element(content, "div") %>% html_text2(),
#                            header = html_element(content, ".deHeadline") %>% html_text2(),
#                            # author = xml_child(content[[1]], 5) %>% html_text2(),
#                            wordcount = xml_find_all(content, "div")[str_detect(as.vector(xml_find_all(content, "div")), "Wörter")][1] %>% 
#                              html_text2() %>% str_extract("^[[:digit:]]+") %>% as.integer(),
#                            date = xml_find_all(content, "div")[str_detect(as.vector(xml_find_all(content, "div")), "2021")][1] %>% 
#                              html_text2() %>% as.Date(format = "%d %B %Y"),
#                            source = xml_find_all(content, "div")[str_detect(xml_find_all(content, "div"), "DWELT") | 
#                                                                    str_detect(xml_find_all(content, "div"), "SDDZ")| 
#                                                                    str_detect(xml_find_all(content, "div"), "TAZ")| 
#                                                                    str_detect(xml_find_all(content, "div"), "ZBILD")| 
#                                                                    str_detect(xml_find_all(content, "div"), "FRARUN")] %>% 
#                              html_text2(),
#                            #  page = xml_child(content[[1]], 11) %>% html_text2(),
#                            document = "delete")
# 
# 
# 
# for (i in test$document) {
# 
# content = xml_find_all(read_html(paste0("articles/Factivahtml/", test$document[i]), encoding = "UTF-8"), "//div[starts-with(@class, 'article ')]")
# 
# new_article = tibble(text = list(html_elements(content, ".articleParagraph") %>% html_text2()) %>% str_replace_all("[\n\r]", "") %>%
#                              str_sub(., 4, -3) %>% str_remove_all(., '\\"') %>% str_remove_all(., "\n") %>% str_remove_all(., "\\\\"),
#                            resort = html_element(content, "div") %>% html_text2(),
#                            header = html_element(content, ".deHeadline") %>% html_text2(),
#                            # author = xml_child(content[[1]], 5) %>% html_text2(),
#                            wordcount = xml_find_all(content, "div")[str_detect(as.vector(xml_find_all(content, "div")), "Wörter")][1] %>% 
#                              html_text2() %>% str_extract("^[[:digit:]]+") %>% as.integer(),
#                            date = xml_find_all(content, "div")[str_detect(as.vector(xml_find_all(content, "div")), "2021")][1] %>% 
#                              html_text2() %>% as.Date(format = "%d %B %Y"),
#                            source = xml_find_all(content, "div")[str_detect(xml_find_all(content, "div"), "DWELT") | 
#                                                                    str_detect(xml_find_all(content, "div"), "SDDZ")| 
#                                                                    str_detect(xml_find_all(content, "div"), "TAZ")| 
#                                                                    str_detect(xml_find_all(content, "div"), "ZBILD")| 
#                                                                    str_detect(xml_find_all(content, "div"), "FRARUN")] %>% 
#                              html_text2(),
#                            #  page = xml_child(content[[1]], 11) %>% html_text2(),
#                            document = test$document[i])
# 
# 
# factiva_fulltexts_missed = add_row(factiva_fulltexts_missed, new_article)
# 
# }








# 
# p_load(tm.plugin.factiva, NLP, tm, xml2, rvest)
# 
# 
# a = readFactivaHTML(test)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# a = readFactivaHTML("articles/Factivahtml/factiva1.txt")
# a$content[[1]]
# a = test()
# 
# test <- function (elem, language, id) 
# {
#   language = NA
#   
#   tmp_file <- tmp_file
#   
#   if (is.na(language)) {
#     cl <- xml_attr(tmp_file, "class")
#     language <- regmatches(cl, regexec("^article ([[:alpha:]]{2})Article$", 
#                                        cl))[[1]][2]
#   }
#   
#   table <- as.data.frame(html_table(xml_children(tmp_file)[[1]]))
#   
#   
#   text <- gsub("[\n\r]", "", xml_text(xml_find_all(tmp_file, 
#                                                    ".//p[starts-with(@class, 'articleParagraph')]")))
#   vars <- c("AN", "BY", "CO", "CY", "ED", "HD", "IN", "IPC", 
#             "IPD", "LA", "LP", "NS", "PD", "PG", "PUB", "RE", "SE", 
#             "SN", "TD", "WC")
#   data <- as.character(table[match(vars, gsub("[^[A-Z]", "", 
#                                               table[, 1])), 2])
#   names(data) <- vars
#   date <- strptime(data[["PD"]], "%d %B %Y")
#   if (is.na(date) && isTRUE(data[["PD"]] != "")) {
#     old.locale <- Sys.getlocale("LC_TIME")
#     Sys.setlocale("LC_TIME", "C")
#     date <- strptime(data[["PD"]], "%d %B %Y")
#     Sys.setlocale("LC_TIME", old.locale)
#     if (Sys.info()["sysname"] == "Darwin") 
#       date <- strptime(sub("[jJ]uillet", "07", data[["PD"]]), 
#                        "%d %m %Y")
#     if (any(is.na(date))) 
#       warning(sprintf("Could not parse document date \"%s\". You may need to change the system locale to match that of the corpus. See LC_TIME in ?Sys.setlocale.", 
#                       data[["PD"]]))
#   }
#   data[["AN"]] <- gsub("Document ", "", data[["AN"]])
#   id <- if (!is.na(data[["AN"]])) 
#     data[["AN"]]
#   else paste(sample(LETTERS, 10), collapse = "")
#   wc <- as.integer(regmatches(data[["WC"]], regexpr("^[[:digit:]]+", 
#                                                     data[["WC"]])))[[1]]
#   subject <- if (!is.na(data[["NS"]])) 
#     strsplit(data[["NS"]], "( \\| )")[[1]]
#   else character(0)
#   subject <- gsub("[^[:print:]]", "", subject)
#   subject <- gsub(".* : ", "", subject)
#   coverage <- if (!is.na(data[["RE"]])) 
#     strsplit(data[["RE"]], "( \\| )")[[1]]
#   else character(0)
#   coverage <- gsub("[^[:print:]]", "", coverage)
#   coverage <- gsub(".* : ", "", coverage)
#   company <- if (!is.na(data[["CO"]])) 
#     strsplit(data[["CO"]], "( \\| )")[[1]]
#   else character(0)
#   company <- gsub("[^[:print:]]", "", company)
#   company <- gsub(".* : ", "", company)
#   industry <- if (!is.na(data[["IN"]])) 
#     strsplit(data[["IN"]], "( \\| )")[[1]]
#   else character(0)
#   industry <- gsub("[^[:print:]]", "", industry)
#   industry <- gsub(".* : ", "", industry)
#   infocode <- if (!is.na(data[["IPC"]])) 
#     strsplit(data[["IPC"]], "( \\| )")[[1]]
#   else character(0)
#   infocode <- gsub("[^[:print:]]", "", infocode)
#   infocode <- gsub(".* : ", "", infocode)
#   infodesc <- if (!is.na(data[["IPD"]])) 
#     strsplit(data[["IPD"]], "( +\\| +| +-+ +| +--+|--+ +|\\._)")[[1]]
#   else character(0)
#   infodesc <- gsub("[^[:print:]]", "", infodesc)
#   infodesc <- gsub(".* : ", "", infodesc)
#   doc <- PlainTextDocument(x = text, author = if (!is.na(data[["BY"]])) 
#     data[["BY"]]
#     else character(0), datetimestamp = date, heading = if (!is.na(data[["HD"]])) 
#       data[["HD"]]
#     else character(0), id = id, origin = if (!is.na(data[["SN"]])) 
#       data[["SN"]]
#     else character(0), language = language)
#   meta(doc, "edition") <- if (!is.na(data[["ED"]])) 
#     data[["ED"]]
#   else character(0)
#   meta(doc, "section") <- if (!is.na(data[["SE"]])) 
#     data[["SE"]]
#   else character(0)
#   meta(doc, "subject") <- subject
#   meta(doc, "coverage") <- coverage
#   meta(doc, "company") <- company
#   meta(doc, "industry") <- industry
#   meta(doc, "infocode") <- infocode
#   meta(doc, "infodesc") <- infodesc
#   meta(doc, "page") <- if (!is.na(data[["PG"]])) 
#     data[["PG"]]
#   else character(0)
#   meta(doc, "wordcount") <- wc
#   meta(doc, "publisher") <- if (!is.na(data[["PUB"]])) 
#     data[["PUB"]]
#   else character(0)
#   meta(doc, "rights") <- if (!is.na(data[["CY"]])) 
#     data[["CY"]]
#   else character(0)
#   doc
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # prepare vectors to bind later:
# links <- c()
# source <- c()
# info <- c()
# lead <- c()
# person <- c()
# heading <- c()
# result_no <- c()
# text <- c()
# 
# files <- dir("articles/Factivahtml/")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# content <- xml_find_all(tmp_file, "//div[starts-with(@class, 'article ')]")
# SimpleSource("UTF-8", length = length(content), content = content, 
#              uri = x, reader = readFactivaHTML, class = "FactivaSource")
# 
# 
# 
# 
# for (i in dir("articles/Factivahtml/")) {
#   
#   tmp_file <- readr::read_file(paste0("articles/Factivahtml/", files[i])) %>% 
#     read_html(encoding = "UTF-8")
#   
# 
#   header = html_elements(tmp_file, ".deHeadline")  %>% html_text2()
#   text = gsub("[\n\r]", "", html_elements(tmp_file, ".dearticleParagraph") %>% html_text2())
# 
#   
#   
# str_c(paste(text))
# 
# }
#   
#     test <- tm.plugin.factiva::FactivaSource(tmp_file)
#   
#     test <- readFactivaHTML()
#     
#     readtext <- append(text,
#                    )
#     
#     links <- append(links,
#                     paste0(base_url, 
#                            str_sub(new_links[n], 3)) %>%  
#                       as.character())
#     heading <- append(heading,
#                       tmp_titles[n] %>% 
#                         as.character())
#     source <- append(source,
#                      tmp_file %>%
#                        html_element(paste0(".headline:nth-child(", n ,") .leadFields a")) %>%
#                        html_text2() %>% as.character())
#     info <- append(info,
#                    tmp_file %>%
#                      html_element(xpath = paste0('//*[@id="headlines"]/table/tbody/tr[', n, ']/td[3]/div[1]/text()')) %>%
#                      html_text2() %>% as.character())
#     lead <- append(lead,
#                    tmp_file %>%
#                      html_element(xpath = paste0('//*[@id="headlines"]/table/tbody/tr[', n, ']/td[3]/div[2]/text()')) %>%
#                      html_text2() %>% as.character())
#     person <- append(person,
#                      tmp_candidate)
#     result_no <- append(result_no,
#                         tmp_file %>%
#                           html_element(".resultsBar") %>%
#                           html_text2())
# }
# 
# 
# 
# 
# 
# 
# 
# for (j in n){
#   Factiva_sheet <- paste0("articles/broken/facti", candidate[i] , "_files/Factiva", str_sub(candidate[i], 1, 1), j, ".htm")
#   
#   source <- tm.plugin.factiva::FactivaSource
#   "articles/broken/FactivaB1.html", 
#                                              encoding = "UTF-8", 
#                                              format = "HTML")
#   
#   # more efficient to loop over source only, but need to include candidate variable, therefore:
#   corpus <- append(corpus, tm::Corpus(source))
# }
# 
# 
# ### flatten factiva list objects to df ####
# 
# df <- tibble()
# for (i in length(corpus)) {
#   df <- df %>% rbind(.,
#                      tibble(
#                        id = corpus[[i]][["meta"]][["id"]],
#                        heading = corpus[[i]][["meta"]][["heading"]],
#                        source = corpus[[i]][["meta"]][["origin"]],
#                        mention = names(corpus[i]),
#                        author = corpus[[i]][["meta"]][["author"]],
#                        resort = na_if(str_c(corpus[[i]][["meta"]][["section"]], collapse = ","), ""),
#                        wordcount = corpus[[i]][["meta"]][["wordcount"]],
#                        page = as.numeric(corpus[[i]][["meta"]][["page"]]),
#                        date = corpus[[i]][["meta"]][["datetimestamp"]],
#                        text = str_c(corpus[[i]]$content, collapse = " ")
#                      ) 
#   )
# }
# 
# 
