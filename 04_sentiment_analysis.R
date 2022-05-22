# 04 sentiment analysis

library(pacman)
p_load(quanteda,
       quanteda.textstats, # to calculate wordfrequencies/Similarity/Readability
#       quanteda.textplots, # for keyword/network/wordcloud graphs
       tidyverse,
       lubridate,
       magrittr)

# parallel processing:
library(parallel)
library(iterators)
library(foreach)
# library(doParallel)

# parallel processing
{split = detectCores()
eachStart = 25

cl = makeCluster(split)
init = clusterEvalQ(cl, { library(MASS); NULL })
results = parLapplyLB(cl
                      ,rep(eachStart, split)
                      ,function(nstart) kmeans(Boston, 4, nstart=nstart))
withinss = sapply(results, function(result) result$tot.withinss)
result = results[[which.min(withinss)]]
stopCluster(cl)
}


save(list = c("corpus", "corpus_candidates",
              "RT_corpus", "RT_corpus_candidate", 
              "factiva_corpus", "factiva_corpus_candidate",
              "all_sources", "all_sources_candidates", 
              "baerbock_context", "scholz_context", "laschet_context", # probably unnecessary
              "headlead_corpus", "headlead_corpus_candidates"
              ), 
     file = "analyses/corpora.RData")

save(list = c("corpus_tokenized_source",
              "headlead_tokenized_articles", 
              "headlead_tokenized_source_day", "headlead_tokenized_source_week", 
              "headlead_tokenized_source_day_candidate", "headlead_tokenized_source_week_candidate",
              "headlead_tokenized_source", "headlead_tokenized_source_day",
              "corpus_candidates_tokenized_source", 
              "corpus_candidates_tokenized_source_candidate", 
              "corpus_candidates_tokenized_candidate",
              "corpus_candidates_tokenized_day", "corpus_candidates_tokenized_week",
              "corpus_articles_tokenized"
              ), 
file = "analyses/corpora_tokenized.RData")

save(list = c("articles_score",
              "source_candidate_sentiment", "source_candidate_sentiment_week", "source_candidate_sentiment_day",
              "source_candidates_sentiment", "source_candidates_sentiment_RT_rest",
              "source_base_sentiment",            # careful with this one: on all for RT, on candidates only for rest
              "candidate_base_sentiment", 
              "headlead_source_sentiment", "headlead_source_sentiment_day",
              "headlead_article_score", 
              "headlead_score_day", "headlead_score_week",
              "headlead_score_candidate_day", "headlead_score_candidate_week", # also avg
              "scholz_sentiment_5", "laschet_sentiment_5", "baerbock_sentiment_5",
              "scholz_sentiment_15", "laschet_sentiment_15", "baerbock_sentiment_15" # also with headlead
              ), 
     file = "analyses/sentiment_results.RData")

# individual sources
load("articles/corpus_factiva.RData")
load("articles/RTcorpus.RData")

# all corpora
load("analyses/corpora.RData")


# tokenized corpora
load("analyses/corpora_tokenized.RData")

# results
load("analyses/sentiment_results.RData")


# load dictionaries: ####
# political dictionary:
{
  load("analyses/Rauh_SentDictionaryGerman.RData")
  load("analyses/Rauh_SentDictionaryGerman_Negation.Rdata")
  
# sent.dictionary with 37080 entries

# trim dictionary entries:
neg.sent.dictionary %<>% mutate(feature = str_trim(feature),
                                pattern = str_trim(pattern),
                                replacement = str_trim(replacement))
sent.dictionary %<>% mutate(feature = str_trim(feature)) %>% 
  # combine them:
  bind_rows(.,
            neg.sent.dictionary %>% select(feature, sentiment) %>% mutate(sentiment = as.character(sentiment)))
  

# convert to dictionary object
sent.dictionary <- dictionary(list(positive = str_trim(sent.dictionary$feature[sent.dictionary$sentiment>0]), negative = str_trim(sent.dictionary$feature[sent.dictionary$sentiment<0])))

}

# media dictionary:
sentiWS_neg <- read_delim("analyses/SentiWS_v2.0_Negative.txt", col_names = c("word", "polarity", "variants"))


# build full corpus: ####
RT <- rio::import("articles/RT_corpus.xlsx")
Factiva <- rio::import("articles/corpus_factiva.xlsx")

all_sources <- bind_rows(RT, select(Factiva, names(RT))) 

multiword <- c("annalena baerbock", "armin laschet", "olaf scholz")
# use this when tokenizing:
tokens_replace(x = all_sources$fulltext, pattern = phrase(multiword), replacement = phrase(c("baerbock", "laschet", "scholz")))


# very long processing!
all_sources$fulltext <- str_replace_all(all_sources$fulltext,
                                        setNames(
                                          object = neg.sent.dictionary$replacement,
                                          nm = neg.sent.dictionary$pattern
                                            )
                                        )
all_sources %<>% left_join(., 
                          bind_rows(RT, select(Factiva, names(RT))) %>%  select(link, fulltext),
                          by = "link", 
                          suffix = c("", "_original"))

all_sources_candidates <- all_sources %>% filter(scholz == 1 |
                                       baerbock == 1 |
                                       laschet == 1 |
                                       mentions_baerbock > 0 |
                                       mentions_laschet > 0 |
                                       mentions_scholz > 0 |
                                       !is.na(saved_baerbock) |
                                       !is.na(saved_laschet) |
                                       !is.na(saved_scholz)) %>% 
  pivot_longer(., 
               cols = c("scholz", "baerbock", "laschet"), 
               names_to = "candidate") %>% 
  filter(value == 1) %>%  # select this away?
  rename("negative_score" = "negative_score.x", 
         "sent_score" = "sent_score.x") %>% 
  select(-ends_with(c(".x", ".y")), -positve_binary)
  
RT <- all_sources %>% filter(., source == "RTDE")
Factiva <- all_sources %>% filter(., source != "RTDE")

RT_candidates <- all_sources_candidates %>% filter(source == "RTDE")
Factiva_candidates <- all_sources_candidates %>% filter(source != "RTDE")

# slow way: 
# replace_negations <- function(variable, dictionary = neg.sent.dictionary){
#   for (i in 1:nrow(neg.sent.dictionary)){ 
#     variable <- str_replace_all(string = variable, 
#                             pattern = dictionary$pattern[i], 
#                             replacement = dictionary$replacement[i])
#     # text2 <- gsub(neg.sent.dictionary$pattern[i], neg.sent.dictionary$replacement[i], text2, fixed = FALSE)
#     }
#   }
# 
# all_sources$fulltext %<>% replace_negations()


# for headlines and lead sentence:
all_sources %<>% mutate(
  head_lead_original = str_c(header, lead)
)

all_sources$head_lead <- str_replace_all(all_sources$head_lead_original,
                                        setNames(
                                          object = neg.sent.dictionary$replacement,
                                          nm = neg.sent.dictionary$pattern
                                          )
                                        )
all_sources %<>% 
  rename("negative_score" = "negative_score.x", 
       "sent_score" = "sent_score.x") %>% 
  select(-ends_with(c(".x", ".y")), -positve_binary)

# by article:
headlead_corpus <- 
  corpus(all_sources, 
       docid_field = "link", 
       text_field = "head_lead", 
       meta = list("resort", "date", "candidate", "mentions_baerbock", "mentions_scholz", "mentions_laschet", "wordcount", "source"),
       unique_docnames = T
)

headlead_tokenized_articles <- 
  headlead_corpus %>% 
    tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>% 
    tokens_tolower() %>% 
    tokens_remove(stopwords("de"))

(headlead_article_score <- 
headlead_corpus %>% 
  dfm() %>% 
  dfm_lookup(., sent.dictionary) 
) %>% 
  dfm_weight(., scheme = "prop")

# by day:

headlead_tokenized_source_day <- 
  headlead_corpus %>% 
  corpus_group(interaction(source, date)) %>%
  tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("de"))

(headlead_score_day <- 
  headlead_tokenized_source_day %>% 
  dfm() %>% 
  dfm_lookup(., sent.dictionary) 
) %>% 
  dfm_weight(., scheme = "prop")


# by week:
headlead_tokenized_source_week <- 
  headlead_corpus %>% 
  corpus_group(interaction(source, week(date))) %>%
  tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("de"))

(headlead_score_week <- 
    headlead_tokenized_source_week %>% 
    dfm() %>% 
    dfm_lookup(., sent.dictionary) 
) %>% 
  dfm_weight(., scheme = "prop")



# for candidate articles: 

headlead_corpus_candidates <- 
  corpus(all_sources_candidates, 
         docid_field = "link", 
         text_field = "head_lead", 
         meta = list("resort", "date", "candidate", "mentions_baerbock", "mentions_scholz", "mentions_laschet", "wordcount", "source"),
         unique_docnames = F
  )


# by day:
headlead_tokenized_source_day_candidate <- 
  headlead_corpus_candidates %>% 
  corpus_group(interaction(source, date, candidate)) %>%
  tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("de"))

(headlead_score_candidate_day <-  
    headlead_tokenized_source_day_candidate %>% 
    dfm() %>% 
    dfm_lookup(., sent.dictionary) 
) %>% 
  dfm_weight(., scheme = "prop")


# by week:
headlead_tokenized_source_week_candidate <- 
  headlead_corpus_candidates %>% 
  corpus_group(interaction(source, week(date), candidate)) %>%
  tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("de"))

(headlead_score_candidate_week <-  
    headlead_tokenized_source_week_candidate %>% 
    dfm() %>% 
    dfm_lookup(., sent.dictionary) 
) %>% 
  dfm_weight(., scheme = "prop")




# build main corpora
corpus <- corpus(all_sources, 
                 docid_field = "link", 
                 text_field = "fulltext", 
                 meta = list("resort", "date", "candidate", "mentions_baerbock", "mentions_scholz", "mentions_laschet", "wordcount", "source"),
                 unique_docnames = F
                 )

corpus_candidates <- corpus(all_sources_candidates, 
                 docid_field = "link", 
                 text_field = "fulltext", 
                 meta = list("resort", "date", "candidate", "mentions_baerbock", "mentions_scholz", "mentions_laschet", "wordcount", "source"),
                 unique_docnames = F
)

# corpus_candidates <- c(RT_corpus_candidate, factiva_corpus_candidate)


# analyse: ####

# by candidate:
# corpus_candidates_tokenized_wordstem <- corpus_candidates %>%
#   corpus_group(candidate) %>%
#   tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>% 
#   tokens_tolower() %>% 
#   tokens_remove(stopwords("de")) %>% 
#   # check if this influences sentiment analysis
#   tokens_wordstem("de")
corpus_candidates_tokenized_candidate <- corpus_candidates %>%
  corpus_group(candidate) %>%
  tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("de"))

(candidate_base_sentiment <- 
    corpus_candidates_tokenized_candidate %>% 
    dfm() %>% 
    dfm_lookup(., sent.dictionary) 
) %>% 
  dfm_weight(., scheme = "prop")

# by candidate/date:
# week:
corpus_candidates_tokenized_week <- corpus_candidates %>%
  corpus_group(interaction(source, week(date), candidate), fill = T) %>%
  tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>%
  tokens_tolower()

(source_candidate_sentiment_week <-
    corpus_candidates_tokenized_week %>%
    dfm() %>%
    dfm_lookup(., sent.dictionary)
) %>%
    dfm_weight(., scheme = "prop")

# day:
corpus_candidates_tokenized_day <- corpus_candidates %>%
  corpus_group(interaction(source, date, candidate), fill = T) %>%
  tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("de"))

(source_candidate_sentiment_day <-
    corpus_candidates_tokenized_day %>%
    dfm() %>%
    dfm_lookup(., sent.dictionary)
) %>%
    dfm_weight(., scheme = "prop")

# by candidate/source
corpus_candidates_tokenized_source_candidate <- corpus_candidates %>%
  corpus_group(interaction(source, candidate)) %>%
  tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("de")) 

(source_candidate_sentiment <- 
  corpus_candidates_tokenized_source_candidate %>% 
    dfm() %>% 
    dfm_lookup(., sent.dictionary) 
) %>% 
  dfm_weight(., scheme = "prop")

# by source:
corpus_candidates_tokenized_source <- corpus_candidates %>%
  corpus_group(source) %>%
  tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("de")) 

corpus_tokenized_source <- corpus %>%
  corpus_group(source) %>%
  tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("de")) 

# base sentiment:
  # for RT: general sentiment, for Rest: general candidate sentimnent
(source_base_sentiment <- 
  corpus_tokenized_source %>% 
  dfm() %>% 
  dfm_lookup(., sent.dictionary) 
) %>% 
  dfm_weight(., scheme = "prop")

  # additionally: 
    # run for headlines/lead

headlead_tokenized_source_day  <- 
  headlead_corpus %>% 
  corpus_group(interaction(source, date)) %>%
  tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("de"))

(headlead_source_sentiment_day <- 
  headlead_tokenized_source_day %>% 
  dfm() %>% 
  dfm_lookup(., sent.dictionary)
) %>% 
    dfm_weight(., scheme = "prop")


headlead_tokenized_source <- 
 headlead_corpus %>%  
   corpus_group(source) %>%
   tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>% 
   tokens_tolower() %>% 
   tokens_remove(stopwords("de")) 

(headlead_source_sentiment <- 
  headlead_tokenized_source %>% 
  dfm() %>% 
  dfm_lookup(., sent.dictionary) 
) %>% 
  dfm_weight(., scheme = "prop")



# additionally:
    # run for candidate articles
(source_candidates_sentiment <- 
  corpus_candidates_tokenized_source %>% 
  dfm() %>% 
  dfm_lookup(., sent.dictionary) 
) %>% 
  dfm_weight(., scheme = "prop")


(source_candidates_sentiment_RT_rest <- 
    headlead_corpus %>% 
    corpus_group(source == "RTDE") %>%
    tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>% 
    tokens_tolower() %>% 
    tokens_remove(stopwords("de")) %>% 
  
    dfm() %>% 
    dfm_lookup(., sent.dictionary) 
) %>% 
  dfm_weight(., scheme = "prop")


# by source/date:
# corpus_tokenized <- corpus %>%
#   corpus_group(interaction(source, week(date)), fill = T) %>%
#   tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>% 
#   tokens_tolower() %>% 
#   tokens_remove(stopwords("de")) 
#   # %>% 
#   # # check if this influences sentiment analysis
#   # tokens_wordstem("de")
# 
# 
# corpus_dfm <- dfm(corpus_tokenized)
# corpus_sentiment <- dfm(corpus_tokenized, dictionary = sentiment.lexikon.rauh)
# dfm_weight(corpus_dfm %>% dfm_lookup(., sentiment.lexikon.rauh), scheme = "prop")


# by article -> score and binary 
corpus_articles_tokenized <- corpus %>%
  tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("de"))
  
# corpus_articles_tokenized_wordstem <- corpus %>%
#   tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>% 
#   tokens_tolower() %>% 
#   tokens_remove(stopwords("de")) %>% 
#   # check if this influences sentiment analysis
#   tokens_wordstem("de")


# classification (or rather detection of any feature from category -> do by hand)
# articles_classification <- 
#   corpus_articles_tokenized %>% 
#   dfm() %>% 
#   dfm_lookup(., sent.dictionary) %>% 
#   dfm_weight(., scheme = "boolean")

# articles_classification_wordstem <- 
#   corpus_articles_tokenized_wordstem %>% 
#   dfm() %>% 
#   dfm_lookup(., sent.dictionary) %>% 
#   dfm_weight(., scheme = "boolean")


# score:
(articles_score <- 
  corpus_articles_tokenized %>% 
  dfm() %>% 
  dfm_lookup(., sent.dictionary) 
) %>% 
  dfm_weight(., scheme = "prop")

# articles_score_wordstem <- 
#   corpus_articles_tokenized_wordstem %>% 
#   dfm() %>% 
#   dfm_lookup(., sent.dictionary) %>% 
#   dfm_weight(., scheme = "prop")

# RT only (deprecated?) ####
# by resort:
  # ToDobuild groups to compare across media 
tokens_RT_resort <- RT_corpus %>%
  corpus_group(groups = resort) %>%
  tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T, padding = F) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("de")) %>% 
  # check if this influences sentiment analysis
  tokens_wordstem("de")

# by candidate
tokens_RT_candidate <- RT_corpus_candidate %>%
  corpus_group(groups = candidate) %>%
  tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T, padding = F) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("de")) %>% 
  # check if this influences sentiment analysis
  tokens_wordstem("de")

# ToDo:
# by candidate/timeframe (week?)
tokens_RT_candidate_week <- RT_corpus_candidate %>%
  corpus_group(groups = interaction(candidate, lubridate::week(date))) %>%
  tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T, padding = F) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("de")) %>%
  # check if this influences sentiment analysis
  tokens_wordstem("de")



# document-feature matrix
sentiment_resorts <- dfm(tokens_RT_resort, dictionary = sent.dictionary)
sentiment_candidates <- dfm(tokens_RT_candidate, dictionary = sent.dictionary)

# use wordcounts
test <- dfm_trim(source_candidate_sentiment, min_termfreq = 5)
topfeatures(source_candidate_sentiment, 20)

toks_article <- corpus_RT %>% dfm_lookup(sent.dictionary) %>% tokens()
article_sentiment <- dfm(toks_article, dictionary = sentiment.lexikon.rauh) 
article_sentiment_prop <- dfm_weight(article_sentiment, scheme = "prop")





### wordcount -> types/tokens/sentences

# plotting features (pos-neg) ####
# by candidate/source
features_frequencies_candidates <- 
  source_candidate_sentiment %>% 
  textstat_frequency(n = 10, group = interaction(candidate, source)) %>% 
  bind_cols(., 
            str_split(string = features_frequencies_candidates$group, pattern = "\\.") %>% 
              jsonlite::toJSON() %>% jsonlite::fromJSON(simplifyDataFrame = T) %>% 
              tibble::as_tibble() %>% rename("candidate" = V1, "source" = V2)) 

#  mutate(sent_score = log((positive + 0.5) / (negative + 0.5))


features_frequencies_candidates %>%
  mutate(candidate = factor(candidate)) %>%
  ggplot(aes(x = reorder(feature, frequency), y = frequency, fill = candidate)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "share of words per candidate") +
  facet_wrap(~group, ncol = 3) +
  coord_flip()




# targeted sentiment analysis: ####

# candidates in context:
  # ToDo: allow composita before/after & optional first-name

baerbock_context <- kwic(corpus_articles_tokenized, "baerbock*", window = 15) %>% mutate(context = str_c(pre, post, sep = " "))
scholz_context <- kwic(corpus_articles_tokenized, "scholz*", window = 15) %>% mutate(context = str_c(pre, post, sep = " "))
laschet_context <- kwic(corpus_articles_tokenized, "laschet*", window = 15) %>% mutate(context = str_c(pre, post, sep = " "))


# each with window = 5; 15
(
  baerbock_sentiment_15 <- 
    corpus %>% 
    tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T, padding = F) %>% 
    tokens_tolower() %>% 
    tokens_remove(stopwords("de")) %>% 
    tokens_select(valuetype = "regex", "baerbock*", selection = "keep", window = 15, padding = F, verbose = T) %>% 
    
  dfm() %>% 
  dfm_group(source) %>% 
  dfm_lookup(sent.dictionary)
) %>% 
  dfm_weight(scheme = "prop")


(
  laschet_sentiment_15 <- 
    corpus %>% 
    tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T, padding = F) %>% 
    tokens_tolower() %>% 
    tokens_remove(stopwords("de")) %>% 
    tokens_select(valuetype = "regex", "laschet*", selection = "keep", window = 15, padding = F, verbose = T) %>% 
    
    dfm() %>% 
    dfm_group(source) %>% 
    dfm_lookup(sent.dictionary) 
) %>% 
  dfm_weight(scheme = "prop")
  
(
  scholz_sentiment_15 <- 
    corpus %>% 
    tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T, padding = F) %>% 
    tokens_tolower() %>% 
    tokens_remove(stopwords("de")) %>% 
    tokens_select(valuetype = "regex", "scholz*", selection = "keep", window = 15, padding = F, verbose = T) %>% 
    
    dfm() %>% 
    dfm_group(source) %>% 
    dfm_lookup(sent.dictionary) 
) %>% 
  dfm_weight(scheme = "prop")

(
  scholz_sentiment_15_article <- 
    corpus %>% 
    tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T, padding = F) %>% 
    tokens_tolower() %>% 
    tokens_remove(stopwords("de")) %>% 
    tokens_select(valuetype = "regex", "scholz*", selection = "keep", window = 15, padding = F, verbose = T) %>% 
    
    dfm() %>% 
#    dfm_group(source) %>% 
    dfm_lookup(sent.dictionary) 
) %>% 
  dfm_weight(scheme = "prop")


# join sentiment scores to corpus ####
document_sentiment <- 
  articles_classification %>% 
    as_tibble() %>% 
  # join scores
  left_join(., articles_score %>% 
              as_tibble(), 
            by = "doc_id", 
            suffix = c("_classification", "_score")) %>% 
  mutate(sent_score = log((positive_score + 0.5) / (negative_score + 0.5)),
# how to classify neutral articles?
         negative_binary = ifelse(sent_score < 0, 1, 0)) %>% 
  dplyr::filter(., positive_score + negative_score != 0) %>% 
  select(-c(positive_classification, negative_classification, positive_score))



document_sentiments <- 
  articles_score %>% 
  dfm_weight(scheme = "prop") %>% 
  as_tibble() %>% 
    left_join(articles_score %>% as_tibble(),
              by = "doc_id",
              suffix = c("", "_count")) %>% 
    mutate(article_sent = log((positive + 0.5) / (negative + 0.5)),
         # how to classify neutral articles?
         negative_binary = ifelse(article_sent < 0, 1, 0),
         n_classified = positive_count + negative_count) %>% 
    
  # join scores
  left_join(., headlead_article_score %>% 
              dfm_weight(scheme = "prop") %>% 
              as_tibble() %>% 
              left_join(articles_score %>% as_tibble(),
                        by = "doc_id",
                        suffix = c("", "_count")) %>% 
              mutate(article_sent = log((positive + 0.5) / (negative + 0.5)),
                     # how to classify neutral articles?
                     negative_binary = ifelse(article_sent < 0, 1, 0),
                     n_classified = positive_count + negative_count), 
            by = "doc_id", 
            suffix = c("_article", "_headlead")) %>% 
   
  filter(., n_classified_article != 0)
  
  select(-c(positive_classification, negative_classification, positive_score))

  
  # 
  # # join headlead:
  # left_join(., articles_score %>% 
  #             as_tibble(), 
  #           by = "doc_id", 
  #           suffix = c("_classification", "_score")) %>% 
  # mutate(sent_score = log((positive_score + 0.5) / (negative_score + 0.5)),
  #        # how to classify neutral articles?
  #        negative_binary = ifelse(sent_score < 0, 1, 0)) %>% 
  # dplyr::filter(., positive_score + negative_score != 0)




# low number of classified articles before (bc wordstem and negations)

# per article:
all_sources %<>% 
  left_join(., 
            document_sentiment, by = c("link" = "doc_id"))

all_sources_candidates %<>% 
  left_join(., 
            document_sentiment, by = c("link" = "doc_id"))

# per day:

# day_candidate_sentiment <- 
#   source_candidate_sentiment_day %>% 
#   as_tibble() %>%  
#   separate(., doc_id, c("source", "date", "candidate"), sep = "\\.", remove = T) %>% 
#   mutate(date = as.Date(date))

day_sentiment <- 
  headlead_base_sentiment_day %>% 
  as_tibble() %>%  
  separate(., doc_id, c("source", "date"), sep = "\\.", remove = T) %>% 
  mutate(date = as.Date(date)) %>% 
  select(-positive, overall = negative) %>% 

# join candidate specific sentiment of the day:
  left_join(.,
            source_candidate_sentiment_day %>% 
              as_tibble() %>%  
              separate(., doc_id, c("source", "date", "candidate"), sep = "\\.", remove = T) %>% 
              mutate(date = as.Date(date)) %>%
              mutate(negative = ifelse(
                positive + negative == 0, NA, negative
              )) %>% 
              select(-positive) %>% 
              pivot_wider(., 
                          names_from = "candidate", 
                          values_from = "negative"),
            by = c("source", "date"))

day_sentiment_long <- pivot_longer(day_sentiment, -c(date, source), names_to = "candidate")

# plot time-series:
week_candidate_sentiment
  source_candidate_sentiment_week %>%
  as_tibble() %>%
  separate(., doc_id, c("source", "week", "candidate"), sep = "\\.", remove = T) %>% 
  mutate(negative = ifelse(
    positive + negative == 0, NA, negative
  )) %>% 
  select(-positive)


 ggplot(aes(x = date, y = value), data = day_sentiment_long) +

#gg_leading_party_background +
  #ggnewscale::new_scale_fill() +
  #layers$candidatecolors + 
  geom_point() + 
   geom_smooth(se = F) +
  facet_grid(rows = vars(source), cols = vars(candidate)) +
  theme_classic() +
  geom_hline(yintercept = .5) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  coord_cartesian(ylim = c(0, 1))
  
# overall is only about headlines --> compare to only headlines of candidates
 
# plot it:
ggplot() +
  geom_line(aes(x = date, y = negative, colour = source), data = day_sentiment)

  
# get average sentiments -> plot average in time-series?
headlead_base_sentiment


source_base_sentiment %>% 
  as_tibble() %>% 
  ggplot() +
  geom_point(aes(x = doc_id, y = positive), size = 5, shape = 6) + # more ink: geom_col
  geom_hline(yintercept = .5, linetype = 5)
  
  
base_sentiments <- left_join(headlead_base_sentiment %>% as_tibble(), 
                             source_base_sentiment %>% as_tibble(), 
                             by = "doc_id",
                             suffix = c("_header", "_fulltext")) %>% 
  left_join(., 
            source_candidates_sentiment %>% as_tibble(),
            by = "doc_id") %>% 
  rename("positive_candidates" = "positive",
         "negative_candidates" = "negative")

# beware not to mix candidate-related sentiment with general sentiment!
base_sentiments %>% 
  ggplot() +
  geom_point(aes(x = doc_id, y = positive_candidates), size = 5, shape = 6) + # more ink: geom_col
  geom_point(aes(x = doc_id, y = positive_header), size = 5, shape = 2) +
  geom_pointrange(aes(x = doc_id, y = positive_fulltext, ymin = positive_header, ymax = positive_candidates)) +
  geom_hline(yintercept = .5, linetype = 5)
            

# plot frequency of negative articles:
# frequency:
all_sources_candidates %>% 
  dplyr::filter(., negative_binary == 0) %>% 
  
  ggplot() +
#  geom_rect(data = rects_nogaps, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = list("gray40", "greenyellow", "gray40", "lightcoral"), alpha = .01), show.legend = F) +
#layers$background_gaps +
#    ggnewscale::new_scale_fill() + 
  layers$candidatecolors +
  geom_freqpoly(aes(x = as.Date(date), group = str_to_title(candidate), colour = str_to_title(candidate))) +  # or geom_density for share? -> filtered data: only share of all negative articles
  layers$scalex +
  labs(x = "Date", y = "N Negative Articles", colour = "Candidate") +
  facet_grid(rows = vars(source))
  

# share (of all negative articles):
all_sources_candidates %>% 
  dplyr::filter(., negative_binary == 0) %>% 
  
  ggplot() +
  #  geom_rect(data = rects_nogaps, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = list("gray40", "greenyellow", "gray40", "lightcoral"), alpha = .01), show.legend = F) +
  #layers$background_gaps +
  #    ggnewscale::new_scale_fill() + 
  layers$candidatecolors +
  geom_density(aes(x = as.Date(date), group = str_to_title(candidate), colour = str_to_title(candidate))) +  # or geom_density for share? -> filtered data: only share of all negative articles
  layers$scalex +
  labs(x = "Date", y = "Share of all negative articles", colour = "Candidate") +
  facet_grid(rows = vars(source)) + 
  guides(fill = F)



# wordcount (after tokenization)

tokencount <- c()
for (i in 1:length(corpus_articles_tokenized)) {
  tokencount[i] <- length(corpus_articles_tokenized[[i]])
}

all_sources <- bind_cols(all_sources, "tokencount" = tokencount) %>% 
  mutate(tokencount = ifelse(is.na(fulltext), NA, tokencount))

# source_candidate_sentiment_day 
