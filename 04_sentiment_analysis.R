# 04 sentiment analysis

library(pacman)
p_load(quanteda,
#       quanteda.textstats, # to calculate wordfrequencies/Similarity/Readability
#       quanteda.textplots, # for keyword/network/wordcloud graphs
       tidyverse,
       lubridate,
       magrittr)

save(list = c("corpus", "corpus_candidates", "RT_corpus", "RT_corpus_candidate", "factiva_corpus", "factiva_corpus_candidate", "corpus_tokenized_source"), 
     file = "analyses/corpora.RData")
save(list = c("articles_classification", "source_candidate_sentiment"), 
     file = "analyses/sentiment_results.RData")

load("analyses/corpora.RData")

# load dictionaries: ####
# political dictionary:
load("analyses/Rauh_SentDictionaryGerman.RData")
# sent.dictionary with 37080 entries

# convert to dictionary object
sentiment.lexikon.rauh <- dictionary(list(positive = str_trim(sent.dictionary$feature[sent.dictionary$sentiment>0]), negative = str_trim(sent.dictionary$feature[sent.dictionary$sentiment<0])))

# media dictionary:
sentiWS_neg <- read_delim("analyses/SentiWS_v2.0_Negative.txt", col_names = c("word", "polarity", "variants"))




# build full corpus: ####
RT <- rio::import("articles/RT_corpus.xlsx")
Factiva <- rio::import("articles/corpus_factiva.xlsx")

all_sources <- bind_rows(RT, select(Factiva, names(RT))) 
corpus <- corpus(all_sources, 
                 docid_field = "link", 
                 text_field = "fulltext", 
                 meta = list("resort", "date", "candidate", "mentions_baerbock", "mentions_scholz", "mentions_laschet", "wordcount", "source"),
                 unique_docnames = F
                 )

# analyse: ####

# by candidate/date
corpus_candidates <- c(RT_corpus_candidate, factiva_corpus_candidate)

corpus_candidates_tokenized_period <- corpus_candidates %>%
  corpus_group(interaction(source, week(date), candidate), fill = T) %>%
  tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("de")) %>% 
  # check if this influences sentiment analysis
  tokens_wordstem("de")

# by candidate:
corpus_candidates_tokenized_period <- corpus_candidates %>%
  corpus_group(candidate) %>%
  tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("de")) %>% 
  # check if this influences sentiment analysis
  tokens_wordstem("de")

# by candidate/source
corpus_candidates_tokenized_period <- corpus_candidates %>%
  corpus_group(interaction(source, candidate)) %>%
  tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("de")) %>% 
  # check if this influences sentiment analysis
  tokens_wordstem("de")



# by source:
corpus_candidates_tokenized_source <- corpus_candidates %>%
  corpus_group(source) %>%
  tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("de")) %>% 
  # check if this influences sentiment analysis
  tokens_wordstem("de")

corpus_tokenized_source <- corpus %>%
  corpus_group(source) %>%
  tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("de")) %>% 
  # check if this influences sentiment analysis
  tokens_wordstem("de")

# base sentiment:
  # for RT: general sentiment, for Rest: general candidate sentimnent
source_base_sentiment <- 
  corpus_tokenized_source %>% 
  dfm() %>% 
  dfm_lookup(., sentiment.lexikon.rauh) %>% 
  dfm_weight(., scheme = "prop")

  # additionally: 
    # run for headlines/lead?
all_sources %<>% mutate(
  head_lead = str_c(header, lead)
)

headlead_tokenized_source <- 
  corpus(all_sources, 
         docid_field = "link", 
         text_field = "head_lead", 
         meta = list("resort", "date", "candidate", "mentions_baerbock", "mentions_scholz", "mentions_laschet", "wordcount", "source"),
         unique_docnames = T
)
headlead_tokenized_source %<>% 
  corpus_group(source) %>%
  tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("de")) %>% 
  tokens_wordstem("de")

headlead_base_sentiment <- 
  headlead_tokenized_source %>% 
  dfm() %>% 
  dfm_lookup(., sentiment.lexikon.rauh) %>% 
  dfm_weight(., scheme = "prop")


# additionally:
    # run for candidate articles?
(source_candidate_sentiment <- 
  corpus_candidates_tokenized_source %>% 
  dfm() %>% 
  dfm_lookup(., sentiment.lexikon.rauh) %>% 
  dfm_weight(., scheme = "prop")
)


# by source/date:
corpus_tokenized <- corpus %>%
  corpus_group(interaction(source, week(date)), fill = T) %>%
  tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("de")) %>% 
  # check if this influences sentiment analysis
  tokens_wordstem("de")


corpus_dfm <- dfm(corpus_tokenized)
corpus_sentiment <- dfm(corpus_tokenized, dictionary = sentiment.lexikon.rauh)
dfm_weight(corpus_dfm %>% dfm_lookup(., sentiment.lexikon.rauh), scheme = "prop")


# by article -> score and binary 
corpus_articles_tokenized <- corpus %>%
  tokens(., remove_punct = T, remove_numbers = T, remove_symbols = T, remove_url = T) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("de")) %>% 
  # check if this influences sentiment analysis
  tokens_wordstem("de")

articles_classification <- 
  corpus_articles_tokenized %>% 
  dfm() %>% 
  dfm_lookup(., sentiment.lexikon.rauh) %>% 
  dfm_weight(., scheme = "boolean")

# ToDo:
# graph
# variations



# tokenize ####
# grouped by resort:
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



# documentfeature matrix
sentiment_resorts <- dfm(tokens_RT_resort, dictionary = sentiment.lexikon.rauh)
sentiment_candidates <- dfm(tokens_RT_candidate, dictionary = sentiment.lexikon.rauh)

# use wordcounts
dfm_trim(sentiment_resorts, min_termfreq = 5)
topfeatures(sentiment_resorts, 20)


dfm_weight(sentiment_resorts, scheme = "prop")


corpus_RT_test <- corpus(RT_corpus_all, docid_field = "id", text_field = "text.x", meta = list("resort"), unique_docnames = F)

summary(corpus_RT)

toks_article <- corpus_RT %>% dfm_lookup(sentiment.lexikon.rauh) %>% tokens()
article_sentiment <- dfm(toks_article, dictionary = sentiment.lexikon.rauh) 
article_sentiment_prop <- dfm_weight(article_sentiment, scheme = "prop")


# wordcount -> types/tokens/sentences

# plotting frequencies
feature_frequencies_candidates <- article_sentiment %>% textstat_frequency(n = 10, group = candidate)
feature_frequencies_candidates %>%
  mutate(candidate = factor(group)) %>%
  ggplot(aes(x = reorder(feature, frequency), y = frequency, fill = candidate)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "share of words per candidate") +
  facet_wrap(~candidate, ncol = 2, scales = "free") +
  coord_flip()

# targeted sentiment analysis: ####

# in RT only yet -> use full corpus!
# candidates in context:
baerbock_context <- kwic(toks_article, "baerbock", window = 10)
scholz_context <- kwic(toks_article, "scholz", window = 10)
laschet_context <- kwic(toks_article, "laschet", window = 10)



(
  baerbock_sentiment <- 
  RT_corpus %>% 
    tokens() %>% 
    tokens_select("baerbock", selection = "keep", window = 20, padding = F, verbose = T) %>% 
    
  dfm() %>% 
  dfm_lookup(sentiment.lexikon.rauh) %>% 
  dfm_group(source)
)

(
  laschet_sentiment <- 
    RT_corpus %>% 
    tokens() %>% 
    tokens_select("laschet", selection = "keep", window = 20, padding = F, verbose = T) %>% 
    
    dfm() %>% 
    dfm_lookup(sentiment.lexikon.rauh) %>% 
    dfm_group(source)
) 
  
(
  scholz_sentiment <- 
    RT_corpus %>% 
    tokens() %>% 
    tokens_select("scholz", selection = "keep", window = 20, padding = F, verbose = T) %>% 
    
    dfm() %>% 
    dfm_lookup(sentiment.lexikon.rauh) %>% 
    dfm_group(source)
)