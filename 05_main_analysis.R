# 05 main anaalysis

library(pacman)
p_load(tidyverse, ggnewscale, collapse, magrittr, lubridate)

load("analyses/sentiment_results.RData")
load("analyses/corpora.RData")
load("polls/all_polls.RData")


events = tibble(event = c("nomination_gru", "nomination_cdu", "plagiarism", "INSM/pädophilie", "#LaschetLacht", "#GrünerMist", "talks", "foreign_minister", "office"),
                 date = as.Date(c("2021-04-19", "2021-04-20", "2021-05-11", "2021-06-10", "2021-07-17", "2021-08-11", "2021-10-22", "2021-11-24", "2021-12-08")),
                 candidate = c("baerbock", "laschet", "baerbock", "baerbock", "laschet", "baerbock", "scholz", "baerbock", "scholz"),
                party = c("gru", "cdu", "gru", "gru", "cdu", "gru", "spd", "gru", "spd")
)


threshold = .75

analysis_data <- 
  all_sources %>% 
  dplyr::select(doc_id = link, date, source, wordcount, tokencount, baerbock, mentions_baerbock, laschet, mentions_laschet, scholz, mentions_scholz) %>% 
  mutate(date = as.Date(date)) %>%  
  # join polls
  left_join(., 
            polls_interpolated_wide %>% 
              dplyr::select(date, starts_with("share"), rank_cdu, rank_spd, rank_gru) %>% 
            mutate(cdu_gru = share_cdu - share_gru,
                   cdu_spd = share_cdu - share_spd,
                   spd_gru = share_spd - share_gru), 
            by = "date") %>% 
  # join sentiment results
  left_join(., 
            articles_score %>% 
              dfm_weight(scheme = "prop") %>% 
              as_tibble() %>% 
              left_join(., articles_score %>% as_tibble(),
                        by = "doc_id",
                        suffix = c("", "_count")) %>% 
              mutate(article_sent = log((positive + 0.5) / (negative + 0.5)),
                     # how to classify neutral articles?
                     negative_binary = ifelse(article_sent < 0, 1, 0),
                     n_classified = positive_count + negative_count) %>% 
              filter(., n_classified != 0),
            by = "doc_id") %>% 
    left_join(.,
              headlead_article_score %>% 
                dfm_weight(scheme = "prop") %>% 
                as_tibble() %>% 
                left_join(., headlead_article_score %>% as_tibble(),
                          by = "doc_id",
                          suffix = c("", "_count")) %>% 
                mutate(article_sent = log((positive + 0.5) / (negative + 0.5)),
                       # how to classify neutral articles?
                       negative_binary = ifelse(article_sent < 0, 1, 0),
                       n_classified = positive_count + negative_count) %>% 
                filter(., n_classified != 0), 
              by = "doc_id", 
              suffix = c("_article", "_headlead")) %>% 

mutate(laschet = ifelse(laschet == 0 & mentions_laschet != 0, 1, laschet)) %>% 
mutate(scholz = ifelse(scholz == 0 & mentions_scholz != 0, 1, scholz)) %>% 
mutate(baerbock = ifelse(baerbock == 0 & mentions_baerbock != 0, 1, baerbock)) %>%   

  mutate(baerbock = ifelse(is.na(baerbock), 0, baerbock)) %>% 
mutate(scholz = ifelse(is.na(scholz), 0, scholz)) %>% 
mutate(laschet = ifelse(is.na(laschet), 0, laschet)) %>% 
  mutate(    
         share_classified = n_classified_article / wordcount,
         share_classified_toks = n_classified_article / tokencount,
         
         negative_binary_strong_article = ifelse(negative_article >= threshold, 1, 0),
         negative_binary_strong_headlead = ifelse(negative_headlead >= threshold, 1, 0),
         negative_binary_headlead = ifelse(negative_headlead > .5, 1, 0),
         
         negative_baerbock = ifelse(baerbock == 1 & negative_binary_article == 1, 1, 0),
         negative_scholz = ifelse(scholz == 1 & negative_binary_article == 1, 1, 0),
         negative_laschet = ifelse(laschet == 1 & negative_binary_article == 1, 1, 0),
         
         negative_strong_baerbock = ifelse(baerbock == 1 & negative_binary_strong_article == 1, 1, 0),
         negative_strong_scholz = ifelse(scholz == 1 & negative_binary_strong_article == 1, 1, 0),
         negative_strong_laschet = ifelse(laschet == 1 & negative_binary_strong_article == 1, 1, 0),
         
         negative_strong_baerbock = ifelse(baerbock == 1 & negative_binary_strong_headlead == 1, 1, 0),
         negative_strong_scholz = ifelse(scholz == 1 & negative_binary_strong_headlead == 1, 1, 0),
         negative_strong_laschet = ifelse(laschet == 1 & negative_binary_strong_headlead == 1, 1, 0),
         
         negative_strong_baerbock_headlead = ifelse(baerbock == 1 & negative_binary_strong_headlead == 1, 1, 0),
         negative_strong_scholz_headlead = ifelse(scholz == 1 & negative_binary_strong_headlead == 1, 1, 0),
         negative_strong_laschet_headlead = ifelse(laschet == 1 & negative_binary_strong_headlead == 1, 1, 0),
         
         candidate = ifelse(scholz + laschet + baerbock > 0, 1, 0),
         RT = ifelse(source == "RTDE", 1, 0), 
         source_bin = ifelse(RT == 1, "RT", "german media"),
         
         rank_cdu = ifelse(is.na(rank_cdu) & date < max(period1_gaps), 1, rank_cdu),
         rank_gru = ifelse(is.na(rank_gru) & date < max(period1_gaps), 2, rank_gru),
         rank_spd = ifelse(is.na(rank_spd) & date < max(period1_gaps), 3, rank_spd),

         rank_cdu = ifelse(is.na(rank_cdu) & date > max(period4_gaps-2), 2, rank_cdu),
         rank_gru = ifelse(is.na(rank_gru) & date > max(period4_gaps-2), 3, rank_gru),
         rank_spd = ifelse(is.na(rank_spd) & date > max(period4_gaps-2), 1, rank_spd),


         period1_clear = ifelse(between(date, period1_gaps[1], period1_nogaps[2]), 1, 0),
         period2_clear = ifelse(between(date, period2_gaps[1], period2_nogaps[2]), 1, 0),
         period3_clear = ifelse(between(date, period3_gaps[1], period3_nogaps[2]), 1, 0),
         period4_clear = ifelse(between(date, period4_gaps[1], period4_nogaps[2]), 1, 0),

         period = case_when(between(date, -Inf, period1_nogaps[2]) ~ "before",
                            between(date, period2_gaps[1], period2_nogaps[2]) ~ "green_period",
                            between(date, period3_gaps[1], period3_nogaps[2]) ~ "black_period",
                            between(date, period4_gaps[1], period4_nogaps[2]) ~ "red_period",
                            between(date, period4_gaps[2], Inf) ~ "after",
                            TRUE ~ "interim"),
         
         leader = case_when(rank_cdu == 1 ~ "cdu",
                            rank_spd == 1 ~ "spd",
                            rank_gru == 1 ~ "gru"),
         
         
         # period1_just = ifelse(between(date, period1_nogaps[1], period1_nogaps[2]), 1, 0),
         # period2_just = ifelse(between(date, period2_nogaps[1], period1_nogaps[2]), 1, 0),
         # period3_just = ifelse(between(date, period3_nogaps[1], period2_nogaps[2]), 1, 0),
         # period4_just = ifelse(between(date, period4_nogaps[1], period4_nogaps[2]), 1, 0),

         negative_leader = case_when(rank_cdu == 1 ~ negative_laschet,
                                     rank_gru == 1 ~ negative_baerbock,
                                     rank_spd == 1 ~ negative_scholz),
        
         mentions_leader = case_when(rank_cdu == 1 ~ mentions_laschet,
                                      rank_gru == 1 ~ mentions_baerbock,
                                      rank_spd == 1 ~ mentions_scholz),
         
         negative_middle = case_when(rank_cdu == 2 ~ negative_laschet,
                                     rank_gru == 2 ~ negative_baerbock,
                                     rank_spd == 2 ~ negative_scholz),
         
         mentions_middle = case_when(rank_cdu == 2 ~ mentions_laschet,
                                     rank_gru == 2 ~ mentions_baerbock,
                                     rank_spd == 2 ~ mentions_scholz),

         negative_last = case_when(rank_cdu == 3 ~ negative_laschet,
                                     rank_gru == 3 ~ negative_baerbock,
                                     rank_spd == 3 ~ negative_scholz),
         
         mentions_last = case_when(rank_cdu == 3 ~ mentions_laschet,
                                     rank_gru == 3 ~ mentions_baerbock,
                                     rank_spd == 3 ~ mentions_scholz),
         
         negative_nonleader = (negative_middle + negative_last),
         mentions_nonleader = (mentions_middle + mentions_last),
         
         leader_focus_difference_mentions = mentions_leader - mentions_nonleader,
         leader_focus_difference_negative = negative_leader - negative_nonleader,
         
# beware of the +1!
         leader_focus_ratio_mentions = (mentions_leader) / (mentions_nonleader+1),
         leader_focus_ratio_negative = (negative_leader) / (negative_nonleader+1),
         
         none = ifelse(candidate == 0, 1, 0),

         sent_score_baerbock = ifelse(baerbock == 1, article_sent_article, NA),
         sent_score_laschet = ifelse(laschet == 1, article_sent_article, NA),        
         sent_score_scholz = ifelse(scholz == 1, article_sent_article, NA),     
        negativity_baerbock = ifelse(baerbock == 1, negative_article, NA),
        negativity_laschet = ifelse(laschet == 1, negative_article, NA),        
        negativity_scholz = ifelse(scholz == 1, negative_article, NA),

        sent_score_baerbock = ifelse(baerbock == 1, article_sent_headlead, NA),
        sent_score_laschet = ifelse(laschet == 1, article_sent_headlead, NA),        
        sent_score_scholz = ifelse(scholz == 1, article_sent_headlead, NA),     
        negativity_baerbock = ifelse(baerbock == 1, negative_headlead, NA),
        negativity_laschet = ifelse(laschet == 1, negative_headlead, NA),        
        negativity_scholz = ifelse(scholz == 1, negative_headlead, NA)

         ) %>% 
  filter(year(date) == 2021)
  

analysis_candidate_data_long <- analysis_data %>% 
#  filter(candidate == 1) %>% 
  mutate(week = week(date)) %>% 
  pivot_longer(., 
    cols = c("scholz", "baerbock", "laschet", "none"), 
    names_to = "candidatename",
    values_to = "N") %>% 
  filter(N == 1) %>% 
  mutate(
    mentions = case_when(
      candidatename == "baerbock" ~ mentions_baerbock,
      candidatename == "scholz" ~ mentions_scholz,
      candidatename == "laschet" ~ mentions_laschet,
      candidatename == "none" ~ 0
      ),
    
    negative_mentions = case_when(
      candidatename == "baerbock" ~ negative_baerbock,
      candidatename == "scholz" ~ negative_scholz,
      candidatename == "laschet" ~ negative_laschet,
      candidatename == "none" ~ 0
    )) %>% 
  select(-c(mentions_baerbock, mentions_laschet, mentions_scholz, negative_baerbock, negative_laschet, negative_scholz, RT, candidate)) %>% 
  select(doc_id, date, source_bin, candidatename,source,  period, leader, N, starts_with("n_"), contains("count"), contains("binary"), contains("mention"), everything())

filler <- expand(analysis_candidate_data_long, date, source_bin, candidatename) %>% 
  left_join(., 
            polls_interpolated_wide %>% 
              dplyr::select(date, starts_with("share"), rank_cdu, rank_spd, rank_gru) %>% 
              mutate(cdu_gru = share_cdu - share_gru,
                     cdu_spd = share_cdu - share_spd,
                     spd_gru = share_spd - share_gru), 
            by = "date") %>%
  mutate(
    rank_cdu = ifelse(is.na(rank_cdu) & date < max(period1_gaps), 1, rank_cdu),
    rank_gru = ifelse(is.na(rank_gru) & date < max(period1_gaps), 2, rank_gru),
    rank_spd = ifelse(is.na(rank_spd) & date < max(period1_gaps), 3, rank_spd),
    
    rank_cdu = ifelse(is.na(rank_cdu) & date > max(period4_gaps-2), 2, rank_cdu),
    rank_gru = ifelse(is.na(rank_gru) & date > max(period4_gaps-2), 3, rank_gru),
    rank_spd = ifelse(is.na(rank_spd) & date > max(period4_gaps-2), 1, rank_spd),
    
    period = case_when(between(date, -Inf, period1_nogaps[2]) ~ "before",
                       between(date, period2_gaps[1], period2_nogaps[2]) ~ "green_period",
                       between(date, period3_gaps[1], period3_nogaps[2]) ~ "black_period",
                       between(date, period4_gaps[1], period4_nogaps[2]) ~ "red_period",
                       between(date, period4_gaps[2], Inf) ~ "after",
                       TRUE ~ "interim"),
    
    leader = case_when(rank_cdu == 1 ~ "cdu",
                       rank_spd == 1 ~ "spd",
                       rank_gru == 1 ~ "gru"),
    week = week(date)
  )

analysis_candidate_data_long %<>% bind_rows(., filler) %>% distinct()


# by day but not by candidate:
byday_all <- collapse::collap(analysis_data %>% mutate(articles_per_day = 1) %>% 
                                select(doc_id, date, source_bin, source,  period, leader, starts_with("n_"), contains("count"), contains("binary"), contains("mention"), everything()), 
                          ~ date + source, 
                          custom = list(fmean = 7:71, fsum = 72)
) %>% 
  mutate(overall_sent = log((positive_count_article/(positive_count_article + negative_count_article) + 0.5) / 
                              (negative_count_article/(positive_count_article + negative_count_article) + 0.5)),
         source_bin = ifelse(RT == 1, "RT", "german media"),
         period = case_when(between(date, -Inf, period1_nogaps[2]) ~ "before",
                                        between(date, period2_gaps[1], period2_nogaps[2]) ~ "green_period",
                                        between(date, period3_gaps[1], period3_nogaps[2]) ~ "black_period",
                                        between(date, period4_gaps[1], period4_nogaps[2]) ~ "red_period",
                                        between(date, period4_gaps[2], Inf) ~ "after",
                                        TRUE ~ "interim")
         # ,
         # across(contains("strong_baerbock"), ~ ifelse(candidatename == "baerbock", .x, NA)),
         # across(contains("strong_laschet"), ~ ifelse(candidatename == "laschet", .x, NA)),
         # across(contains("strong_scholz"), ~ ifelse(candidatename == "scholz", .x, NA))
         ) %>% 
      select(date = date, source = source, articles_per_day = articles_per_day,
             everything()) %>%  
  rename(
             "related_baerbock" = "baerbock", 
             "negative-strong_baerbock" = "negative_strong_baerbock", 
             "negative-strong-headlead_baerbock" = "negative_strong_baerbock_headlead",
             "related_laschet" = "laschet", 
             "negative-strong_laschet" = "negative_strong_laschet", 
             "negative-strong-headlead_laschet" = "negative_strong_laschet_headlead",
             "related_scholz" = "scholz", 
             "negative-strong_scholz" = "negative_strong_scholz", 
             "negative-strong-headlead_scholz" = "negative_strong_scholz_headlead",
             "sent-score_scholz" = "sent_score_scholz",
             "sent-score_baerbock" = "sent_score_baerbock",
             "sent-score_laschet" = "sent_score_laschet"
)

byday_all_long <- byday_all %>% select(-candidate) %>% 
    pivot_longer(cols = contains(c("baerbock", "laschet", "scholz")), 
                 names_sep = "_", names_to = c("variable", "candidate"))
  
(articles_per_day <- byday_all %>% group_by(source_bin, date) %>% summarize(articles_per_day_bin = sum(articles_per_day)) %>% ungroup())

# aggregate ####
byday <- collapse::collap(analysis_candidate_data_long, 
                                 ~ date + source_bin + candidatename, 
                          custom = list(
                            fmean = 29:45, fmean = 56:58,
                                        fsum = 8:28, fsum = 46:55)
                                 ) %>% 
  mutate(N = ifelse(is.na(N), 0, N),
         overall_sent = log((positive_count_article/(positive_count_article + negative_count_article) + 0.5) / 
                              (negative_count_article/(positive_count_article + negative_count_article) + 0.5))
         ) %>% 
  left_join(., byday_all %>% group_by(source_bin, date) %>% summarize(articles_per_day_bin = sum(articles_per_day)) %>% ungroup(), 
            by = c("source_bin", "date")) %>% left_join(., byday %>% mutate(across(contains(c("negative_strong", "negative_binary")), .fns = ~ .x / articles_per_day_bin)) %>% 
              select("source_bin", "date", "candidatename", contains(c("negative_strong", "negative_binary"))),
            by = c("source_bin", "date", "candidatename"),
            suffix = c("", "_relative")
            ) %>% 
  mutate(N_relative = N /articles_per_day_bin)

# join period again?

(
diff_by_day <- 
 bind_cols(
  byday %>% filter(source_bin == "RT") %>%  
    select(date, candidatename), 
  (byday %>% filter(source_bin == "RT") %>% 
     mutate(across(.cols = c(5:24, 42:51), .fns = ~ ifelse(N == 0, 0, .x))) %>% 
     select(-c(date, source_bin, candidatename), contains("spd"), contains("cdu"), contains("gru")) %>% 
       as.matrix() - 
     byday %>% filter(source_bin != "RT") %>% 
        mutate(across(.cols = c(5:24, 42:51), .fns = ~ ifelse(N == 0, 0, .x))) %>% 
        select(-c(date, source_bin, candidatename), contains("spd"), contains("cdu"), contains("gru")) %>% 
     as.matrix()
   ) %>% as_tibble()
 ) %>%  mutate(period = case_when(between(date, -Inf, period1_nogaps[2]) ~ "before",
                      between(date, period2_gaps[1], period2_nogaps[2]) ~ "green_period",
                      between(date, period3_gaps[1], period3_nogaps[2]) ~ "black_period",
                      between(date, period4_gaps[1], period4_nogaps[2]) ~ "red_period",
                      between(date, period4_gaps[2], Inf) ~ "after",
                      TRUE ~ "interim"),
               across(contains("strong_baerbock"), ~ ifelse(candidatename == "baerbock", .x, NA)),
               across(contains("strong_laschet"), ~ ifelse(candidatename == "laschet", .x, NA)),
               across(contains("strong_scholz"), ~ ifelse(candidatename == "scholz", .x, NA))
 ) %>% 
    select(-contains(c("cdu", "spd", "gru"))) %>% 
    bind_cols(., byday %>% 
            filter(source_bin == "RT") %>% 
            select(contains(c("cdu", "spd", "gru"))))
  
) 

byweek <- collapse::collap(analysis_candidate_data_long %>% mutate(day = as.integer(date)) %>% select(-date), 
                          ~ week + source_bin + candidatename, 
                          custom = list(fmean = 29:52,
                                        fsum = 7:28)
                                 )[-48] 
byweek %<>% mutate(N = ifelse(is.na(N), 0, N),
                              overall_sent = log((positive_count_article / (positive_count_article + negative_count_article) + 0.5) / 
                                                   (negative_count_article / (positive_count_article + negative_count_article) + 0.5))) %>% 
  select(weekno = week, everything()) 

(
  diff_by_week <- 
    bind_cols(
      byweek %>% filter(source_bin == "RT") %>% select(weekno, candidatename), 
      (byweek %>% filter(source_bin == "RT") %>% 
         mutate(across(.cols = 5:24, .fns = ~ ifelse(N == 0, 0, .x))) %>% 
         select(-c(source_bin, candidatename, weekno, contains("spd"), contains("cdu"), contains("gru"))) %>% 
        as.matrix() - 
         (byweek %>% filter(source_bin != "RT") %>% 
            mutate(across(.cols = 5:24, .fns = ~ ifelse(N == 0, 0, .x))) %>% 
            select(-c(source_bin, candidatename, weekno, contains("spd"), contains("cdu"), contains("gru"))) %>% 
            as.matrix())
      ) %>% as_tibble()) %>% mutate(period = case_when(between(weekno, -Inf, week(period1_nogaps[2])) ~ "before",
                                                      between(weekno, week(period2_gaps[1]), week(period2_nogaps[2])) ~ "green_period",
                                                      between(weekno, week(period3_gaps[1]), week(period3_nogaps[2])) ~ "black_period",
                                                      between(weekno, week(period4_gaps[1]), week(period4_nogaps[2])) ~ "red_period",
                                                      between(weekno, week(period4_gaps[2]), Inf) ~ "after",
                                                      TRUE ~ "interim"),
                                    across(contains("strong_baerbock"), ~ ifelse(candidatename == "baerbock", .x, NA)),
                                    across(contains("strong_laschet"), ~ ifelse(candidatename == "laschet", .x, NA)),
                                    across(contains("strong_scholz"), ~ ifelse(candidatename == "scholz", .x, NA))
                                  ) %>% mutate( across(.cols = 24:37, 39, .fns = ~ ifelse(rowSums(x = diff_by_week %>% select(3:23)) == 0,
                                                                                          NaN, .x)))
    )


# compare to individual source: ####
# byweek_source <- collapse::collap(analysis_candidate_data_long, 
#                            ~ week + source + candidatename, 
#                            custom = list(fmean = 29:52,
#                                          fsum = 8:28)
# )[-48] 
# byweek_source %<>% mutate(N = ifelse(is.na(N), 0, N)) %>% 
#   select(weekno = week, everything())
# 
# (
#   diff_by_week_RT_BILD <- 
#     bind_cols(
#       byweek_source %>% filter(source == "RTDE") %>% select(weekno, candidatename), 
#       (byweek_source %>% filter(source == "RTDE") %>% select(-c(source, candidatename, weekno, contains("spd"), contains("cdu"), contains("gru"))) %>% replace(is.na(.), 0) %>% as.matrix() - 
#          byweek_source %>% filter(source == "BILD") %>% select(-c(source, candidatename, weekno, contains("spd"), contains("cdu"), contains("gru"))) %>% replace(is.na(.), 0) %>% as.matrix()
#       ) %>% as_tibble() 
#     )
# )

# without BILD ####
byday_nobild <- collapse::collap(analysis_candidate_data_long, 
                          ~ date + source_bin + candidatename, 
                          custom = list(
                            fmean = 29:45, fmean = 56:58,
                            fsum = 8:28, fsum = 46:55)
) %>% 
  mutate(N = ifelse(is.na(N), 0, N),
         overall_sent = log((positive_count_article/(positive_count_article + negative_count_article) + 0.5) / 
                              (negative_count_article/(positive_count_article + negative_count_article) + 0.5))
  )

# join period again?

(
  diff_by_day_nobild <- 
    bind_cols(
      byday_nobild %>% filter(source_bin == "RT") %>% 
        select(date, candidatename), 
      (byday_nobild %>% filter(source_bin == "RT") %>% 
         mutate(across(.cols = 5:24, .fns = ~ ifelse(N == 0, 0, .x))) %>% 
         select(-c(date, source_bin, candidatename), contains("spd"), contains("cdu"), contains("gru")) %>% 
         as.matrix() - 
         byday_nobild %>% filter(source_bin != "RT") %>% 
         mutate(across(.cols = 5:24, .fns = ~ ifelse(N == 0, 0, .x))) %>% 
         select(-c(date, source_bin, candidatename), contains("spd"), contains("cdu"), contains("gru")) %>% 
         as.matrix()
      ) %>% as_tibble()
    ) %>%  mutate(period = case_when(between(date, -Inf, period1_nogaps[2]) ~ "before",
                                     between(date, period2_gaps[1], period2_nogaps[2]) ~ "green_period",
                                     between(date, period3_gaps[1], period3_nogaps[2]) ~ "black_period",
                                     between(date, period4_gaps[1], period4_nogaps[2]) ~ "red_period",
                                     between(date, period4_gaps[2], Inf) ~ "after",
                                     TRUE ~ "interim"),
                  across(contains("strong_baerbock"), ~ ifelse(candidatename == "baerbock", .x, NA)),
                  across(contains("strong_laschet"), ~ ifelse(candidatename == "laschet", .x, NA)),
                  across(contains("strong_scholz"), ~ ifelse(candidatename == "scholz", .x, NA))
    )
) 

# UPDATE IF NEEDED:
# byweek_nobild <- collapse::collap(analysis_candidate_data_long %>% filter(source != "BILD"), 
#                            ~ week + source_bin + candidatename, 
#                            custom = list(fmean = 29:52,
#                                          fsum = 8:28)
# )[-48] 
# byweek_nobild %>% mutate(N = ifelse(is.na(N), 0, N)) %>% 
#   select(weekno = week, everything())
# 
# (
#   diff_by_week_nobild <- 
#     bind_cols(
#       byweek %>% filter(source_bin == "RT") %>% select(weekno, candidatename), 
#       (byweek %>% filter(source_bin == "RT") %>% select(-c(source_bin, candidatename, weekno, contains("spd"), contains("cdu"), contains("gru"))) %>% replace(is.na(.), 0) %>% as.matrix() - 
#          (byweek %>% filter(source_bin != "RT") %>% select(-c(source_bin, candidatename, weekno, contains("spd"), contains("cdu"), contains("gru"))) %>% replace(is.na(.), 0) %>% as.matrix())/4
#       ) %>% as_tibble()) %>% mutate(period = case_when(between(weekno, -Inf, week(period1_nogaps[2])) ~ "before",
#                                                        between(weekno, week(period2_gaps[1]), week(period2_nogaps[2])) ~ "green_period",
#                                                        between(weekno, week(period3_gaps[1]), week(period3_nogaps[2])) ~ "black_period",
#                                                        between(weekno, week(period4_gaps[1]), week(period4_nogaps[2])) ~ "red_period",
#                                                        between(weekno, week(period4_gaps[2]), Inf) ~ "after",
#                                                        TRUE ~ "interim"),
#                                     overall_sent = log((positive_count_article/(positive_count_article + negative_count_article) + 0.5) / 
#                                                          (negative_count_article/(positive_count_article + negative_count_article) + 0.5))
#       )
# )

# plots using analysis data:
ggplot() +
  geom_rect(aes(ymin = -Inf, 
                ymax = Inf, 
                xmin = rects_gaps$xmin[1], 
                xmax = rects_gaps$xmax[1]), 
            fill = "gray40", 
            alpha = .1) +
  geom_rect(aes(ymin = -Inf, 
                ymax = Inf, 
                xmin = rects_gaps$xmin[2], 
                xmax = rects_gaps$xmax[2]), 
            fill = "greenyellow", 
            alpha = .1) +
  geom_rect(aes(ymin = -Inf, 
                ymax = Inf, 
                xmin = rects_gaps$xmin[3], 
                xmax = rects_gaps$xmax[3]), 
            fill = "gray40", 
            alpha = .1) +
  geom_rect(aes(ymin = -Inf, 
                ymax = Inf, 
                xmin = rects_gaps$xmin[4], 
                xmax = rects_gaps$xmax[4]), 
            fill = "lightcoral", 
            alpha = .1) + 
  #  geom_hline(yintercept = 0, linetype = "longdash") +
  scale_color_manual(values = candidatecolors[1:3]) +
  scale_fill_manual(values = candidatecolors[1:3]) +
  theme_minimal() +
  coord_cartesian(xlim = c(as.Date("2020-12-31"), as.Date("2021-12-31"))) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  labs(x =  "Date", y = "Relative frequency of articles mentioning candidate") +
  guides(colour = "none", fill = "none", linetype = "none") +
  
  geom_smooth(aes(x = date, y =  value, 
                linetype = factor(source_bin, levels = c("RT", "german media")),
              #    factor(source, levels = c("RTDE", "BILD", "Frankfurter Rundschau", "Die Welt", "Süddeutsche Zeitung", "taz - die tageszeitung")), 
                colour = candidate, fill = candidate), 
data = byday_all_long %>% filter(variable == "related")) 
#+ 
  geom_point(aes(x = date, y =  value, 
                  group = factor(source_bin, levels = c("RT", "german media")),
                  #    factor(source, levels = c("RTDE", "BILD", "Frankfurter Rundschau", "Die Welt", "Süddeutsche Zeitung", "taz - die tageszeitung")), 
                  colour = candidate, fill = candidate), 
              data = byday_all_long %>% filter(variable == "related"))

ggsave("plots/relative_N_candidate")



segmented_data <- byday_all_long %>% 
  filter(variable == "negativity") %>% 
  dplyr::select(c(date, value, candidate, source_bin, byday_all_long$RT)) %>% 
  mutate(date = as.numeric(date - min(date))) %>% filter(date != 0) 
  
segdata_diff <- diff_by_day %>% 
  filter(variable == "negativity")

  # period
library(chngpt)


fit=chngptm(formula.1=value~1, formula.2=~date*candidate, diff_by_day, type="segmented",
            family="gaussian", var.type="bootstrap", ci.bootstrap.size=100)


summary(fit)
plot(fit)
lincomb(fit, comb=c(0,1,1), alpha=0.05)



  geom_smooth(aes(x = date, y =  value, linetype = source_bin, colour = candidate),
              #      factor(source, levels = c("RTDE", "BILD", "Frankfurter Rundschau", "Die Welt", "Süddeutsche Zeitung", "taz - die tageszeitung"))),  
              se = FALSE, 
              data = byday_all_long %>% filter(variable == "negativity"))
  
  
  
  
  geom_smooth(aes_string(x = "date", y = variable, colour = "candidatename"), data = diff_by_day %>% filter(candidatename != "none" & period == "before"), method = method, se = se) +
  geom_smooth(aes_string(x = "date", y = variable, colour = "candidatename"), data = diff_by_day %>% filter(candidatename != "none" & period == "green_period"), method = method, se = se) +
  geom_smooth(aes_string(x = "date", y = variable, colour = "candidatename"), data = diff_by_day %>% filter(candidatename != "none" & period == "black_period"), method = method, se = se) +
  geom_smooth(aes_string(x = "date", y = variable, colour = "candidatename"), data = diff_by_day %>% filter(candidatename != "none" & period == "red_period"), method = method, se = se) +
  geom_smooth(aes_string(x = "date", y = variable, colour = "candidatename"), data = diff_by_day %>% filter(candidatename != "none" & period == "after"), method = method, se = se) 
  

  analysis_data %>% 
    mutate(article_sent_article = ifelse(candidate == 0, NA, article_sent_article)) %>% 
    group_by(source = factor(source, c("RTDE", "BILD", "Frankfurter Rundschau", "Die Welt", "Süddeutsche Zeitung", "taz - die tageszeitung"))) %>% 
    summarize(n = n(), 
              "candidate related" = sum(candidate), 
              sentiment = fmean(article_sent_article),
              "headline sentiment" = fmean(article_sent_headlead),
              "classification rate" = fmean(share_classified_toks)) %>% 
    mutate(across(where(is.numeric), ~ round(.x, 3))) %>% 
    knitr::kable() %>% 
    kableExtra::kable_styling()
    

# plots using diff ####

(gg_date_diff_week <- 
   ggplot() +
  geom_rect(aes(ymin = -Inf, 
                ymax = Inf, 
                xmin = week(rects_gaps$xmin[1]), 
                xmax = week(rects_gaps$xmax[1])), 
            fill = "gray40", 
            alpha = .1) +
  geom_rect(aes(ymin = -Inf, 
                ymax = Inf, 
                xmin = week(rects_gaps$xmin[2]), 
                xmax = week(rects_gaps$xmax[2])), 
            fill = "greenyellow", 
            alpha = .1) +
  geom_rect(aes(ymin = -Inf, 
                ymax = Inf, 
                xmin = week(rects_gaps$xmin[3]), 
                xmax = week(rects_gaps$xmax[3])), 
            fill = "gray40", 
            alpha = .1) +
  geom_rect(aes(ymin = -Inf, 
                ymax = Inf, 
                xmin = week(rects_gaps$xmin[4]), 
                xmax = week(rects_gaps$xmax[4])), 
            fill = "lightcoral", 
            alpha = .1) +
  
  scale_color_manual(values = candidatecolors[1:3]) +
  geom_line(aes(x = weekno, y = N, colour = candidatename), data = diff_by_week %>% filter(candidatename != "none" & weekno != min(weekno) & weekno != max(weekno))) +
  theme_minimal()
)

# N BILD: for week hardly a difference
(gg_date_diff_day_nobild <- 
    ggplot() +
    geom_rect(aes(ymin = -Inf, 
                  ymax = Inf, 
                  xmin = week(rects_gaps$xmin[1]), 
                  xmax = week(rects_gaps$xmax[1])), 
              fill = "gray40", 
              alpha = .1) +
    geom_rect(aes(ymin = -Inf, 
                  ymax = Inf, 
                  xmin = week(rects_gaps$xmin[2]), 
                  xmax = week(rects_gaps$xmax[2])), 
              fill = "greenyellow", 
              alpha = .1) +
    geom_rect(aes(ymin = -Inf, 
                  ymax = Inf, 
                  xmin = week(rects_gaps$xmin[3]), 
                  xmax = week(rects_gaps$xmax[3])), 
              fill = "gray40", 
              alpha = .1) +
    geom_rect(aes(ymin = -Inf, 
                  ymax = Inf, 
                  xmin = week(rects_gaps$xmin[4]), 
                  xmax = week(rects_gaps$xmax[4])), 
              fill = "lightcoral", 
              alpha = .1) +
    
    scale_color_manual(values = candidatecolors[1:3]) +
    geom_line(aes(x = weekno, y = N, colour = candidatename), data = diff_by_week_nobild %>% filter(candidatename != "none" & weekno != min(weekno) & weekno != max(weekno))) +
    theme_minimal()
)




(gg_date_diff_day <- 
ggplot() +
  geom_rect(aes(ymin = -Inf, 
                ymax = Inf, 
                xmin = rects_gaps$xmin[1], 
                xmax = rects_gaps$xmax[1]), 
            fill = "gray40", 
            alpha = .1) +
  geom_rect(aes(ymin = -Inf, 
                ymax = Inf, 
                xmin = rects_gaps$xmin[2], 
                xmax = rects_gaps$xmax[2]), 
            fill = "greenyellow", 
            alpha = .1) +
  geom_rect(aes(ymin = -Inf, 
                ymax = Inf, 
                xmin = rects_gaps$xmin[3], 
                xmax = rects_gaps$xmax[3]), 
            fill = "gray40", 
            alpha = .1) +
  geom_rect(aes(ymin = -Inf, 
                ymax = Inf, 
                xmin = rects_gaps$xmin[4], 
                xmax = rects_gaps$xmax[4]), 
            fill = "lightcoral", 
            alpha = .1) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  scale_color_manual(values = candidatecolors[1:3]) +
  geom_line(aes(x = date, y = negative_mentions, colour = candidatename), data = diff_by_day %>% filter(candidatename != "none"), formula = "y ~ poly(x,2)") +
  theme_minimal()
)


# no BILD:
(gg_date_diff_day <- 
    ggplot() +
    geom_rect(aes(ymin = -Inf, 
                  ymax = Inf, 
                  xmin = rects_gaps$xmin[1], 
                  xmax = rects_gaps$xmax[1]), 
              fill = "gray40", 
              alpha = .1) +
    geom_rect(aes(ymin = -Inf, 
                  ymax = Inf, 
                  xmin = rects_gaps$xmin[2], 
                  xmax = rects_gaps$xmax[2]), 
              fill = "greenyellow", 
              alpha = .1) +
    geom_rect(aes(ymin = -Inf, 
                  ymax = Inf, 
                  xmin = rects_gaps$xmin[3], 
                  xmax = rects_gaps$xmax[3]), 
              fill = "gray40", 
              alpha = .1) +
    geom_rect(aes(ymin = -Inf, 
                  ymax = Inf, 
                  xmin = rects_gaps$xmin[4], 
                  xmax = rects_gaps$xmax[4]), 
              fill = "lightcoral", 
              alpha = .1) +
    geom_hline(yintercept = 0, linetype = "longdash") +
    scale_color_manual(values = candidatecolors[1:3]) +
    geom_line(aes(x = date, y = negative_mentions, colour = candidatename), data = diff_by_day_nobild %>% filter(candidatename != "none"), formula = "y ~ poly(x,2)") +
    theme_minimal()
)



## plot lm by periods

(gg_lm_period <- 
    ggplot() +
    geom_rect(aes(ymin = -Inf, 
                  ymax = Inf, 
                  xmin = rects_gaps$xmin[1], 
                  xmax = rects_gaps$xmax[1]), 
              fill = "gray40", 
              alpha = .1) +
    geom_rect(aes(ymin = -Inf, 
                  ymax = Inf, 
                  xmin = rects_gaps$xmin[2], 
                  xmax = rects_gaps$xmax[2]), 
              fill = "greenyellow", 
              alpha = .1) +
    geom_rect(aes(ymin = -Inf, 
                  ymax = Inf, 
                  xmin = rects_gaps$xmin[3], 
                  xmax = rects_gaps$xmax[3]), 
              fill = "gray40", 
              alpha = .1) +
    geom_rect(aes(ymin = -Inf, 
                  ymax = Inf, 
                  xmin = rects_gaps$xmin[4], 
                  xmax = rects_gaps$xmax[4]), 
              fill = "lightcoral", 
              alpha = .1) + 
  #  geom_hline(yintercept = 0, linetype = "longdash") +
    scale_color_manual(values = candidatecolors[1:3]) +
    geom_smooth(aes(x = date, y = negative_mentions, colour = candidatename), data = diff_by_day %>% filter(candidatename != "none" & period == "before"), method = "lm", se = FALSE) +
    geom_smooth(aes(x = date, y = negative_mentions, colour = candidatename), data = diff_by_day %>% filter(candidatename != "none" & period == "green_period"), method = "lm", se = FALSE) +
    geom_smooth(aes(x = date, y = negative_mentions, colour = candidatename), data = diff_by_day %>% filter(candidatename != "none" & period == "black_period"), method = "lm", se = FALSE) +
    geom_smooth(aes(x = date, y = negative_mentions, colour = candidatename), data = diff_by_day %>% filter(candidatename != "none" & period == "red_period"), method = "lm", se = FALSE) +
    geom_smooth(aes(x = date, y = negative_mentions, colour = candidatename), data = diff_by_day %>% filter(candidatename != "none" & period == "after"), method = "lm", se = FALSE) +
    theme_minimal() +
    labs(x =  "Date", y = "Negative overreporting", colour = "Candidate")
)
ggsave("plots/lm_period.png")

# plot function: ####
plot_overreporting <- function(variable = "N", method = "loess", se = FALSE){  
  ggplot() +
    geom_rect(aes(ymin = -Inf, 
                  ymax = Inf, 
                  xmin = rects_gaps$xmin[1], 
                  xmax = rects_gaps$xmax[1]), 
              fill = "gray40", 
              alpha = .1) +
    geom_rect(aes(ymin = -Inf, 
                  ymax = Inf, 
                  xmin = rects_gaps$xmin[2], 
                  xmax = rects_gaps$xmax[2]), 
              fill = "greenyellow", 
              alpha = .1) +
    geom_rect(aes(ymin = -Inf, 
                  ymax = Inf, 
                  xmin = rects_gaps$xmin[3], 
                  xmax = rects_gaps$xmax[3]), 
              fill = "gray40", 
              alpha = .1) +
    geom_rect(aes(ymin = -Inf, 
                  ymax = Inf, 
                  xmin = rects_gaps$xmin[4], 
                  xmax = rects_gaps$xmax[4]), 
              fill = "lightcoral", 
              alpha = .1) + 
    #  geom_hline(yintercept = 0, linetype = "longdash") +
    scale_color_manual(values = candidatecolors[1:3]) +
    geom_smooth(aes_string(x = "date", y = variable, colour = "candidatename"), data = diff_by_day %>% filter(candidatename != "none" & period == "before"), method = method, se = se) +
    geom_smooth(aes_string(x = "date", y = variable, colour = "candidatename"), data = diff_by_day %>% filter(candidatename != "none" & period == "green_period"), method = method, se = se) +
    geom_smooth(aes_string(x = "date", y = variable, colour = "candidatename"), data = diff_by_day %>% filter(candidatename != "none" & period == "black_period"), method = method, se = se) +
    geom_smooth(aes_string(x = "date", y = variable, colour = "candidatename"), data = diff_by_day %>% filter(candidatename != "none" & period == "red_period"), method = method, se = se) +
    geom_smooth(aes_string(x = "date", y = variable, colour = "candidatename"), data = diff_by_day %>% filter(candidatename != "none" & period == "after"), method = method, se = se) +
    theme_minimal() +
    coord_cartesian(xlim = c(as.Date("2020-12-31"), as.Date("2021-12-31"))) + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
    labs(x =  "Date", y = paste0("Overreporting (", variable, ")"), colour = "Candidate") +
    guides(colour = "none")
  }


# N articles:
(gg_loess_period_N <- plot_overreporting() + 
    geom_hline(yintercept = 0, alpha = .2) +
    geom_vline(aes(xintercept = date, group = event, colour = candidate), data = events, linetype = "dashed")) +
  geom_jitter(aes(date, N,  colour = candidatename), data = diff_by_day %>% filter(candidatename != "none"), alpha = .3)
ggsave("plots/loess_period_N.png")
# Not:
  (gg_lm_period_N <- plot_overreporting(method = "lm") + geom_hline(yintercept = 0, linetype = "dashed"))
  ggsave("plots/lm_period_N.png")

# mentions:
  (gg_loess_period_N <- plot_overreporting("mentions") + 
      geom_hline(yintercept = 0, alpha = .2) +
      geom_vline(aes(xintercept = date, group = event, colour = candidate), data = events, linetype = "dashed")) +
  geom_jitter(aes(date, mentions,  colour = candidatename), data = diff_by_day %>% filter(candidatename != "none"), alpha = .3)
  ggsave("plots/loess_period_mentions.png")
  
  # Not:
  (gg_lm_period_N <- plot_overreporting("mentions", method = "lm") + geom_hline(yintercept = 0, linetype = "dashed"))
  ggsave("plots/lm_period_mentions.png")
  
  

# headlines:
(gg_loess_period_N <- plot_overreporting("negative_headlead", "lm") + 
    geom_hline(yintercept = 0, linetype = "dashed")+
    geom_vline(aes(xintercept = date, group = event, colour = candidate), data = events, linetype = "dashed"))
ggsave("plots/loess_period_head_negativity.png")


# negative articles:
# good:
(gg_loess_period_Nneg <- plot_overreporting("negative_binary_article"))
ggsave("plots/loess_period_Nneg.png")
(gg_lm_period_Nneg <- plot_overreporting("negative_binary_article", "lm") + 
    geom_jitter(aes(date, negative_binary_article,  colour = candidatename), data = diff_by_day %>% filter(candidatename != "none")))
ggsave("plots/lm_period_Nneg.png")

# NEED TO TRY:
# very negative articles:
# (gg_loess_period_Nextremelyneg <- plot_overreporting("negative_binary_article_strong"))
# ggsave("plots/loess_period_Nextremeneg.png")
# (gg_lm_period_Nextremelyneg <- plot_overreporting("negative_binary_article_strong", "lm"))
# ggsave("plots/lm_period_Nextremeneg.png")


# polarity:
  (gg_loess_period_negativity <- plot_overreporting("negative_article"))
  ggsave("plots/loess_period_negativity.png")
# THIS ONE:
(gg_lm_period_negativity <- plot_overreporting("negative_article", "lm")) +
    geom_jitter(aes(date, negative_article,  colour = candidatename), data = diff_by_day %>% filter(candidatename != "none"), alpha = .3)
  
ggsave("plots/lm_period_negativity.png")


# strong polarity:
ggplot() +
  geom_rect(aes(ymin = -Inf, 
                ymax = Inf, 
                xmin = rects_gaps$xmin[1], 
                xmax = rects_gaps$xmax[1]), 
            fill = "gray40", 
            alpha = .1) +
  geom_rect(aes(ymin = -Inf, 
                ymax = Inf, 
                xmin = rects_gaps$xmin[2], 
                xmax = rects_gaps$xmax[2]), 
            fill = "greenyellow", 
            alpha = .1) +
  geom_rect(aes(ymin = -Inf, 
                ymax = Inf, 
                xmin = rects_gaps$xmin[3], 
                xmax = rects_gaps$xmax[3]), 
            fill = "gray40", 
            alpha = .1) +
  geom_rect(aes(ymin = -Inf, 
                ymax = Inf, 
                xmin = rects_gaps$xmin[4], 
                xmax = rects_gaps$xmax[4]), 
            fill = "lightcoral", 
            alpha = .1) + 
  # geom_smooth(aes(date, negative_strong_baerbock_headlead, group = period), linetype = "dashed", colour = "green", data = diff_by_day %>%  filter(period != "interim"), se = FALSE) +
  # geom_smooth(aes(date, negative_strong_laschet_headlead, group = period), linetype = "dashed", colour = "black", data = diff_by_day %>% filter(period != "interim"), se = FALSE) +
  # geom_smooth(aes(date, negative_strong_scholz_headlead, group = period), linetype = "dashed", colour = "red", data = diff_by_day %>% filter(period != "interim"), se = FALSE) +
  
  
geom_smooth(aes(date, negative_strong_baerbock, group = period),  colour = "green", data = diff_by_day %>%  filter(period != "interim"), se = FALSE) +
  geom_jitter(aes(date, negative_strong_baerbock),  colour = "green", data = diff_by_day , alpha = .3) +
  geom_smooth(aes(date, negative_strong_laschet, group = period),  colour = "black", data = diff_by_day %>% filter(period != "interim"), se = FALSE) +
    geom_jitter(aes(date, negative_strong_laschet),  colour = "black", data = diff_by_day , alpha = .3) +
  geom_smooth(aes(date, negative_strong_scholz, group = period),  colour = "red", data = diff_by_day %>% filter(period != "interim"), se = FALSE) +
  geom_jitter(aes(date, negative_strong_scholz),  colour = "red", data = diff_by_day , alpha = .3) +
 
   theme_minimal() +
  coord_cartesian(xlim = c(as.Date("2020-12-31"), as.Date("2021-12-31"))) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  labs(x =  "Date", y = "overreporting in strongly negative articles", colour = "Candidate") +
  guides(colour = "none")
ggsave("plots/Nstrongly_negative_articles.png")

ggplot() +
  geom_rect(aes(ymin = -Inf, 
                ymax = Inf, 
                xmin = rects_gaps$xmin[1], 
                xmax = rects_gaps$xmax[1]), 
            fill = "gray40", 
            alpha = .1) +
  geom_rect(aes(ymin = -Inf, 
                ymax = Inf, 
                xmin = rects_gaps$xmin[2], 
                xmax = rects_gaps$xmax[2]), 
            fill = "greenyellow", 
            alpha = .1) +
  geom_rect(aes(ymin = -Inf, 
                ymax = Inf, 
                xmin = rects_gaps$xmin[3], 
                xmax = rects_gaps$xmax[3]), 
            fill = "gray40", 
            alpha = .1) +
  geom_rect(aes(ymin = -Inf, 
                ymax = Inf, 
                xmin = rects_gaps$xmin[4], 
                xmax = rects_gaps$xmax[4]), 
            fill = "lightcoral", 
            alpha = .1) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
geom_smooth(aes(date, negative_strong_baerbock_headlead, group = period),  colour = "green", data = diff_by_day %>%  filter(period != "interim"), se = FALSE) +
  geom_jitter(aes(date, negative_strong_baerbock_headlead),  colour = "green", data = diff_by_day , alpha = .3) +
  geom_smooth(aes(date, negative_strong_laschet_headlead, group = period),  colour = "black", data = diff_by_day %>% filter(period != "interim"), se = FALSE) +
  geom_jitter(aes(date, negative_strong_laschet_headlead),  colour = "black", data = diff_by_day , alpha = .3) +
  geom_smooth(aes(date, negative_strong_scholz_headlead, group = period),  colour = "red", data = diff_by_day %>% filter(period != "interim"), se = FALSE) +
  geom_jitter(aes(date, negative_strong_scholz_headlead),  colour = "red", data = diff_by_day , alpha = .3) +
  theme_minimal() +
  coord_cartesian(xlim = c(as.Date("2020-12-31"), as.Date("2021-12-31"))) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  labs(x =  "Date", y = "overreporting in strongly negative headline & lead", colour = "Candidate") +
  guides(colour = "none")
ggsave("plots/Nstrongly_negative_headlines.png")

# negativity without BILD
ggplot() +
  geom_rect(aes(ymin = -Inf, 
                ymax = Inf, 
                xmin = rects_gaps$xmin[1], 
                xmax = rects_gaps$xmax[1]), 
            fill = "gray40", 
            alpha = .1) +
  geom_rect(aes(ymin = -Inf, 
                ymax = Inf, 
                xmin = rects_gaps$xmin[2], 
                xmax = rects_gaps$xmax[2]), 
            fill = "greenyellow", 
            alpha = .1) +
  geom_rect(aes(ymin = -Inf, 
                ymax = Inf, 
                xmin = rects_gaps$xmin[3], 
                xmax = rects_gaps$xmax[3]), 
            fill = "gray40", 
            alpha = .1) +
  geom_rect(aes(ymin = -Inf, 
                ymax = Inf, 
                xmin = rects_gaps$xmin[4], 
                xmax = rects_gaps$xmax[4]), 
            fill = "lightcoral", 
            alpha = .1) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(aes(date, negative_strong_baerbock_headlead, group = period),  colour = "green", data = diff_by_day_nobild %>%  filter(period != "interim"), se = FALSE) +
  geom_jitter(aes(date, negative_strong_baerbock_headlead),  colour = "green", data = diff_by_day_nobild , alpha = .3) +
  geom_smooth(aes(date, negative_strong_laschet_headlead, group = period),  colour = "black", data = diff_by_day_nobild %>% filter(period != "interim"), se = FALSE) +
  geom_jitter(aes(date, negative_strong_laschet_headlead),  colour = "black", data = diff_by_day_nobild , alpha = .3) +
  geom_smooth(aes(date, negative_strong_scholz_headlead, group = period),  colour = "red", data = diff_by_day_nobild %>% filter(period != "interim"), se = FALSE) +
  geom_jitter(aes(date, negative_strong_scholz_headlead),  colour = "red", data = diff_by_day_nobild , alpha = .3) +
  theme_minimal() +
  coord_cartesian(xlim = c(as.Date("2020-12-31"), as.Date("2021-12-31"))) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  labs(x =  "Date", y = "overreporting in strongly negative headline & lead", colour = "Candidate") +
  guides(colour = "none")
ggsave("plots/Nstrongly_negative_headlines_nobild.png")


# bisschen schwieriger: candidate_sentiment

library(CausalImpact)
library(zoo)
library(lubridate)

library(segmented)




# NOT USED: synthetic controls: ####

# day aggregation needed?

# without gaps
period1_nogaps <- c(rects_nogaps$xmin[1]+1, rects_nogaps$xmax[1]) 
period2_nogaps <- c(rects_nogaps$xmin[2]+1, rects_nogaps$xmax[2])
period3_nogaps <- c(rects_nogaps$xmin[3]+1, rects_nogaps$xmax[3]) 
period4_nogaps <- c(rects_nogaps$xmin[4]+1, rects_nogaps$xmax[4])

# with gaps (if thats possible)
period1_gaps <- c(rects_gaps$xmin[1]+1, rects_gaps$xmax[1]) 
period2_gaps <- c(rects_gaps$xmin[2]+1, rects_gaps$xmax[2])
period3_gaps <- c(rects_gaps$xmin[3]+1, rects_gaps$xmax[3]) 
period4_gaps <- c(rects_gaps$xmin[4]+1, rects_gaps$xmax[4])

# week without gaps
period1_nogaps_week <- c(1, week(rects_nogaps$xmax[1])-1)
period2_nogaps_week <- c(week(rects_nogaps$xmin[2]), week(rects_nogaps$xmax[2])-1)
period3_nogaps_week <- c(week(rects_nogaps$xmin[3]), week(rects_nogaps$xmax[3])-1) 
period4_nogaps_week <- c(week(rects_nogaps$xmin[4]), week(rects_nogaps$xmax[4])-1)

# with gaps
period1_gaps_week <- c(1, week(rects_gaps$xmax[1])-1)
period2_gaps_week <- c(week(rects_gaps$xmin[2]), week(rects_gaps$xmax[2])-1)
period3_gaps_week <- c(week(rects_gaps$xmin[3]), week(rects_gaps$xmax[3])-1) 
period4_gaps_week <- c(week(rects_gaps$xmin[4]), week(rects_gaps$xmax[4])-1)


# IVs <- list(byday %>% select(contains(c("baerbock", "scholz", "laschet"))))



causal_data1 <- byw %>% filter(source == "RTDE")  
timepoints <- causal_data1$week

DV1L <- ts(causal_data1$mentions_laschet)
DV1B <- ts(causal_data1$mentions_baerbock)
DV1S <- ts(causal_data1$mentions_scholz)

DV2L <- ts(causal_data1$negative_laschet)
DV2B <- ts(causal_data1$negative_baerbock)
DV2S <- ts(causal_data1$negative_scholz)



causal_impact_dataL <- zoo(DV1L, timepoints)
causal_impact_dataB <- zoo(DV1B, timepoints)
causal_impact_dataS <- zoo(DV1S, timepoints)

causal_impact_dataL <- zoo(DV2L, timepoints)
causal_impact_dataB <- zoo(DV2B, timepoints)
causal_impact_dataS <- zoo(DV2S, timepoints)


diff_by_day_b <- diff_by_day %>% filter(candidatename == "baerbock")
causal_impact_dataW <- zoo(ts(diff_by_day_b$negative_headlead), diff_by_day_b$date)
impactW <- CausalImpact::CausalImpact(causal_impact_dataW, period1_nogaps, period2_nogaps)


impactL <- CausalImpact::CausalImpact(causal_impact_dataL, period1_nogaps_week, period2_nogaps_week)
impactS <- CausalImpact::CausalImpact(causal_impact_dataS, period1_nogaps_week, period2_nogaps_week)


impact_gaps <- CausalImpact::CausalImpact(causal_impact_data1, period1_gaps_week, period2_gaps_week
                                     # , period3_nogaps, period4_nogaps
)

plot(impactW)
summary(impactB)
summary(impactB, "report")

plot(impactL)
summary(impactL)
summary(impactL, "report")

plot(impactS)
summary(impactS)
summary(impactS, "report")

# period 2




# strange plot: ####

# select(-c(positive_classification, negative_classification, positive_score))

data_plot1 <- analysis_data %>% 
  # pivot_longer(c("baerbock", "laschet", "scholz"), names_to = "candidate", values_to = "bin") %>% 
  pivot_longer(contains(c("_baerbock", "_laschet", "_scholz")), names_to = c("variable", "candidate") , names_sep = "_") %>% 
  mutate(value = ifelse(is.na(value), 0, value))

# plot differences:
  # freqpoly instead smooth?
# ggplot(data_plot1, aes(y = value, x = date)) +
ggplot(data = data_plot1, aes(y = value, x = date)) +
 geom_rect(aes(ymin = -.1, 
             ymax = .4, 
             xmin = rects_gaps$xmin[1], 
             xmax = rects_gaps$xmax[1]), 
             fill = "gray40", 
             alpha = .1) +
   geom_rect(aes(ymin = -.1, 
             ymax = .4, 
             xmin = rects_gaps$xmin[2], 
             xmax = rects_gaps$xmax[2]), 
             fill = "greenyellow", 
             alpha = .1) +
   geom_rect(aes(ymin = -.1, 
             ymax = .4, 
             xmin = rects_gaps$xmin[3], 
             xmax = rects_gaps$xmax[3]), 
             fill = "gray40", 
             alpha = .1) +
   geom_rect(aes(ymin = -.1, 
             ymax = .4, 
             xmin = rects_gaps$xmin[4], 
             xmax = rects_gaps$xmax[4]), 
             fill = "lightcoral", 
             alpha = .1) +
 #gg_leading_party_background_gaps +
  geom_smooth(aes(fill = factor(RT))) +
  geom_smooth(aes(colour = source), se = F) +
  coord_cartesian(xlim = c(as.Date("2020-12-31"), as.Date("2021-12-31")), ylim = c(0, .4)) + 
 # scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  facet_grid(rows = vars(variable), 
             cols = vars(candidate), 
             scales = "free_y") +
  theme_minimal() +
  labs(x = "Date", y = "share of all articles")
ggsave("plots/candidate_mentions.png")


# save:

save(list = c(
  "diff_by_day", "diff_by_day_nobild"
), 
file = "analyses/diff_by_day.RData")
