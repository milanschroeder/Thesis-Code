# 06 Regression

library(pacman)
p_load(tidyverse, magrittr)

# prepare data: ####
load("analyses/diff_by_day.RData")

diff_by_day %<>% mutate(period_red  = ifelse(period == "red_period", 1, 0),
                        period_green  = ifelse(period == "green_period", 1, 0),
                        period_black  = ifelse(period == "black_period", 1, 0), 
                        pre_campaign = ifelse(as.Date(date, origin = as.Date("2021-01-01")) < as.Date("2021-04-19"), 1, 0),
                        post_campaign = ifelse(as.Date(date, origin = as.Date("2021-01-01")) > as.Date("2021-09-26"), 1, 0),
                        date = as.numeric(date - min(date)), 
                        own_period = case_when(candidatename == "baerbock" & period_green == 1 ~ 1,
                                               candidatename == "scholz" & period_red == 1 ~ 1,
                                               candidatename == "laschet"  & period_black == 1 ~ 1, 
                                               TRUE ~ 0),
                        
                        rank_top = case_when(candidatename == "baerbock" & rank_gru == 1 ~ 1,
                                             candidatename == "scholz" & rank_spd == 1 ~ 1,
                                             candidatename == "laschet" & rank_cdu == 1 ~ 1,  
                                             TRUE ~ 0),
                        
                        
                        gru_spd = share_gru - share_spd,
                        gru_cdu = share_gru - share_cdu,
                        spd_cdu = share_spd - share_cdu,
                        
                        rank_top2 = case_when(candidatename == "baerbock" & rank_gru < 3 ~ 1,
                                              candidatename == "scholz" & rank_spd < 3 ~ 1,
                                              candidatename == "laschet"  & rank_cdu < 3 ~ 1,
                                              candidatename == "baerbock" & is.na(rank_gru) ~ NaN,
                                              candidatename == "scholz" & is.na(rank_spd) ~ NaN,
                                              candidatename == "laschet"  & is.na(rank_cdu) ~ NaN,
                                              TRUE ~ 0),
                        
                        relative_share = case_when(candidatename == "baerbock" ~ gru_cdu + gru_spd,
                                                   candidatename == "scholz" ~ spd_cdu + spd_gru,
                                                   candidatename == "laschet" ~ cdu_gru + cdu_spd, 
                                                   TRUE ~ NaN
                        ),
                        
                        candidatename = factor(candidatename, levels = c( "none", "scholz", "laschet", "baerbock")),
                        
                        baerbock = ifelse(candidatename == "baerbock", 1, 0),
                        laschet = ifelse(candidatename == "laschet", 1, 0),
                        scholz = ifelse(candidatename == "scholz", 1, 0)
                        
)




# regression: ####

{
(lm_N_1 <- lm(N ~ date * own_period + date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
(lm_N_2 <- lm(N ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_N_3 <- lm(N ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_N_4 <- lm(N ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_N_5 <- lm(N ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
regtab_N <- stargazer(lm_N_1, lm_N_2, lm_N_3, lm_N_4, lm_N_5, type = "text")
writeLines(regtab_N, "analyses/regtab_N.txt", useBytes = T)

(lm_relative_N_1 <- lm(N_relative ~ date * own_period + date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
(lm_relative_N_2 <- lm(N_relative ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_relative_N_3 <- lm(N_relative ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_relative_N_4 <- lm(N_relative ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_relative_N_5 <- lm(N_relative ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
regtab_relative_N <- stargazer(lm_relative_N_1, lm_relative_N_2, lm_relative_N_3, lm_relative_N_4, lm_relative_N_5, type = "text")
writeLines(regtab_relative_N, "analyses/regtab_relative_N.txt", useBytes = T)

(lm_mentions_1 <- lm(mentions ~ date * own_period + date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
(lm_mentions_2 <- lm(mentions ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_mentions_3 <- lm(mentions ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_mentions_4 <- lm(mentions ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_mentions_5 <- lm(mentions ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
regtab_mentions <- stargazer(lm_mentions_1, lm_mentions_2, lm_mentions_3, lm_mentions_4, lm_mentions_5, type = "text")
writeLines(regtab_mentions, "analyses/regtab_mentions.txt", useBytes = T)

(lm_negative_mentions_1 <- lm(negative_mentions ~ date * own_period+ date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
(lm_negative_mentions_2 <- lm(negative_mentions ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_mentions_3 <- lm(negative_mentions ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_mentions_4 <- lm(negative_mentions ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_mentions_5 <- lm(negative_mentions ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
regtab_negative_mentions <- stargazer(lm_negative_mentions_1, lm_negative_mentions_2, lm_negative_mentions_3, lm_negative_mentions_4, lm_negative_mentions_5, type = "text")
writeLines(regtab_negative_mentions, "analyses/regtab_negative_mentions.txt", useBytes = T)

(lm_negative_article_1 <- lm(negative_article ~ date * own_period+ date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
(lm_negative_article_2 <- lm(negative_article ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_article_3 <- lm(negative_article ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_article_4 <- lm(negative_article ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_article_5 <- lm(negative_article ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
regtab_negative_article <- stargazer(lm_negative_article_1, lm_negative_article_2, lm_negative_article_3, lm_negative_article_4, lm_negative_article_5, type = "text")
writeLines(regtab_negative_article, "analyses/regtab_negative_article.txt", useBytes = T)

(lm_negative_headlead_1 <- lm(negative_headlead ~ date * own_period+ date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
(lm_negative_headlead_2 <- lm(negative_headlead ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_headlead_3 <- lm(negative_headlead ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_headlead_4 <- lm(negative_headlead ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_headlead_5 <- lm(negative_headlead ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
regtab_negative_headlead <- stargazer(lm_negative_headlead_1, lm_negative_headlead_2, lm_negative_headlead_3, lm_negative_headlead_4, lm_negative_headlead_5, type = "text")
writeLines(regtab_negative_headlead, "analyses/regtab_negative_headlead.txt", useBytes = T)

(lm_negative_binary_article_relative_1 <- lm(negative_binary_article_relative ~ date * own_period+ date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
(lm_negative_binary_article_relative_2 <- lm(negative_binary_article_relative ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_binary_article_relative_3 <- lm(negative_binary_article_relative ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_binary_article_relative_4 <- lm(negative_binary_article_relative ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_binary_article_relative_5 <- lm(negative_binary_article_relative ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
regtab_negative_binary_article_relative <- stargazer(lm_negative_binary_article_relative_1, lm_negative_binary_article_relative_2, lm_negative_binary_article_relative_3, lm_negative_binary_article_relative_4, lm_negative_binary_article_relative_5, type = "text")
writeLines(regtab_negative_binary_article_relative, "analyses/regtab_negative_binary_article_relative.txt", useBytes = T)

(lm_negative_binary_headlead_relative_1 <- lm(negative_binary_headlead_relative ~ date * own_period+ date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
(lm_negative_binary_headlead_relative_2 <- lm(negative_binary_headlead_relative ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_binary_headlead_relative_3 <- lm(negative_binary_headlead_relative ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_binary_headlead_relative_4 <- lm(negative_binary_headlead_relative ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_binary_headlead_relative_5 <- lm(negative_binary_headlead_relative ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
regtab_negative_binary_headlead_relative <- stargazer(lm_negative_binary_headlead_relative_1, lm_negative_binary_headlead_relative_2, lm_negative_binary_headlead_relative_3, lm_negative_binary_headlead_relative_4, lm_negative_binary_headlead_relative_5, type = "text")
writeLines(regtab_negative_binary_headlead_relative, "analyses/regtab_negative_binary_headlead_relative.txt", useBytes = T)

(lm_negative_binary_strong_article_relative_1 <- lm(negative_binary_strong_article_relative ~ date * own_period+ date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
(lm_negative_binary_strong_article_relative_2 <- lm(negative_binary_strong_article_relative ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_binary_strong_article_relative_3 <- lm(negative_binary_strong_article_relative ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_binary_strong_article_relative_4 <- lm(negative_binary_strong_article_relative ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_binary_strong_article_relative_5 <- lm(negative_binary_strong_article_relative ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
regtab_negative_binary_strong_article_relative <- stargazer(lm_negative_binary_strong_article_relative_1, lm_negative_binary_strong_article_relative_2, lm_negative_binary_strong_article_relative_3, lm_negative_binary_strong_article_relative_4, lm_negative_binary_strong_article_relative_5, type = "text")
writeLines(regtab_negative_binary_strong_article_relative, "analyses/regtab_negative_binary_strong_article_relative.txt", useBytes = T)

(lm_negative_binary_strong_headlead_relative_1 <- lm(negative_binary_strong_headlead_relative ~ date * own_period+ date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
(lm_negative_binary_strong_headlead_relative_2 <- lm(negative_binary_strong_headlead_relative ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_binary_strong_headlead_relative_3 <- lm(negative_binary_strong_headlead_relative ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_binary_strong_headlead_relative_4 <- lm(negative_binary_strong_headlead_relative ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_binary_strong_headlead_relative_5 <- lm(negative_binary_strong_headlead_relative ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
regtab_negative_binary_strong_headlead_relative <- stargazer(lm_negative_binary_strong_headlead_relative_1, lm_negative_binary_strong_headlead_relative_2, lm_negative_binary_strong_headlead_relative_3, lm_negative_binary_strong_headlead_relative_4, lm_negative_binary_strong_headlead_relative_5, type = "text")
writeLines(regtab_negative_binary_strong_headlead_relative, "analyses/regtab_negative_binary_strong_headlead_relative.txt", useBytes = T)
}




# alternative specifications: ####

{
  (lm_N_1 <- lm(N ~ date * own_period + date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
  (lm_N_2 <- lm(N ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_N_3 <- lm(N ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_N_4 <- lm(N ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_N_5 <- lm(N ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  regtab_N <- stargazer(lm_N_1, lm_N_2, lm_N_3, lm_N_4, lm_N_5, type = "text")
  writeLines(regtab_N, "analyses/regtab_N.txt", useBytes = T)
  
  (lm_relative_N_1 <- lm(N_relative ~ date * own_period + date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
  (lm_relative_N_2 <- lm(N_relative ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_relative_N_3 <- lm(N_relative ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_relative_N_4 <- lm(N_relative ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_relative_N_5 <- lm(N_relative ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  regtab_relative_N <- stargazer(lm_relative_N_1, lm_relative_N_2, lm_relative_N_3, lm_relative_N_4, lm_relative_N_5, type = "text")
  writeLines(regtab_relative_N, "analyses/regtab_relative_N.txt", useBytes = T)
  
  (lm_mentions_1 <- lm(mentions ~ date * own_period + date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
  (lm_mentions_2 <- lm(mentions ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_mentions_3 <- lm(mentions ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_mentions_4 <- lm(mentions ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_mentions_5 <- lm(mentions ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  regtab_mentions <- stargazer(lm_mentions_1, lm_mentions_2, lm_mentions_3, lm_mentions_4, lm_mentions_5, type = "text")
  writeLines(regtab_mentions, "analyses/regtab_mentions.txt", useBytes = T)
  
  (lm_negative_mentions_1 <- lm(negative_mentions ~ date * own_period+ date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
  (lm_negative_mentions_2 <- lm(negative_mentions ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_mentions_3 <- lm(negative_mentions ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_mentions_4 <- lm(negative_mentions ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_mentions_5 <- lm(negative_mentions ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  regtab_negative_mentions <- stargazer(lm_negative_mentions_1, lm_negative_mentions_2, lm_negative_mentions_3, lm_negative_mentions_4, lm_negative_mentions_5, type = "text")
  writeLines(regtab_negative_mentions, "analyses/regtab_negative_mentions.txt", useBytes = T)
  
  (lm_negative_article_1 <- lm(negative_article ~ date * own_period+ date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
  (lm_negative_article_2 <- lm(negative_article ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_article_3 <- lm(negative_article ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_article_4 <- lm(negative_article ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_article_5 <- lm(negative_article ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  regtab_negative_article <- stargazer(lm_negative_article_1, lm_negative_article_2, lm_negative_article_3, lm_negative_article_4, lm_negative_article_5, type = "text")
  writeLines(regtab_negative_article, "analyses/regtab_negative_article.txt", useBytes = T)
  
  (lm_negative_headlead_1 <- lm(negative_headlead ~ date * own_period+ date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
  (lm_negative_headlead_2 <- lm(negative_headlead ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_headlead_3 <- lm(negative_headlead ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_headlead_4 <- lm(negative_headlead ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_headlead_5 <- lm(negative_headlead ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  regtab_negative_headlead <- stargazer(lm_negative_headlead_1, lm_negative_headlead_2, lm_negative_headlead_3, lm_negative_headlead_4, lm_negative_headlead_5, type = "text")
  writeLines(regtab_negative_headlead, "analyses/regtab_negative_headlead.txt", useBytes = T)
  
  (lm_negative_binary_article_relative_1 <- lm(negative_binary_article_relative ~ date * own_period+ date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
  (lm_negative_binary_article_relative_2 <- lm(negative_binary_article_relative ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_binary_article_relative_3 <- lm(negative_binary_article_relative ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_binary_article_relative_4 <- lm(negative_binary_article_relative ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_binary_article_relative_5 <- lm(negative_binary_article_relative ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  regtab_negative_binary_article_relative <- stargazer(lm_negative_binary_article_relative_1, lm_negative_binary_article_relative_2, lm_negative_binary_article_relative_3, lm_negative_binary_article_relative_4, lm_negative_binary_article_relative_5, type = "text")
  writeLines(regtab_negative_binary_article_relative, "analyses/regtab_negative_binary_article_relative.txt", useBytes = T)
  
  (lm_negative_binary_headlead_relative_1 <- lm(negative_binary_headlead_relative ~ date * own_period+ date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
  (lm_negative_binary_headlead_relative_2 <- lm(negative_binary_headlead_relative ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_binary_headlead_relative_3 <- lm(negative_binary_headlead_relative ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_binary_headlead_relative_4 <- lm(negative_binary_headlead_relative ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_binary_headlead_relative_5 <- lm(negative_binary_headlead_relative ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  regtab_negative_binary_headlead_relative <- stargazer(lm_negative_binary_headlead_relative_1, lm_negative_binary_headlead_relative_2, lm_negative_binary_headlead_relative_3, lm_negative_binary_headlead_relative_4, lm_negative_binary_headlead_relative_5, type = "text")
  writeLines(regtab_negative_binary_headlead_relative, "analyses/regtab_negative_binary_headlead_relative.txt", useBytes = T)
  
  (lm_negative_binary_strong_article_relative_1 <- lm(negative_binary_strong_article_relative ~ date * own_period+ date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
  (lm_negative_binary_strong_article_relative_2 <- lm(negative_binary_strong_article_relative ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_binary_strong_article_relative_3 <- lm(negative_binary_strong_article_relative ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_binary_strong_article_relative_4 <- lm(negative_binary_strong_article_relative ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_binary_strong_article_relative_5 <- lm(negative_binary_strong_article_relative ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  regtab_negative_binary_strong_article_relative <- stargazer(lm_negative_binary_strong_article_relative_1, lm_negative_binary_strong_article_relative_2, lm_negative_binary_strong_article_relative_3, lm_negative_binary_strong_article_relative_4, lm_negative_binary_strong_article_relative_5, type = "text")
  writeLines(regtab_negative_binary_strong_article_relative, "analyses/regtab_negative_binary_strong_article_relative.txt", useBytes = T)
  
  (lm_negative_binary_strong_headlead_relative_1 <- lm(negative_binary_strong_headlead_relative ~ date * own_period+ date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
  (lm_negative_binary_strong_headlead_relative_2 <- lm(negative_binary_strong_headlead_relative ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_binary_strong_headlead_relative_3 <- lm(negative_binary_strong_headlead_relative ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_binary_strong_headlead_relative_4 <- lm(negative_binary_strong_headlead_relative ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_binary_strong_headlead_relative_5 <- lm(negative_binary_strong_headlead_relative ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  regtab_negative_binary_strong_headlead_relative <- stargazer(lm_negative_binary_strong_headlead_relative_1, lm_negative_binary_strong_headlead_relative_2, lm_negative_binary_strong_headlead_relative_3, lm_negative_binary_strong_headlead_relative_4, lm_negative_binary_strong_headlead_relative_5, type = "text")
  writeLines(regtab_negative_binary_strong_headlead_relative, "analyses/regtab_negative_binary_strong_headlead_relative.txt", useBytes = T)
}







# published: ####


p_load(texreg)
p_load(finalfit)

published_regs_n <- stargazer(
  lm_N_1, lm_N_3, lm_N_5, 
  lm_relative_N_1, lm_relative_N_3, lm_relative_N_5, 
  lm_mentions_1, lm_mentions_3
  , type = "html")
writeLines(published_regs, "analyses/published_regressions.html", useBytes = T)


published_regs_negative <- stargazer(
  lm_negative_binary_article_relative_1, lm_negative_binary_article_relative_2,
  lm_negative_binary_strong_article_relative_1, lm_negative_binary_strong_article_relative_2,
  lm_negative_mentions_1, lm_negative_mentions_2, lm_negative_mentions_5
  , type = "html")
writeLines(published_regs_negative, "analyses/published_regressions_negativity.html", useBytes = T)

published_main_models <- stargazer(
  lm_N_1,
  lm_relative_N_1,
  lm_mentions_1,
  lm_negative_mentions_1,
  lm_negative_article_1
  , type = "html")
writeLines(published_main_models, "analyses/published_main_regressions.html", useBytes = T)



finalfit(.data = diff_by_day %>% filter(candidatename != "none"), dependent = "N", metrics = T, explanatory = c("date", "campaign_started", "own_period", "candidatename", "candidatename"))


# lagged: ####
DV <- c("N", "N_relative", "mentions", "negative_mentions", "negative_article", "negative_headlead", "negative_binary_article_relative", "negative_binary_headlead_relative", "negative_binary_strong_article_relative", "negative_binary_strong_headlead_relative")
diff_by_day %<>% bind_cols(., arrange(diff_by_day, candidatename) %>% lag(n = 30) %>% select(DV) %>% rename_with(.fn = ~ paste0(.x, "_lagged"))) %>% mutate(across(contains("_lagged"), ~ ifelse(date <= 30, NA, .x)))

{
  (lm_N_1_lagged <- lm(N_lagged ~ date * own_period + date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
  (lm_N_2_log <- lm(ifelse(is.finite(log(N_lagged)), log(N_lagged), 0) ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_N_3 <- lm(N_lagged ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_N_4 <- lm(N_lagged ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_N_5 <- lm(N_lagged ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  regtab_N <- stargazer(lm_N_1, lm_N_2, lm_N_3, lm_N_4, lm_N_5, type = "text")
  writeLines(regtab_N, "analyses/regtab_N.txt", useBytes = T)
  
  (lm_relative_N_1 <- lm(N_relative ~ date * own_period + date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
  (lm_relative_N_2 <- lm(N_relative ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_relative_N_3 <- lm(N_relative ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_relative_N_4 <- lm(N_relative ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_relative_N_5 <- lm(N_relative ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  regtab_relative_N <- stargazer(lm_relative_N_1, lm_relative_N_2, lm_relative_N_3, lm_relative_N_4, lm_relative_N_5, type = "text")
  writeLines(regtab_relative_N, "analyses/regtab_relative_N.txt", useBytes = T)
  
  (lm_mentions_1 <- lm(mentions ~ date * own_period + date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
  (lm_mentions_2 <- lm(mentions ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_mentions_3 <- lm(mentions ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_mentions_4 <- lm(mentions ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_mentions_5 <- lm(mentions ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  regtab_mentions <- stargazer(lm_mentions_1, lm_mentions_2, lm_mentions_3, lm_mentions_4, lm_mentions_5, type = "text")
  writeLines(regtab_mentions, "analyses/regtab_mentions.txt", useBytes = T)
  
  (lm_negative_mentions_1 <- lm(negative_mentions ~ date * own_period+ date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
  (lm_negative_mentions_2 <- lm(negative_mentions ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_mentions_3 <- lm(negative_mentions ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_mentions_4 <- lm(negative_mentions ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_mentions_5 <- lm(negative_mentions ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  regtab_negative_mentions <- stargazer(lm_negative_mentions_1, lm_negative_mentions_2, lm_negative_mentions_3, lm_negative_mentions_4, lm_negative_mentions_5, type = "text")
  writeLines(regtab_negative_mentions, "analyses/regtab_negative_mentions.txt", useBytes = T)
  
  (lm_negative_article_1 <- lm(negative_article ~ date * own_period+ date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
  (lm_negative_article_2 <- lm(negative_article ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_article_3 <- lm(negative_article ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_article_4 <- lm(negative_article ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_article_5 <- lm(negative_article ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  regtab_negative_article <- stargazer(lm_negative_article_1, lm_negative_article_2, lm_negative_article_3, lm_negative_article_4, lm_negative_article_5, type = "text")
  writeLines(regtab_negative_article, "analyses/regtab_negative_article.txt", useBytes = T)
  
  (lm_negative_headlead_1 <- lm(negative_headlead ~ date * own_period+ date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
  (lm_negative_headlead_2 <- lm(negative_headlead ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_headlead_3 <- lm(negative_headlead ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_headlead_4 <- lm(negative_headlead ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_headlead_5 <- lm(negative_headlead ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  regtab_negative_headlead <- stargazer(lm_negative_headlead_1, lm_negative_headlead_2, lm_negative_headlead_3, lm_negative_headlead_4, lm_negative_headlead_5, type = "text")
  writeLines(regtab_negative_headlead, "analyses/regtab_negative_headlead.txt", useBytes = T)
  
  (lm_negative_binary_article_relative_1 <- lm(negative_binary_article_relative ~ date * own_period+ date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
  (lm_negative_binary_article_relative_2 <- lm(negative_binary_article_relative ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_binary_article_relative_3 <- lm(negative_binary_article_relative ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_binary_article_relative_4 <- lm(negative_binary_article_relative ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_binary_article_relative_5 <- lm(negative_binary_article_relative ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  regtab_negative_binary_article_relative <- stargazer(lm_negative_binary_article_relative_1, lm_negative_binary_article_relative_2, lm_negative_binary_article_relative_3, lm_negative_binary_article_relative_4, lm_negative_binary_article_relative_5, type = "text")
  writeLines(regtab_negative_binary_article_relative, "analyses/regtab_negative_binary_article_relative.txt", useBytes = T)
  
  (lm_negative_binary_headlead_relative_1 <- lm(negative_binary_headlead_relative ~ date * own_period+ date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
  (lm_negative_binary_headlead_relative_2 <- lm(negative_binary_headlead_relative ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_binary_headlead_relative_3 <- lm(negative_binary_headlead_relative ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_binary_headlead_relative_4 <- lm(negative_binary_headlead_relative ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_binary_headlead_relative_5 <- lm(negative_binary_headlead_relative ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  regtab_negative_binary_headlead_relative <- stargazer(lm_negative_binary_headlead_relative_1, lm_negative_binary_headlead_relative_2, lm_negative_binary_headlead_relative_3, lm_negative_binary_headlead_relative_4, lm_negative_binary_headlead_relative_5, type = "text")
  writeLines(regtab_negative_binary_headlead_relative, "analyses/regtab_negative_binary_headlead_relative.txt", useBytes = T)
  
  (lm_negative_binary_strong_article_relative_1 <- lm(negative_binary_strong_article_relative ~ date * own_period+ date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
  (lm_negative_binary_strong_article_relative_2 <- lm(negative_binary_strong_article_relative ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_binary_strong_article_relative_3 <- lm(negative_binary_strong_article_relative ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_binary_strong_article_relative_4 <- lm(negative_binary_strong_article_relative ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_binary_strong_article_relative_5 <- lm(negative_binary_strong_article_relative ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  regtab_negative_binary_strong_article_relative <- stargazer(lm_negative_binary_strong_article_relative_1, lm_negative_binary_strong_article_relative_2, lm_negative_binary_strong_article_relative_3, lm_negative_binary_strong_article_relative_4, lm_negative_binary_strong_article_relative_5, type = "text")
  writeLines(regtab_negative_binary_strong_article_relative, "analyses/regtab_negative_binary_strong_article_relative.txt", useBytes = T)
  
  (lm_negative_binary_strong_headlead_relative_1 <- lm(negative_binary_strong_headlead_relative ~ date * own_period+ date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
  (lm_negative_binary_strong_headlead_relative_2 <- lm(negative_binary_strong_headlead_relative ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_binary_strong_headlead_relative_3 <- lm(negative_binary_strong_headlead_relative ~ date * rank_top * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_binary_strong_headlead_relative_4 <- lm(negative_binary_strong_headlead_relative ~ date * rank_top2  * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  (lm_negative_binary_strong_headlead_relative_5 <- lm(negative_binary_strong_headlead_relative ~ relative_share * candidatename +  own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
  regtab_negative_binary_strong_headlead_relative <- stargazer(lm_negative_binary_strong_headlead_relative_1, lm_negative_binary_strong_headlead_relative_2, lm_negative_binary_strong_headlead_relative_3, lm_negative_binary_strong_headlead_relative_4, lm_negative_binary_strong_headlead_relative_5, type = "text")
  writeLines(regtab_negative_binary_strong_headlead_relative, "analyses/regtab_negative_binary_strong_headlead_relative.txt", useBytes = T)
}


# broom::tidy(lm_N1) 
# stargazer::stargazer(lm_negative1, lm_mentions1, lm_N1, type = "text") # NOTE: you MUST include results = "asis" in the chunk header for this to be visible once knitted



