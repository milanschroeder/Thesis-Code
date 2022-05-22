# 06 Regression

# prepare data: ####


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
                        
                        candidatename = factor(candidatename, levels = c( "none", "laschet", "baerbock", "scholz")),
                        
                        baerbock = ifelse(candidatename == "baerbock", 1, 0),
                        laschet = ifelse(candidatename == "laschet", 1, 0),
                        scholz = ifelse(candidatename == "scholz", 1, 0)
                        
)




# regression: ####

DV <- c("N", "N_relative", "mentions", "negative_mentions", "negative_article", "negative_headlead", "negative_binary_article_relative", "negative_binary_headlead_relative", "negative_binary_strong_article_relative", "negative_binary_strong_headlead_relative")


# broom::tidy(lm_N1) 
# stargazer::stargazer(lm_negative1, lm_mentions1, lm_N1, type = "text") # NOTE: you MUST include results = "asis" in the chunk header for this to be visible once knitted

(lm_N_1 <- lm(N ~ date * own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
(lm_N_2 <- lm(N ~ date * own_period + date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_N_3 <- lm(N ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_N_4 <- lm(N ~ date * own_period + candidatename * date * own_period   + relative_share * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_N_5 <- lm(N ~ date * own_period + candidatename * date * rank_top, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_N_6 <- lm(N ~ date * own_period + candidatename * date * rank_top2, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
regtab_N <- stargazer(lm_N_1, lm_N_2, lm_N_3, lm_N_4, lm_N_5, lm_N_6, type = "text")
writeLines(regtab_N, "analyses/regtab_N.txt", useBytes = T)

(lm_relative_N_1 <- lm(N_relative ~ date * own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
(lm_relative_N_2 <- lm(N_relative ~ date * own_period + date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_relative_N_3 <- lm(N_relative ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_relative_N_4 <- lm(N_relative ~ date * own_period + candidatename * date * own_period  + relative_share * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_relative_N_5 <- lm(N_relative ~ date * own_period + candidatename * date * rank_top, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_relative_N_6 <- lm(N_relative ~ date * own_period + candidatename * date * rank_top2, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
regtab_relative_N <- stargazer(lm_relative_N_1, lm_relative_N_2, lm_relative_N_3, lm_relative_N_4, lm_relative_N_5, lm_relative_N_6, type = "text")
writeLines(regtab_relative_N, "analyses/regtab_relative_N.txt", useBytes = T)

(lm_mentions_1 <- lm(mentions ~ date * own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
(lm_mentions_2 <- lm(mentions ~ date * own_period + date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_mentions_3 <- lm(mentions ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_mentions_4 <- lm(mentions ~ date * own_period + candidatename * date * own_period  + relative_share * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_mentions_5 <- lm(mentions ~ date * own_period + candidatename * date * rank_top, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_mentions_6 <- lm(mentions ~ date * own_period + candidatename * date * rank_top2, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
regtab_mentions <- stargazer(lm_mentions_1, lm_mentions_2, lm_mentions_3, lm_mentions_4, lm_mentions_5, lm_mentions_6, type = "text")
writeLines(regtab_mentions, "analyses/regtab_mentions.txt", useBytes = T)

(lm_negative_mentions_1 <- lm(negative_mentions ~ date * own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
(lm_negative_mentions_2 <- lm(negative_mentions ~ date * own_period + date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_mentions_3 <- lm(negative_mentions ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_mentions_4 <- lm(negative_mentions ~ date * own_period + candidatename * date * own_period  + relative_share * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_mentions_5 <- lm(negative_mentions ~ date * own_period + candidatename * date * rank_top, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_mentions_6 <- lm(negative_mentions ~ date * own_period + candidatename * date * rank_top2, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
regtab_negative_mentions <- stargazer(lm_negative_mentions_1, lm_negative_mentions_2, lm_negative_mentions_3, lm_negative_mentions_4, lm_negative_mentions_5, lm_negative_mentions_6, type = "text")
writeLines(regtab_negative_mentions, "analyses/regtab_negative_mentions.txt", useBytes = T)

(lm_negative_article_1 <- lm(negative_article ~ date * own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
(lm_negative_article_2 <- lm(negative_article ~ date * own_period + date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_article_3 <- lm(negative_article ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_article_4 <- lm(negative_article ~ date * own_period + candidatename * date * own_period  + relative_share * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_article_5 <- lm(negative_article ~ date * own_period + candidatename * date * rank_top, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_article_6 <- lm(negative_article ~ date * own_period + candidatename * date * rank_top2, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
regtab_negative_article <- stargazer(lm_negative_article_1, lm_negative_article_2, lm_negative_article_3, lm_negative_article_4, lm_negative_article_5, lm_negative_article_6, type = "text")
writeLines(regtab_negative_article, "analyses/regtab_negative_article.txt", useBytes = T)

(lm_negative_headlead_1 <- lm(negative_headlead ~ date * own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
(lm_negative_headlead_2 <- lm(negative_headlead ~ date * own_period + date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_headlead_3 <- lm(negative_headlead ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_headlead_4 <- lm(negative_headlead ~ date * own_period + candidatename * date * own_period  + relative_share * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_headlead_5 <- lm(negative_headlead ~ date * own_period + candidatename * date * rank_top, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_headlead_6 <- lm(negative_headlead ~ date * own_period + candidatename * date * rank_top2, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
regtab_negative_headlead <- stargazer(lm_negative_headlead_1, lm_negative_headlead_2, lm_negative_headlead_3, lm_negative_headlead_4, lm_negative_headlead_5, lm_negative_headlead_6, type = "text")
writeLines(regtab_negative_headlead, "analyses/regtab_negative_headlead.txt", useBytes = T)

(lm_negative_binary_article_relative_1 <- lm(negative_binary_article_relative ~ date * own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
(lm_negative_binary_article_relative_2 <- lm(negative_binary_article_relative ~ date * own_period + date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_binary_article_relative_3 <- lm(negative_binary_article_relative ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_binary_article_relative_4 <- lm(negative_binary_article_relative ~ date * own_period + candidatename * date * own_period  + relative_share * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_binary_article_relative_5 <- lm(negative_binary_article_relative ~ date * own_period + candidatename * date * rank_top, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_binary_article_relative_6 <- lm(negative_binary_article_relative ~ date * own_period + candidatename * date * rank_top2, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
regtab_negative_binary_article_relative <- stargazer(lm_negative_binary_article_relative_1, lm_negative_binary_article_relative_2, lm_negative_binary_article_relative_3, lm_negative_binary_article_relative_4, lm_negative_binary_article_relative_5, lm_negative_binary_article_relative_6, type = "text")
writeLines(regtab_negative_binary_article_relative, "analyses/regtab_negative_binary_article_relative.txt", useBytes = T)

(lm_negative_binary_headlead_relative_1 <- lm(negative_binary_headlead_relative ~ date * own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
(lm_negative_binary_headlead_relative_2 <- lm(negative_binary_headlead_relative ~ date * own_period + date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_binary_headlead_relative_3 <- lm(negative_binary_headlead_relative ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_binary_headlead_relative_4 <- lm(negative_binary_headlead_relative ~ date * own_period + candidatename * date * own_period  + relative_share * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_binary_headlead_relative_5 <- lm(negative_binary_headlead_relative ~ date * own_period + candidatename * date * rank_top, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_binary_headlead_relative_6 <- lm(negative_binary_headlead_relative ~ date * own_period + candidatename * date * rank_top2, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
regtab_negative_binary_headlead_relative <- stargazer(lm_negative_binary_headlead_relative_1, lm_negative_binary_headlead_relative_2, lm_negative_binary_headlead_relative_3, lm_negative_binary_headlead_relative_4, lm_negative_binary_headlead_relative_5, lm_negative_binary_headlead_relative_6, type = "text")
writeLines(regtab_negative_binary_headlead_relative, "analyses/regtab_negative_binary_headlead_relative.txt", useBytes = T)

(lm_negative_binary_strong_article_relative_1 <- lm(negative_binary_strong_article_relative ~ date * own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
(lm_negative_binary_strong_article_relative_2 <- lm(negative_binary_strong_article_relative ~ date * own_period + date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_binary_strong_article_relative_3 <- lm(negative_binary_strong_article_relative ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_binary_strong_article_relative_4 <- lm(negative_binary_strong_article_relative ~ date * own_period + candidatename * date * own_period  + relative_share * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_binary_strong_article_relative_5 <- lm(negative_binary_strong_article_relative ~ date * own_period + candidatename * date * rank_top, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_binary_strong_article_relative_6 <- lm(negative_binary_strong_article_relative ~ date * own_period + candidatename * date * rank_top2, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
regtab_negative_binary_strong_article_relative <- stargazer(lm_negative_binary_strong_article_relative_1, lm_negative_binary_strong_article_relative_2, lm_negative_binary_strong_article_relative_3, lm_negative_binary_strong_article_relative_4, lm_negative_binary_strong_article_relative_5, lm_negative_binary_strong_article_relative_6, type = "text")
writeLines(regtab_negative_binary_strong_article_relative, "analyses/regtab_negative_binary_strong_article_relative.txt", useBytes = T)

(lm_negative_binary_strong_headlead_relative_1 <- lm(negative_binary_strong_headlead_relative ~ date * own_period, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary() 
(lm_negative_binary_strong_headlead_relative_2 <- lm(negative_binary_strong_headlead_relative ~ date * own_period + date * pre_campaign + date * post_campaign, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_binary_strong_headlead_relative_3 <- lm(negative_binary_strong_headlead_relative ~ date * own_period * candidatename + date * pre_campaign + date * post_campaign * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_binary_strong_headlead_relative_4 <- lm(negative_binary_strong_headlead_relative ~ date * own_period + candidatename * date * own_period  + relative_share * candidatename, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_binary_strong_headlead_relative_5 <- lm(negative_binary_strong_headlead_relative ~ date * own_period + candidatename * date * rank_top, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_negative_binary_strong_headlead_relative_6 <- lm(negative_binary_strong_headlead_relative ~ date * own_period + candidatename * date * rank_top2, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
regtab_negative_binary_strong_headlead_relative <- stargazer(lm_negative_binary_strong_headlead_relative_1, lm_negative_binary_strong_headlead_relative_2, lm_negative_binary_strong_headlead_relative_3, lm_negative_binary_strong_headlead_relative_4, lm_negative_binary_strong_headlead_relative_5, lm_negative_binary_strong_headlead_relative_6, type = "text")
writeLines(regtab_negative_binary_strong_headlead_relative, "analyses/regtab_negative_binary_strong_headlead_relative.txt", useBytes = T)







# old: ####



(lm_N_full <-  lm(N ~  date * own_period + own_period * candidatename + I(candidatename * rank_top2) + campaign_started * date + candidatename * relative_share, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()


assign(paste0("lm_", DV[1]), lm(eval(parse(text = DV[1])) ~  date * campaign_started + own_period*candidatename + candidatename * rank_top2, data = diff_by_day %>% filter(candidatename != "none")))



assign(paste0("lm_", DV[1]), lm(eval(parse(text = DV[1])) ~  date * campaign_started + own_period*candidatename + candidatename * rank_top2, data = diff_by_day %>% filter(candidatename != "none")))





(lm_mentions <- lm(mentions ~  I(date * campaign_ended) + own_period*candidatename + candidatename * rank_top2, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
(lm_mentions <- lm(N ~  date * campaign_started + own_period*candidatename + candidatename * rank_top2, data = diff_by_day %>% filter(candidatename != "none"))) %>% summary()
# headlines:

p_load(texreg)
p_load(finalfit, knitr)

finalfit(.data = diff_by_day %>% filter(candidatename != "none"), dependent = "N", metrics = T, explanatory = c("date", "campaign_started", "own_period", "candidatename", "candidatename"))
















stargazer::stargazer(name_of_model_1, name_of_model_2, type = "format of output") # NOTE: you MUST include results = "asis" in the chunk header for this to be visible once knitted
texreg::screenreg(list(model1, model2),
                  digits = 3,
                  custom.model.names = c("model1","model2"),
                  custom.coef.names = c("coef1","coef2"))

# plot(diff_by_day$date, diff_by_day$mentions)

(firstmodel_l <- lm(mentions ~  date + share_cdu , data = diff_by_day %>% filter(candidatename == "laschet"))) %>% summary()
(firstmodel_s <- lm(mentions ~  I(date *  + share_spd), data = diff_by_day %>% filter(candidatename == "scholz"))) %>% summary()


# using diff:
lm(negative_leader ~ period * week, data = diff_by_week %>% mutate(week = week * week)) %>% summary()
# interact with RT bc not only level change expected!

summary(firstmodel_b)
