# 03 plots

library(pacman)
p_load(tidyverse, grid, ggnewscale, magrittr, lubridate)


#prepare helpers: ####
partycolors <- c("green", "red", "black", "yellow", "brown", "purple", "gray")
names(partycolors) <- c("gru", "spd", "cdu", "fdp", "afd", "lin", "oth")

candidatecolors <- c("green", "red", "black")
names(candidatecolors) <- c("baerbock", "scholz", "laschet")

parties <- c("cdu", "spd", "gru", "lin", "fdp", "afd", "oth")

# get leading periods for each party (leaving out transition periods)
first_cdu <- seq.Date(from = min(polls_wide$date[polls_wide$firstrank_cdu == 1 & polls_wide$overlap_cdu == 0]),
                      to = min(polls_wide$date[polls_wide$overlap_cdu == 1]),
                      by = "day")
first_gru <- seq.Date(from = min(polls_wide$date[polls_wide$firstrank_gru == 1 & polls_wide$overlap_gru == 0]),
                      to = max(polls_wide$date[polls_wide$firstrank_gru == 1 & polls_wide$overlap_gru == 0]),
                      by = "day")
first_spd <- seq.Date(from = min(polls_wide$date[polls_wide$overlap_spd == 0 & polls_wide$overlap_cdu == 0 & polls_wide$firstrank_spd == 1]),
                      to = as.Date("2021-09-26"),
                      by = "day")
second_cdu <- seq.Date(from = min(polls_wide$date[polls_wide$overlap_cdu == 0 & polls_wide$overlap_gru == 0 & polls_wide$firstrank_cdu == 1]),
                       to = max(polls_wide$date[polls_wide$overlap_spd == 0 & polls_wide$overlap_cdu == 0 & polls_wide$firstrank_cdu == 1]),
                       by = "day")

# define background areas
rects_nogaps <- data.frame(ymin = rep(-4, 4), 
                           ymax = rep(Inf, 4), 
                           alpha = rep(.01, 4), 
                           xmin = as.Date(c("2020-12-01", "2021-04-26", "2021-05-16", "2021-08-24")), 
                           xmax = as.Date(c("2021-04-26", "2021-05-16", "2021-08-24", "2021-09-26")),
                           fill = c("gray40", "greenyellow", "gray40", "lightcoral")
)
rects_gaps <- data.frame(ymin = rep(-4, 4), 
                         ymax = rep(Inf, 4), 
                         alpha = rep(.01, 4), 
                         xmin = as.Date(c("2020-12-01", "2021-04-28", "2021-05-25", "2021-08-30")), 
                         xmax = as.Date(c("2021-04-21", "2021-05-08", "2021-08-18", "2021-09-26")),
                         fill = c("gray40", "greenyellow", "gray40", "lightcoral")
)




# import data: ####
load("articles/RTcorpus.RData")
load("polls/all_polls.RData")

# plot RT search results by candidate/day:
ggplot() +
  geom_smooth(data = summariesRT_long, mapping = aes(colour = candidate, y = articles, x = date))

# zweitstimme predictions ####

# see how good zweitstimme predictions (t-100) fitted actual polls: 
  # good trend prediction for all but CDU & SPD (and to much lesser degree FDP) 
(gg_zweitstimme_performance <- 
  ggplot() +
  geom_point(polls_long, mapping = aes(x = date, y = percent, colour = party)) +
  geom_point(zweitstimme_polls_long, mapping = aes(x = date, y = share, colour = party)) +
  geom_smooth(polls_est_long, mapping = aes(x = date, y = share*100, colour = party)) +
  scale_fill_manual(aesthetics = c("colour", "fill"), values = partycolors) +
#  coord_cartesian(ylim = c(0, 40)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
 geom_vline(xintercept = max(zweitstimme_polls_long$date), linetype = 2)
)
  # conclusion: better use actual polling numbers



# plot actual polls ####

# ToDo: split up: trendline and points(?)

# full polls of top3 parties
(gg_leading_parties_trend <-
ggplot(polls_long %>% filter(party %in% parties[1:3]), 
       aes(colour = party, fill = party)) +
  geom_line(aes(x = date, y = mean)) +
  geom_point(aes(x = date, y = percent)) +
  geom_ribbon(aes(date, ymin = lower, ymax = upper, colour = NULL), alpha = .1) +
  scale_fill_manual(aesthetics = c("colour", "fill"), values = partycolors[1:3]) +
#  layers$background +
#  coord_cartesian(ylim = c(0, 40)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b")
) # %>% ggplotly(gg_all_parties_poll)


# polls of all parties
(gg_all_parties_trend <-
    ggplot(polls_long, 
           aes(colour = party, fill = party)) +
    geom_line(aes(x = date, y = mean)) +
    geom_point(aes(x = date, y = percent)) +
    geom_ribbon(aes(date, ymin = lower, ymax = upper, colour = NULL), alpha = .1) +
    scale_fill_manual(aesthetics = c("colour", "fill"), values = partycolors) +
    coord_cartesian(ylim = c(0, 40)) + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b")
) # %>% ggplotly(gg_all_parties_poll)


# define layers to add to plots ####

layers <- vector('list')
layers$scalex <- scale_x_date(date_breaks = "1 month", date_labels = "%b")
layers$partycolors_all <- scale_fill_manual(aesthetics = c("colour", "fill"), values = partycolors)
layers$partycolors_three <- scale_fill_manual(aesthetics = c("colour", "fill"), values = partycolors[1:3])
layers$candidatecolors <- scale_fill_manual(aesthetics = c("colour", "fill"), values = candidatecolors)
layers$background <- geom_rect(data = rects_nogaps, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = list("gray40", "greenyellow", "gray40", "lightcoral"), alpha = .01), show.legend = F)
layers$background_gaps <- geom_rect(data = rects_gaps, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = list("gray40", "greenyellow", "gray40", "lightcoral"), alpha = .1), show.legend = F)
layers$newscale_fill <- ggnewscale::new_scale_fill()
layers$newscale_colour <- ggnewscale::new_scale_colour()
layers$trendline_three <- geom_line(data = polls_long %>% filter(party %in% parties[1:3]), mapping = aes(colour = party, x = date, y = mean))
layers$trends_ci_three <- geom_ribbon(data = polls_long %>% filter(party %in% parties[1:3]), mapping = aes(fill = party, x = date, ymin = lower, ymax = upper, colour = NULL), alpha = .2)
layers$articles_RT_candidates_n <- geom_smooth(data = summariesRT_long %>% filter(candidate != "all"), mapping = aes(x = date, y = articles, colour = candidate), se = F)
layers$articles_RT_candidates_perc <- geom_smooth(data = summariesRT_long %>% filter(candidate != "all"), mapping = aes(x = date, y = share, colour = candidate), se = F)
# producing graph backgrounds ####  

# leaving gaps where close
(gg_leading_party_background_gaps <- 
  ggplot() + 
  layers$background_gaps
)

# until clearly passed
(gg_leading_party_background <- 
  ggplot() +
  layers$background
)

# try out stuff:

# ggplot() + 
#   layers$background_gaps +
(gg_parties_trend <- 
gg_leading_party_background_gaps +
    new_scale_fill() +
  scale_fill_manual(aesthetics = c("colour", "fill"), values = partycolors) +
  geom_line(data = polls_long, mapping = aes(colour = party, x = date, y = mean)) +
geom_ribbon(data = polls_long, mapping = aes(fill = party, x = date, ymin = lower, ymax = upper, colour = NULL), alpha = .2) +
    geom_point(aes(x = date, y = percent, colour = party), data = polls_long) +
    geom_vline(aes(xintercept = as.Date("2021-04-20")), colour = "black", linetype = "dashed") +
    geom_vline(aes(xintercept = as.Date("2021-04-19")), colour = "green", linetype = "dashed") +
    geom_vline(aes(xintercept = as.Date("2020-08-10")), colour = "red", linetype = "dashed") +
  coord_cartesian(xlim = c(as.Date("2020-12-31"), as.Date("2021-09-26")), ylim = c(0, 40)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = "Date", y = "Polling numbers [%]", colour = "Party") +
  guides(color = FALSE) +
    theme_minimal()
)
  ggsave("plots/polls_trend.png")
  
  
  

ggplot() + layers$trendline_three
  coord_cartesian(xlim = c(as.Date("2020-12-31"), as.Date("2022-01-01"))) 
  labs(x = "Date", y = "Number of articles")
  layers$trends_ci_three 
  guides(color = FALSE)




# useful plots ####

# polling trends and leaders over time:
gg_leading_party_background +
  new_scale_fill() +
  layers$partycolors_three +
  layers$scalex +
  layers$trendline_three +
  layers$trends_ci_three

# RT articles mentioning candidates over time:
 gg_leading_party_background +
   new_scale_fill() +
   layers$candidatecolors +
  layers$scalex +
  layers$articles_candidates_n +
  # useful to make this a layer? 
  geom_smooth(data = summariesRT_long %>% filter(candidate == "all"), 
           mapping = aes(x = as.Date(date), y = articles), colour = "grey", linetype = "longdash")
  coord_cartesian(ylim = c(0,3))
  
  # share of articles mentioning candidates (aka zooming in)
  gg_leading_party_background +
    new_scale_fill() +
    layers$articles_candidates_perc +
    coord_cartesian(ylim = c(0,.1)) +
    layers$candidatecolors
#  geom_smooth(data = summariesRT_long %>% filter(candidate != "all"), mapping = aes(x = date, y = share, colour = candidate), se = F)

# compare search summaries to actually scrapable numbers: ####
  load("articles/RTcorpus.RData")

RT_coverage <- corpus_RT %>% 
  group_by(date) %>% 
    summarize(all = n()) %>% 
  mutate(date = as.POSIXct(date)) %>% 
  filter(between(date, as.POSIXct("2021-01-01"), as.POSIXct("2022-01-01"))) %>% 
  left_join(., summariesRT, by = "date", suffix = c("_actual", "_reported")) %>% 
  mutate(date = as.Date(date),
         week = week(date))

(gg_scraping_coverage <- 
  gg_leading_party_background +
  geom_freqpoly(aes(date, all_reported), data = RT_coverage, stat = "smooth", formula = "y ~ poly(x, 2)", colour = "red") +
  geom_freqpoly(aes(date, all_actual), data = RT_coverage, stat = "smooth", formula = "y ~ poly(x, 2)", colour = "blue") +
    geom_linerange(aes(x = date, ymin = all_actual, ymax = all_reported), data = RT_coverage) + 
    geom_point(aes(date, all_reported), data = RT_coverage, colour = "red") +
    geom_point(aes(date, all_actual), data = RT_coverage, colour = "blue") +
    geom_freqpoly(aes(date, all_reported - all_actual), data = RT_coverage, stat = "smooth", formula = "y ~ poly(x, 2)", color = "white", se = F) +
    coord_cartesian(xlim = c(as.Date("2020-12-31"), as.Date("2022-01-01")), ylim = c(0, max(RT_coverage$all_reported+5))) + 
    labs(x = "Date", y = "Number of articles")
)    
ggsave("plots/rt_coverage.png")

# by week:
RT_coverage_week <- RT_coverage %>% dplyr::select(-date) %>% group_by(., week) %>% summarize(across(.cols = everything(), .fns = sum)) %>% 
  # exclude incomplete weeks:
  filter(week != min(week) & week != max(week))

(gg_scraping_coverage_week <- ggplot() +
  geom_freqpoly(aes(week, all_reported), data = RT_coverage_week, stat = "smooth", formula = "y ~ poly(x, 2)", colour = "red") +
  geom_freqpoly(aes(week, all_actual), data = RT_coverage_week, stat = "smooth", formula = "y ~ poly(x, 2)", colour = "blue") +
  # better: vertical lines?
  geom_linerange(aes(x = week, ymin = all_actual, ymax = all_reported), data = RT_coverage_week) + 
  geom_point(aes(week, all_reported), data = RT_coverage_week, colour = "red") +
  geom_point(aes(week, all_actual), data = RT_coverage_week, colour = "blue") +
  geom_freqpoly(aes(week, all_reported - all_actual), stat = "smooth", formula = "y ~ poly(x, 2)", data = RT_coverage_week, color = "white", se = F) +
  coord_cartesian(ylim = c(0, max(RT_coverage_week$all_reported+10))) + 
  labs(x = "Week", y = "Number of articles")
)   
cor(RT_coverage_week$all_reported, RT_coverage_week$all_actual) # .71 (previously .71... what changed?) .85

# only lines:
ggplot() +
  geom_freqpoly(aes(week, all_reported), data = RT_coverage_week, stat = "smooth", formula = "y ~ poly(x, 2)", colour = "red") +
  geom_freqpoly(aes(week, all_actual), data = RT_coverage_week, stat = "smooth", formula = "y ~ poly(x, 2)", colour = "blue")  

cor(RT_coverage$all_reported, RT_coverage$all_actual) # .69
# per candidate:


# Factchecks: ####
# vertical lines?
  # also for other important events?


# sentiment plots ####

# negativity distribution:
(gg_sent_score_text_source <- 
analysis_candidate_data %>% 
  ggplot() +
  geom_hline(aes(yintercept = 0)) +
  geom_violin(aes(x = source, y = article_sent_article, fill = source), scale = "count", draw_quantiles = .5, show.legend = F) +
  labs(x = "", y = "Sentiment score") +
  theme_minimal()
)
ggsave("plots/sent_score_text.png")


(gg_sent_score_headlead_source <- 
  analysis_data %>% 
  ggplot() +
  geom_hline(aes(yintercept = 0)) +
  geom_violin(aes(x = source, y = article_sent_headlead, fill = source), scale = "count", draw_quantiles = .5, show.legend = F) + 
  scale_fill_brewer(palette = "Set2") +
  labs(x = "", y = "Sentiment score") +
  theme_minimal()
)
ggsave("plots/sent_score_header.png")
  
# time-series (average sentiment, pos/neg-classification per period)






  
# save all: ####
save(list = c("layers", 
                "gg_leading_party_background", "gg_leading_party_background_gaps", 
#                "gg_zweitstimme_performance", 
              "gg_leading_parties_trend", "gg_all_parties_trend"), 
       file = "plots/all_plots_layers.RData")

load("plots/all_plots_layers.RData")

# save results: ####
save(list = c("gg_zweitstimme_performance", 
              "gg_parties_trend",
              "gg_scraping_coverage", "gg_scraping_coverage_week"), 
     file = "plots/result_plots.RData")

load("plots/result_plots.RData")





