# 04_additional analyses:

library(dplyr)

# frequency of op-eds:
# candidate related only
all_sources_candidates %>% filter(source != "BILD") %>% group_by(source, resort) %>% summarise(N = n()) %>% arrange(desc(N))

all_sources %>% filter(source == "taz - die tageszeitung") %>% group_by(resort) %>% summarise(N = n()) %>% arrange(desc(N))
267/25469
all_sources %>% filter(source == "Süddeutsche Zeitung") %>% group_by(resort) %>% summarise(N = n()) %>% arrange(desc(N))
362/81470
all_sources %>% filter(source == "Frankfurter Rundschau") %>% group_by(resort) %>% summarise(N = n()) %>% arrange(desc(N))
208/50011
all_sources %>% filter(source == "RTDE") %>% group_by(resort) %>% summarise(N = n()) %>% arrange(desc(N))
570/9412
all_sources %>% filter(source == "Die Welt") %>% group_by(resort) %>% summarise(N = n()) %>% arrange(desc(N))
310/15598


# for Bild not computable/not uniquely identifiable resort

# summary tables:

byday_all_long %>% filter(variable == "")
  dplyr::group_by(source, candidate) %>% 
  summarize(n = n())  
  
  
 #  dplyr::select(articles_per_daynegative = negative_binary_article, overall_sent, classified_tokens = share_classified_toks) %>% 
 #  dplyr::summarise_all(fmean) %>% # uses the summarize function to all the vars we selected 
 # # t() %>% # To transpose the df 
  as.data.frame() %>%  
  # dplyr::rename(Male = V1, Female = V2) %>% # call the columns by what they are 
  dplyr::mutate_if(is.numeric, ~ round(.x, 3)) %>% 
  knitr::kable() %>% 
  kableExtra::kable_styling()



# NOT: most linked articles (using mehrlink and intextlinks)

# NOT: resort/topic over time

# NOT: N Types (i.e. unique tokens)
# NOT: ratio unique tokens/tokens/sentences
# NOT: daytime of posts

# classification performance (and source comparison to identify possible strange language patterns at RT)
analysis_data %>% 
ggplot() +
  geom_density(aes(share_classified_toks, group = source), linetype = "dashed", colour = "gray", binwidth = .01, alpha = .2) +
  geom_density(aes(share_classified_toks, fill = source_bin, colour = source_bin), linetype = "dashed", binwidth = .01, alpha = .3) +
  geom_density(aes(share_classified, group = source), colour = "gray", binwidth = .01, alpha = .2) +
  geom_density(aes(share_classified, fill = source_bin, colour = source_bin), binwidth = .01, alpha = .3) +
  theme_minimal() +
  labs(x = "Classification rate", y = "Count", fill = "Source") +
  guides(colour = "none")
ggsave("plots/classification_rate.png")


# bad:
ggplot() + geom_smooth(aes(date, leader_focus_difference_negative, colour = period, fill = period), analysis_data) + facet_wrap(vars(RT))

(negativity_by_period <- 
analysis_data %>% 
  group_by(source_bin, period) %>% 
  summarize(across(.cols = c("negative_baerbock", "negative_laschet", "negative_scholz"), .fns = ~ mean(.x, na.rm = T))) %>%  
  pivot_longer(cols = c("negative_baerbock", "negative_laschet", "negative_scholz"), names_to = c("variable", "candidate"), values_to = "negative", names_sep = "_") %>% 
#  mutate(negative = ifelse(source_bin != "RT", negative / 5, negative)) %>%
    filter(period != "interim") %>% 
    ggplot() +
  scale_fill_manual(values = candidatecolors[1:3]) +
  geom_col(aes(factor(period, levels = c("before", "green_period", "black_period", "red_period", "after")), negative, fill = candidate), position = "dodge") + 
  facet_wrap(vars(source_bin)) +
  theme_minimal() +
  guides(fill = "none") +
  labs(y = "share of negative articles")  
) 


# Differnces. Significant?
analysis_data %>% 
  group_by(source) %>%
  summarize(across(.cols = negative_article, negative_headlead, .fns =  ~ mean(.x, na.rm = T)))
analysis_data %>% 
  group_by(baerbock) %>%
  summarize(across(.cols = negative_article, negative_headlead, .fns =  ~ mean(.x, na.rm = T)))
analysis_data %>% 
  group_by(laschet) %>%
  summarize(across(.cols = negative_article, negative_headlead, .fns =  ~ mean(.x, na.rm = T)))
analysis_data %>% 
  group_by(scholz) %>%
  summarize(across(.cols = negative_article, negative_headlead, .fns =  ~ mean(.x, na.rm = T)))

# crosstab:
table(analysis_data$source, analysis_data$baerbock)
table(analysis_data$source, analysis_data$laschet)
table(analysis_data$source, analysis_data$scholz)

# compare headlead sentiments

analysis_candidate_data <- analysis_data %>% filter(candidate == 1)  


(
crosstab_plot_sentscore_text <- 
ggplot() +
#  source mean
  geom_hline(aes(yintercept = mean(analysis_candidate_data$negative_article[analysis_candidate_data$RT == 1], na.rm = T)), colour = "orange", size = 2) +
  geom_hline(aes(yintercept = mean(analysis_data$negative_article[analysis_data$RT == 1], na.rm = T)), colour = "orange", linetype = "dashed", size = 2) +
#  geom_hline(aes(yintercept = mean(analysis_candidate_data$negative_headlead[analysis_candidate_data$RT == 1], na.rm = T)), colour = "orange", linetype = "longdash", size = 2) +
  geom_hline(aes(yintercept = mean(analysis_candidate_data$negative_article[analysis_candidate_data$RT == 0], na.rm = T)), colour = "gray", size = 2) +
#  geom_hline(aes(yintercept = mean(analysis_candidate_data$negative_headlead[analysis_candidate_data$RT == 0], na.rm = T)), colour = "gray", linetype = "longdash", size = 2) +

# candidate mean
  geom_vline(aes(xintercept = mean(analysis_candidate_data$negative_article[analysis_candidate_data$baerbock == 1], na.rm = T)), colour = "green", size = 2) +
#  geom_vline(aes(xintercept = mean(analysis_candidate_data$negative_headlead[analysis_candidate_data$baerbock == 1], na.rm = T)), colour = "green", linetype = "longdash", size = 2) +
  geom_vline(aes(xintercept = mean(analysis_candidate_data$negative_article[analysis_candidate_data$scholz == 1], na.rm = T)), colour = "red", size = 2) +
#  geom_vline(aes(xintercept = mean(analysis_candidate_data$negative_headlead[analysis_candidate_data$scholz == 1], na.rm = T)), colour = "red", linetype = "longdash", size = 2) +
  geom_vline(aes(xintercept = mean(analysis_candidate_data$negative_article[analysis_candidate_data$laschet == 1], na.rm = T)), colour = "black", size = 2) +
#  geom_vline(aes(xintercept = mean(analysis_candidate_data$negative_headlead[analysis_candidate_data$laschet == 1], na.rm = T)), colour = "black", linetype = "longdash", size = 2) +
  
  geom_point(aes(x = mean(analysis_candidate_data$negative_article[analysis_candidate_data$scholz == 1 & analysis_candidate_data$RT == 1], na.rm = T),
               y = mean(analysis_candidate_data$negative_article[analysis_candidate_data$scholz == 1 & analysis_candidate_data$RT == 1], na.rm = T)), 
           colour = "red", size = 7) +
  geom_point(aes(x = mean(analysis_candidate_data$negative_article[analysis_candidate_data$laschet == 1 & analysis_candidate_data$RT == 1], na.rm = T),
               y = mean(analysis_candidate_data$negative_article[analysis_candidate_data$laschet == 1 & analysis_candidate_data$RT == 1], na.rm = T)), 
           colour = "black", size = 7) +
  geom_point(aes(x = mean(analysis_candidate_data$negative_article[analysis_candidate_data$baerbock == 1 & analysis_candidate_data$RT == 1], na.rm = T),
                 y = mean(analysis_candidate_data$negative_article[analysis_candidate_data$baerbock == 1 & analysis_candidate_data$RT == 1], na.rm = T)),
             colour = "green", size = 7) +
  
    geom_text(aes(label = "B", x = mean(analysis_candidate_data$negative_article[analysis_candidate_data$scholz == 1 & analysis_candidate_data$source == "BILD"], na.rm = T),
                   y = mean(analysis_candidate_data$negative_article[analysis_candidate_data$scholz == 1 & analysis_candidate_data$source == "BILD"], na.rm = T)), 
               colour = "red", size = 5) +
    geom_text(aes(label = "B", x = mean(analysis_candidate_data$negative_article[analysis_candidate_data$laschet == 1 & analysis_candidate_data$source == "BILD"], na.rm = T),
                   y = mean(analysis_candidate_data$negative_article[analysis_candidate_data$laschet == 1 & analysis_candidate_data$source == "BILD"], na.rm = T)), 
               colour = "black", shape = 1, size = 5) +
    geom_text(aes(label = "B", x = mean(analysis_candidate_data$negative_article[analysis_candidate_data$baerbock == 1 & analysis_candidate_data$source == "BILD"], na.rm = T),
                       y = mean(analysis_candidate_data$negative_article[analysis_candidate_data$baerbock == 1 & analysis_candidate_data$source == "BILD"], na.rm = T)), 
                   colour = "green", shape = 1, size = 5) +
    
    geom_text(aes(label = "W", x = mean(analysis_candidate_data$negative_article[analysis_candidate_data$scholz == 1 & analysis_candidate_data$source == "Die Welt"], na.rm = T),
                   y = mean(analysis_candidate_data$negative_article[analysis_candidate_data$scholz == 1 & analysis_candidate_data$source == "Die Welt"], na.rm = T)), 
               colour = "red", shape = 2, size = 5) +
    geom_text(aes(label = "W", x = mean(analysis_candidate_data$negative_article[analysis_candidate_data$laschet == 1 & analysis_candidate_data$source == "Die Welt"], na.rm = T),
                   y = mean(analysis_candidate_data$negative_article[analysis_candidate_data$laschet == 1 & analysis_candidate_data$source == "Die Welt"], na.rm = T)), 
               colour = "black", shape = 2, size = 5) +
    geom_text(aes(label = "W", x = mean(analysis_candidate_data$negative_article[analysis_candidate_data$baerbock == 1 & analysis_candidate_data$source == "Die Welt"], na.rm = T),
                   y = mean(analysis_candidate_data$negative_article[analysis_candidate_data$baerbock == 1 & analysis_candidate_data$source == "Die Welt"], na.rm = T)), 
               colour = "green", shape = 2, size = 5) +
    
  
    geom_text(aes(label = "FR", x = mean(analysis_candidate_data$negative_article[analysis_candidate_data$scholz == 1 & analysis_candidate_data$source == "Frankfurter Rundschau"], na.rm = T),
                   y = mean(analysis_candidate_data$negative_article[analysis_candidate_data$scholz == 1 & analysis_candidate_data$source == "Frankfurter Rundschau"], na.rm = T)), 
               colour = "red", shape = 3, size = 5) +
    geom_text(aes(label = "FR", x = mean(analysis_candidate_data$negative_article[analysis_candidate_data$laschet == 1 & analysis_candidate_data$source == "Frankfurter Rundschau"], na.rm = T),
                   y = mean(analysis_candidate_data$negative_article[analysis_candidate_data$laschet == 1 & analysis_candidate_data$source == "Frankfurter Rundschau"], na.rm = T)), 
               colour = "black", shape = 3, size = 5) +
    geom_text(aes(label = "FR", x = mean(analysis_candidate_data$negative_article[analysis_candidate_data$baerbock == 1 & analysis_candidate_data$source == "Frankfurter Rundschau"], na.rm = T),
                   y = mean(analysis_candidate_data$negative_article[analysis_candidate_data$baerbock == 1 & analysis_candidate_data$source == "Frankfurter Rundschau"], na.rm = T)), 
               colour = "green", shape = 3, size = 5) +
    
    
    geom_text(aes(label = "SZ", x = mean(analysis_candidate_data$negative_article[analysis_candidate_data$scholz == 1 & analysis_candidate_data$source == "Süddeutsche Zeitung"], na.rm = T),
                   y = mean(analysis_candidate_data$negative_article[analysis_candidate_data$scholz == 1 & analysis_candidate_data$source == "Süddeutsche Zeitung"], na.rm = T)), 
               colour = "red", shape = 4, size = 5) +
    geom_text(aes(label = "SZ", x = mean(analysis_candidate_data$negative_article[analysis_candidate_data$laschet == 1 & analysis_candidate_data$source == "Süddeutsche Zeitung"], na.rm = T),
                   y = mean(analysis_candidate_data$negative_article[analysis_candidate_data$laschet == 1 & analysis_candidate_data$source == "Süddeutsche Zeitung"], na.rm = T)), 
               colour = "black", shape = 4, size = 5) +
    geom_text(aes(label = "SZ", x = mean(analysis_candidate_data$negative_article[analysis_candidate_data$baerbock == 1 & analysis_candidate_data$source == "Süddeutsche Zeitung"], na.rm = T),
                   y = mean(analysis_candidate_data$negative_article[analysis_candidate_data$baerbock == 1 & analysis_candidate_data$source == "Süddeutsche Zeitung"], na.rm = T)), 
               colour = "green", shape = 4, size = 5) +
    
  
    geom_text(aes(label = "taz", x = mean(analysis_candidate_data$negative_article[analysis_candidate_data$scholz == 1 & analysis_candidate_data$source == "taz - die tageszeitung"], na.rm = T),
                   y = mean(analysis_candidate_data$negative_article[analysis_candidate_data$scholz == 1 & analysis_candidate_data$source == "taz - die tageszeitung"], na.rm = T)), 
               colour = "red", shape = 5, size = 5) +
    geom_text(aes(label = "taz", x = mean(analysis_candidate_data$negative_article[analysis_candidate_data$laschet == 1 & analysis_candidate_data$source == "taz - die tageszeitung"], na.rm = T),
                   y = mean(analysis_candidate_data$negative_article[analysis_candidate_data$laschet == 1 & analysis_candidate_data$source == "taz - die tageszeitung"], na.rm = T)), 
               colour = "black", shape = 5, size = 5)+
    geom_text(aes(label = "taz", x = mean(analysis_candidate_data$negative_article[analysis_candidate_data$baerbock == 1 & analysis_candidate_data$source == "taz - die tageszeitung"], na.rm = T),
                   y = mean(analysis_candidate_data$negative_article[analysis_candidate_data$baerbock == 1 & analysis_candidate_data$source == "taz - die tageszeitung"], na.rm = T)), 
               colour = "green", shape = 5, size = 5) 
    
  + theme_minimal() 
  +  coord_cartesian(xlim = c(.35, .475), ylim = c(.35, .475))
  + labs(x = "mean candidate negativity", y = " mean source negativity")
)  
ggsave("plots/crosstab_plot_sentscore_text.png") 

 
(
  crosstab_plot_sentscore_header <- 
    ggplot() +
    #  source mean
#    geom_hline(aes(yintercept = mean(analysis_candidate_data$negative_article[analysis_candidate_data$RT == 1], na.rm = T)), colour = "orange", size = 2) +
    geom_hline(aes(yintercept = mean(analysis_data$negative_headlead[analysis_data$RT == 1], na.rm = T)), colour = "orange", size = 2) +
#    geom_hline(aes(yintercept = mean(analysis_candidate_data$negative_article[analysis_candidate_data$RT == 0], na.rm = T)), colour = "gray", size = 2) +
    geom_hline(aes(yintercept = mean(analysis_data$negative_headlead[analysis_data$RT == 0], na.rm = T)), colour = "gray", size = 2) +
    
    # candidate mean
#    geom_vline(aes(xintercept = mean(analysis_candidate_data$negative_article[analysis_candidate_data$baerbock == 1], na.rm = T)), colour = "green", size = 2) +
    geom_vline(aes(xintercept = mean(analysis_data$negative_headlead[analysis_data$baerbock == 1], na.rm = T)), colour = "green", size = 2) +
#    geom_vline(aes(xintercept = mean(analysis_candidate_data$negative_article[analysis_candidate_data$scholz == 1], na.rm = T)), colour = "red", size = 2) +
    geom_vline(aes(xintercept = mean(analysis_data$negative_headlead[analysis_data$scholz == 1], na.rm = T)), colour = "red", size = 2) +
#    geom_vline(aes(xintercept = mean(analysis_candidate_data$negative_article[analysis_candidate_data$laschet == 1], na.rm = T)), colour = "black", size = 2) +
    geom_vline(aes(xintercept = mean(analysis_data$negative_headlead[analysis_data$laschet == 1], na.rm = T)), colour = "black", size = 2) +
    
    geom_point(aes(x = mean(analysis_data$negative_headlead[analysis_data$baerbock == 1 & analysis_data$RT == 1], na.rm = T),
                   y = mean(analysis_data$negative_headlead[analysis_data$baerbock == 1 & analysis_data$RT == 1], na.rm = T)),
               colour = "green", size = 7) +
    geom_point(aes(x = mean(analysis_data$negative_headlead[analysis_data$scholz == 1 & analysis_data$RT == 1], na.rm = T),
                   y = mean(analysis_data$negative_headlead[analysis_data$scholz == 1 & analysis_data$RT == 1], na.rm = T)), 
               colour = "red", size = 7) +
    geom_point(aes(x = mean(analysis_data$negative_headlead[analysis_data$laschet == 1 & analysis_data$RT == 1], na.rm = T),
                   y = mean(analysis_data$negative_headlead[analysis_data$laschet == 1 & analysis_data$RT == 1], na.rm = T)), 
               colour = "black", size = 7) +
    
    geom_text(aes(label = "B", x = mean(analysis_data$negative_headlead[analysis_data$baerbock == 1 & analysis_data$source == "BILD"], na.rm = T),
                  y = mean(analysis_data$negative_headlead[analysis_data$baerbock == 1 & analysis_data$source == "BILD"], na.rm = T)), 
              colour = "green", shape = 1, size = 5) +
    geom_text(aes(label = "B", x = mean(analysis_data$negative_headlead[analysis_data$scholz == 1 & analysis_data$source == "BILD"], na.rm = T),
                  y = mean(analysis_data$negative_headlead[analysis_data$scholz == 1 & analysis_data$source == "BILD"], na.rm = T)), 
              colour = "red", size = 5) +
    geom_text(aes(label = "B", x = mean(analysis_data$negative_headlead[analysis_data$laschet == 1 & analysis_data$source == "BILD"], na.rm = T),
                  y = mean(analysis_data$negative_headlead[analysis_data$laschet == 1 & analysis_data$source == "BILD"], na.rm = T)), 
              colour = "black", shape = 1, size = 5) +
    
    geom_text(aes(label = "W", x = mean(analysis_data$negative_headlead[analysis_data$baerbock == 1 & analysis_data$source == "Die Welt"], na.rm = T),
                  y = mean(analysis_data$negative_headlead[analysis_data$baerbock == 1 & analysis_data$source == "Die Welt"], na.rm = T)), 
              colour = "green", shape = 2, size = 5) +
    geom_text(aes(label = "W", x = mean(analysis_data$negative_headlead[analysis_data$scholz == 1 & analysis_data$source == "Die Welt"], na.rm = T),
                  y = mean(analysis_data$negative_headlead[analysis_data$scholz == 1 & analysis_data$source == "Die Welt"], na.rm = T)), 
              colour = "red", shape = 2, size = 5) +
    geom_text(aes(label = "W", x = mean(analysis_data$negative_headlead[analysis_data$laschet == 1 & analysis_data$source == "Die Welt"], na.rm = T),
                  y = mean(analysis_data$negative_headlead[analysis_data$laschet == 1 & analysis_data$source == "Die Welt"], na.rm = T)), 
              colour = "black", shape = 2, size = 5) +
    
    
    geom_text(aes(label = "FR", x = mean(analysis_data$negative_headlead[analysis_data$baerbock == 1 & analysis_data$source == "Frankfurter Rundschau"], na.rm = T),
                  y = mean(analysis_data$negative_headlead[analysis_data$baerbock == 1 & analysis_data$source == "Frankfurter Rundschau"], na.rm = T)), 
              colour = "green", shape = 3, size = 5) +
    geom_text(aes(label = "FR", x = mean(analysis_data$negative_headlead[analysis_data$scholz == 1 & analysis_data$source == "Frankfurter Rundschau"], na.rm = T),
                  y = mean(analysis_data$negative_headlead[analysis_data$scholz == 1 & analysis_data$source == "Frankfurter Rundschau"], na.rm = T)), 
              colour = "red", shape = 3, size = 5) +
    geom_text(aes(label = "FR", x = mean(analysis_data$negative_headlead[analysis_data$laschet == 1 & analysis_data$source == "Frankfurter Rundschau"], na.rm = T),
                  y = mean(analysis_data$negative_headlead[analysis_data$laschet == 1 & analysis_data$source == "Frankfurter Rundschau"], na.rm = T)), 
              colour = "black", shape = 3, size = 5) +
    
    
    geom_text(aes(label = "SZ", x = mean(analysis_data$negative_headlead[analysis_data$baerbock == 1 & analysis_data$source == "Süddeutsche Zeitung"], na.rm = T),
                  y = mean(analysis_data$negative_headlead[analysis_data$baerbock == 1 & analysis_data$source == "Süddeutsche Zeitung"], na.rm = T)), 
              colour = "green", shape = 4, size = 5) +
    geom_text(aes(label = "SZ", x = mean(analysis_data$negative_headlead[analysis_data$scholz == 1 & analysis_data$source == "Süddeutsche Zeitung"], na.rm = T),
                  y = mean(analysis_data$negative_headlead[analysis_data$scholz == 1 & analysis_data$source == "Süddeutsche Zeitung"], na.rm = T)), 
              colour = "red", shape = 4, size = 5) +
    geom_text(aes(label = "SZ", x = mean(analysis_data$negative_headlead[analysis_data$laschet == 1 & analysis_data$source == "Süddeutsche Zeitung"], na.rm = T),
                  y = mean(analysis_data$negative_headlead[analysis_data$laschet == 1 & analysis_data$source == "Süddeutsche Zeitung"], na.rm = T)), 
              colour = "black", shape = 4, size = 5) +
    
    
    geom_text(aes(label = "taz", x = mean(analysis_data$negative_headlead[analysis_data$baerbock == 1 & analysis_data$source == "taz - die tageszeitung"], na.rm = T),
                  y = mean(analysis_data$negative_headlead[analysis_data$baerbock == 1 & analysis_data$source == "taz - die tageszeitung"], na.rm = T)), 
              colour = "green", shape = 5, size = 5) +
    geom_text(aes(label = "taz", x = mean(analysis_data$negative_headlead[analysis_data$scholz == 1 & analysis_data$source == "taz - die tageszeitung"], na.rm = T),
                  y = mean(analysis_data$negative_headlead[analysis_data$scholz == 1 & analysis_data$source == "taz - die tageszeitung"], na.rm = T)), 
              colour = "red", shape = 5, size = 5) +
    geom_text(aes(label = "taz", x = mean(analysis_data$negative_headlead[analysis_data$laschet == 1 & analysis_data$source == "taz - die tageszeitung"], na.rm = T),
                  y = mean(analysis_data$negative_headlead[analysis_data$laschet == 1 & analysis_data$source == "taz - die tageszeitung"], na.rm = T)), 
              colour = "black", shape = 5, size = 5)
  + theme_minimal()
  + coord_cartesian(xlim = c(.4, .6), ylim = c(.4, .6))
  + labs(x = "mean candidate negativity", y = " mean source negativity")
)  
ggsave("plots/crosstab_plot_sentscore_header.png")



  