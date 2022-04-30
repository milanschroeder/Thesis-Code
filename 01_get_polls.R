# 01 get polling data

library(pacman)
p_load(dataverse, rio, jsonlite, tidyverse, magrittr)

dataverse_search("https://dataverse.harvard.edu/api/access/datafile/4816036", server = "dataverse.harvard.edu")
dataverse::get_dataframe_by_id(fileid = "4816036", server = "dataverse.harvard.edu")

zweitstimme <- rio::import("polls/zweitstimme_output_100.RDS")
zweitstimme_polls_aggregated <- zweitstimme$poll_aggregator


# actual polling numbers:
# problem: no data close to election

zweitstimme_polls <- zweitstimme$polls

zweitstimme_polls_long <- 
  pivot_longer(zweitstimme_polls, cols = c("cdu", "spd", "gru", "lin", "fdp", "afd", "oth"), names_to = "party", values_to = "share")


# (Bayesian) aggregated polling numbers (continuous) ####
# problem: not accurate close to election...

parties <- c("cdu", "spd", "gru", "lin", "fdp", "afd", "oth")

polls_est <- 
  sapply(zweitstimme$poll_aggregator, function(x)
    x[1, parties]) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column("date") %>% 
  mutate(date = as.Date(date))

polls_est_long <- 
  pivot_longer(polls_est, cols = c("cdu", "spd", "gru", "lin", "fdp", "afd", "oth"), names_to = "party", values_to = "share") %>% 
  mutate(date = as.Date(date),
         share = as.double(share))

# get polls from web source ####
polls <- jsonlite::fromJSON("https://gfx.sueddeutsche.de/storytelling-assets/datenteam/2021_btw-polls/polls.json") %>% 
  mutate(date = as.Date(date))

polls %<>% 
  group_by(date) %>% 
  mutate(rank = order(order(mean, decreasing = T)),
         firstrank = ifelse(rank == 1,
                            1, 
                            0)) %>% 
  ungroup()

# check for overlap of ci
polls_wide <- polls %>% 
  #  filter(party %in% c("gru", "cdu", "spd")) %>% 
  select(-c(sample_size, institute)) %>% 
  pivot_wider(.,  values_from = c(mean, lower, upper, percent, rank, firstrank), 
              names_from = party, 
              values_fn = list("mean" = mean, "lower" = min, "upper" = max, "percent" = mean, "rank" = min, "firstrank" = min))

polls_wide %<>% rowwise() %>%  mutate(
  # overlap of ci:
  overlap_cdu = case_when(
    between(lower_cdu, lower_spd, upper_spd) | 
      between(lower_cdu, lower_gru, upper_gru) |
      between(upper_cdu, lower_spd, upper_spd) | 
      between(upper_cdu, lower_gru, upper_gru) ~ 1,
    TRUE ~ 0),
  
  overlap_spd = case_when(
    between(lower_spd, lower_cdu, upper_cdu) | 
      between(lower_spd, lower_gru, upper_gru) |
      between(upper_spd, lower_cdu, upper_cdu) | 
      between(upper_spd, lower_gru, upper_gru) ~ 1,
    TRUE ~ 0),
  
  overlap_gru = case_when(
    between(lower_gru, lower_spd, upper_spd) | 
      between(lower_gru, lower_cdu, upper_cdu) |
      between(upper_gru, lower_spd, upper_spd) | 
      between(upper_gru, lower_cdu, upper_cdu) ~ 1,
    TRUE ~ 0),
  
  # overlap with individual polls that lie outside ci:
  anyoverlap_cdu = case_when(
    between(percent_cdu, lower_spd, upper_spd) |
      between(percent_cdu, lower_gru, upper_gru) ~ 1, 
    TRUE ~ 0),
  
  anyoverlap_spd = case_when(
    between(percent_spd, lower_cdu, upper_cdu) |
      between(percent_spd, lower_gru, upper_gru) ~ 1, 
    TRUE ~ 0),
  
  anyoverlap_gru = case_when(
    between(percent_gru, lower_spd, upper_spd) |
      between(percent_gru, lower_cdu, upper_cdu) ~ 1, 
    TRUE ~ 0)
)

polls_long <- polls_wide %>% 
  pivot_longer(cols = -date, 
               names_to = c("variable", "party"), 
               names_pattern = "(.*)_(.*)") %>% 
  pivot_wider(.,  values_from = value, 
              names_from = variable) 

# save all:
save(list = c("polls_wide", "polls_long", 
              "polls_est", "polls_est_long",
              "zweitstimme_polls", "zweitstimme_polls_long"), 
     file = "polls/all_polls.RData")    

