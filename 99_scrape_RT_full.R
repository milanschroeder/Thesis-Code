# Scrape full RT DE Archive:

source(file = "00_RT_Scraper.R")

# test with first ever articles: 
testlinks <- scrapeRT(period_end = "07-09-2014", folder = "test")
test_df <- scrapeRT(period_end = "07-09-2014", folder = "test", full_scrape = T)

testlinks <- c(testlinks, "https://de.rt.com/programme/451-programme/113305-451-grad-gibt-wahlhilfe-keine/")

test_articles_df <- scrape_articles(testlinks, folder = "test")
