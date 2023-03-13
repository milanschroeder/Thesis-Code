# main:

source("00_sitemap_updater.R")

# run base_sitemap scraper: ####
updated_sitemaps <- scrape_base_sitemaps()

# run page_list scraper: ####
# usually: 
scrape_sitemaps()

# initial scrape: ####
# scrape_sitemaps(sitemaps_source = "all", sleeptime = 3)