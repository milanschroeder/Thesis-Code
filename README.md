# Strategies of Kremlin-Originated Disinformation in the 2021 German Federal Election

### A Thesis Project by Milan Schröder

#### Master of International Affairs \| Class of 2022 \| Hertie School, Berlin

### Abstract:

...

### Contents:

-   ***01_scraping_Factiva.R***
    -   loads and converts newspaper articles from the Factiva Database
        -   outputs:
            -   folder *articles/search_results_html*, containing metadata of all articles published by 5 major german newspapers in 2021 (.txt)
            -   (**deprecated** - file *articles/old/overview_searches.xlsx* to identify if search results were missed)
            -   folder *articles/search_results_html_missed*, containing metadata that was missed above (.txt)
            -   folder *articles/Factivahtml*, containing all articles by the 5 newspapers mentioning at least one of the lead candidates (.txt)
            -   file *articles/factiva_data.xlsx*, a dataset of all 211201 articles published by the 5 newpapers in 2021
-   ***01_get_polls.R*** - main output: - file *polls/all_polls.RData*, containing ...
    -   load time-series of aggregated polling data in the run-up to the 2021 German federal election from zweitstimme.org
        -   outputs:
            -   file *polling_data/zweitstimme_output_100.RDS* from dataverse.harvard.edu, containing replication data by zweitstimme.org
            -   file *polling_data/zweitstimme_replication_PS.pdf*, containing replication instructions
    -   scrapes actual polling numbers from dashboard on zweitstimme.org
        -   tbls *polls\_(long/wide)*, containing all pre-election 2021 (averaged) polls
-   ***01_get_factchecks.R***
    -   scrapes factchecked disinformation narratives related to candidates from dpa factcheck and correctiv
        -   outputs:
            -   folder *articles/disinformation*, containing overview of factchecked narratives (.html and .txt files)
    -   considers scraping factchecked disinformation narratives related to candidates from tagesschau-faktenfinder
-   ***01_scraping_RT.R***
    -   scrapes metadata of all articles mentioning at least one of the candidates to identify most relevant articles
        -   outputs:
            -   folder *articles/RT_searches_candidates*, containing results for searches by candidate name on RT (25 day intervals) (.txt)
            -   folder *articles/RT_searches_candidates_old*, identical search parameters, but slightly different results (.txt)
    -   scrapes metadata of all articles by published by RT Deutsch in 2021
        -   outputs:
            -   folder *articles/RT_searches*, containing metadata of all articles by RT in 2021 (one- and two-day intervals) (.txt)
            -   folder *articles/RT_all_searches*, combining all search results in one place (.txt)
    -   scrapes full texts of all articles published by RT in 2021
        -   outputs:
            -   folder *articles/RThtml*, containing full texts of all 9701 articles (- 2 were links were broken) by RT in 2021 (.txt) (*RTarticle[1:931].txt* are articles that mentioned at least one of the candidates)
    -   scrapes full texts from saved articles
        -   outputs:
            -   (**deprecated by RT_corpus** - file *articles/old/data_rt.rds*, a dataset of metadata, full texts, and html code of all article pages)

-   ***02_data_extraction_Factiva.R***
        - main output:
            - file *articles/corpus_factiva.RData*, containing R object *corpus_factiva*, *corpus_factiva_candidate*, *factiva_corpus*, and *factiva_corpus_candidate*
    -   gets tidy data (metadata and fulltext for candidate related articles) from Factivca
        -   outputs:
            -   file *articles/corpus_factiva.xlsx*, a dataset of metadata for all 190577 articles from 5 German newspapers published in 2021, and fulltexts for all 7892 articles mentioning at least one of the lead candidates
-   ***02_data_extraction_RT.R***
    -   main output:
        -   file *articles/RTcorpus.RData*, containing the R objects *corpus_RT*, *corpus_RT_candidate*, *RT_corpus*, *RT_corpus_candidate*, *summariesRT*, and *summariesRT_long*
    -   converts scraped search results data from RT into tidy data
        -   outputs:
            -   (**deprecated by RT_corpus** - file *articles/old/RT_articles.xlsx*, a dataset with metadata of 8297 articles published by RT in 2021)
    -   converts scraped article pages to tidy data
        -   outputs:
            -   (**deprecated by RT_corpus** - file *articles/old/RT_fulltext_all.xlsx*, a dataset of 5701 articles including full text and metadata (including in-text links))
    -   combine both
        -   outputs:
            -   file *articles/RT_corpus.xlsx*, a dataset combining metadata and fulltexts of all articles
    -   scrape search summaries:
        -   outputs:
            -   file *articles/summaries_RT_searches.xlsx*, a dataset of search result counts for all articles by search term and day (wide format)
            -   file *articles/summaries_RT_searches_long.xlsx*, a dataset of search result counts and share for all articles by candidate and day (long format)
-   ***03_plots.R*** - main output: - file *plots/all_plots.RData*, containing R objects of all plot and layers below
    -   visually checks fit of zweitstimme predictions
        -   outputs:
            -   plot *gg_zweitstimme_performance*, pointing to benefits of using actual polls instead of estimates
    -   creates background graphics highlighting leading party over time
        -   outputs:
            -   plot *gg_leading_party_background*, highlighting leading party until clearly passed
            -   plot *gg_leading_party_background_gaps*, leaving blank where top-2 don't differ significantly
            -   plot *gg_leading_parties_trend*, trends for top-3 parties
            -   plot *gg_all_parties_poll*, polls and trends for all parties
            -   list *layers*, containing ggplot layers:
                -   ...
-   ***04_sentiment_analysis.R***
        -   main output:
            - 
    -   performs sentiment analysis
-   ***04_additional_analysis.R***
    -   performs all types of descriptive statistic
-   ***FIX/DELETE**: broken/superseded files*
    -   articles/old/factiva_data_candidate_articles.xlsx (broken date, no header)
    -   articles/old/factiva_data_candidates_probably_wrong.xlsx (broken header)
    -   ?/RT_articles_all.xlsx (only link/lead/header of 2400 articles)
    -   articles/old/links_rt.csv (links to 5000 articles)
    -   articles/old/RT_articles_more.xlsx (header/link/lead of 45 articles that were previously missed)

### ToDo list:

-   save disinfo and Factiva as .RData!
-   concatenate "factiva\_", source_abb, month, and (result_no -\> number) to saved (despite irregularities with missed ones? or just source-month-results_no)
-   add plot layer with
    -   disinformation highlights
    -   individual article/mentions counts per newspaper
    -   aggregated article/mentions count
    -   sentiment scores
    -   ...?
-   aggregate (by day/week, what else?)
-   get sentiment analyses (avg, binary for each doc, plot by c/d case)
    -   compare results across dictionaries
-   analyse time-series
-   finish paper (...there is also theory...)
-   clean unnecessary stuff from 02_data_extraction_Factiva.R
-   build replication scraper that uses saved .txt files
-   improve scraping functions to make them publicly available
-   clean folder
