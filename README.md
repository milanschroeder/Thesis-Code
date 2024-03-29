# Strategies of Kremlin-Originated Disinformation in the 2021 German Federal Election

### A Thesis Project by Milan Schröder

#### Master of International Affairs \| Class of 2022 \| Hertie School, Berlin

### Abstract:

Election interferences by authoritarian states, most prominently Russia, have been grown to a major concern after their alleged contribution to populist victories. However, little is known about the effect of such influence operations. To assess their effectiveness and potential countermeasures, it is critical to understand what the goal and strategy behind such campaigns is. Besides partisan intervention, elections also provide opportunity for democracy-eroding interference. For such an intervention, the present paper discusses two ways of targeting political support in the sense of Easton (1975), by promoting political alienation (Gamson, 1968), focussing on the perception of either the input, or output of the democratic political system. The empirical part of the paper, tests for these strategies in the runup to the 2021 German federal election. Quantitative text analysis was used to examine the information output by Russian state medium RT Deutsch over the course of the campaign. It finds that over-coverage compared to established media – including negative one – is specifically directed at candidates that lead the polls at any point of the election. This result supports the proposed outputalienation-hypothesis, though variation between candidates cautions the interpretation of the findings. The analysis also suggests that the implications of the hypothesis, negative focus on (likely future) government figures, also extends after the election. To establish this extension empirically and to make sense of differences between candidates is left to future research.

### Contents:

-   **00_RT_Scraper.R**
    -   updated scraping functions:

        -   more efficient

        -   allows scraping full pages of all RT versions
-   **99_scrape_RT_full.R**
    -   scraping for *Russian Media Archive* Project, using 00_RT_Scraper.R
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
-   ***02_data_extraction_Factiva.R*** - main output: - file *articles/corpus_factiva.RData*, containing R object *corpus_factiva*, *corpus_factiva_candidate*, *factiva_corpus*, and *factiva_corpus_candidate*
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
    -   performs sentiment analysis
-   ***04_additional_analysis.R***
    -   performs all types of descriptive statistic
-   ***05_main_analysis.R***
    -   performs main graphical analyses
-   ***06_regression.R***
    -   performs multiple interrupted time-series regressions in various specifications
