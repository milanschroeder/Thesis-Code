# Strategies of Kremlin-Originated Disinformation in the 2021 German Federal Election 
### A Thesis Project by Milan Schröder
#### Master of International Affairs, Class of 2022, Hertie School, Berlin


### Contents:

- *01_scraping_Factiva.R*
  - loads and converts newspaper articles from the Factiva Database
    - outputs:
      - folder articles/search_results_html, containing metadata of all articles published by 5 major german newspapers in 2021 (.txt)
      - folder articles/search_results_html_missed, containing metadata that was missed above (.txt)
      - folder articles/Factivahtml, containing all articles by the 5 newspapers mentioning at least one of the lead candidates (.txt)
    
- *01_scraping_RT.R*  
  - scrapes metadata of all articles by published by RT Deutsch in 2021
    - outputs: 
      - folder articles/RT_searches, containing metadata of all articles by RT in 2021 (one- and two-day intervals) (.txt)
  - scraping metadata of all articles mentioning at least one of the candidates to identify relevant articles
    - outputs: 
      - folder articles/RT_searches_candidates, containing results for searches by candidate name on RT (25 day intervals) (.txt)
      - folder articles/RT_searches_candidates_old, identical search parameters, but slightly different results (.txt)
  - scrapes full texts of all articles mentioning at least one of the lead candidates of the 2021 German federal election
    - outputs: 
      - folder articles/RThtml, containing full texts of all articles by RT in 2021 that mentioned at least one of the candidates (.txt)

- *01_get_polls.R*
  - plans to load/scrape and pre-process time-series of aggregated polling data in the run-up to the 2021 German federal election from zweitstimme.org
    - outputs:
      - EMPTY folder polls (data still missing)
  
- *01_get_factchecks.R*
  - scrapes factchecked disinformation narratives related to candidates from dpa factcheck and correctiv
    - outputs:
      - folder articles/disinformation, containing overview of factchecked narratives (.html and .txt files) 
  - considers scraping factchecked disinformation narratives related to candidates from tagesschau-faktenfinder
  
- *02_data_extraction_RT.R*
  - converts scraped data from RT into tidy data
    - outputs:
      - 
  
### ToDo list:

- structure work
- convert dates
- get polling data
- fix data issues
- merge RT and Factiva corpora
- aggregate
- analyse
- finish paper

