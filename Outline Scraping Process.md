# Scraping Process

1.  Scrape base_Sitemaps
    1.  Read xml (using sitemap_versions)

    2.  updated_sitemaps \<- filter for Sitemaps not in archive (lastmod \> lastscrape)

    3.  amend archive base_sitemaps with updated_sitemaps
2.  Scrape updated_sitemaps 1. Read xml (using updated_sitemaps\$loc) 2. new_articles \<- filter for urls not in archive (lastmod \> lastscrape)
    1.  compare to archived Sitemaps or archived articles\$link?

        1.  redirects?

        2.  errors?

            3.  amend archive with new_articles
3.  Scrape new_articles
    1.  select version-specific scraping function

        1.  DE = ESP = RS

            1.  RS: twice for both versions?

        2.  EN \~ FR \~ RUS

        3.  Arabic: more difficult (DDos, specific Format)

    2.  Amend meta_archive (incl. Text) with every article scrape

        ::: {style="color: red"}
        1.  add hash from link linking meta_archive to media_archive

            1.  Either:

                1.  split up media_archive in multiple folders ordered by sub-hash

                2.  (split up by (version-year pair (cf. sitemap) or) version-month pair)

            2.  name media files {hash_no}
        :::

    3.  Either:

        1.  ~~(amend linklist etc. with id/hash and (horizontal) vector of links)~~

        2.  amend yt_list, twitter_list, linklist, ... with article_id - link pair

    4.  include reference_count for each link in yt/twitter/link_list

        1.  if internal link: include reference_count into meta_archive as internally_referenced

            1.  If not in meta_archive: amend meta_archive with note

                1.  try scrape in next run

                2.  if not exist: lookup on archive.org

        2.  else (if external link):

            1.  if available: save full page, including media (using archive.org servers?)

            2.  else: check if on archive.org (earliest version)

            3.  save link to archive.org
4.  check that all new_articles are in scrape & check for DDos-guard pages (unless previous error note)
    1.  log with every loop

        1.  run again for missing ones

            1.  if not exist: lookup at archive.org

            2.  add note

        2.  log again

    2.  if unusually high number of missings (or missings in specific fields): add note

        1.  potentially rescrape those
5.  create db query method/API
    1.  if including article list: split into multiple variables
    2.  (if including media: create zipped folder)
