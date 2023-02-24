# Scraping Process

1.  Scrape base_Sitemaps
    1.  Read xml (using sitemap_versions)

    2.  updated_sitemaps \<- filter for Sitemaps not in archive (lastmod \> lastscrape)

    3.  amend archive base_sitemaps with updated_sitemaps
2.   Scrape updated_sitemaps
    1.  Read xml (using updated_sitemaps\$loc)
    2.  new_articles \<- filter for urls not in archive (lastmod \> lastscrape)
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

        1.  ::: {style="color: red"}
            1.  add hash to meta archive

                1.  Either:

                    1.  split up details in multiple folders ordered by hash

                    2.  split up by version-year pair (cf. sitemap) or version-month pair
            :::

        2.  Either:

            1.  (amend linklist etc. with id/hash and (horizontal) vector of links)

            2.  amend yt_list, twitter_list, linklist, ... with article_id - link pair

    3.  check that all new_articles are in scrape & check for DDos-guard pages (unless previous error note)

        1.  log with every loop

            1.  run again for missing ones

                1.  if not exist: lookup at archive.org

                2.  add note

            2.  log again

    4.   collect recommendations

        1.  internal links: update link count

            1.  If not in archive: amend archive with note

                1.  scrape in next run

                2.  if not exist: lookup on archive.org

        2.  external links:

            1.  if available: save full page, including media (on archive.org)

                1.  else: check if on archive.org (earliest version)

            2.  save link to archive.org

                1.  what if recurrent?
4.  create db query method
    1.  if including article list: split into multiple variables
