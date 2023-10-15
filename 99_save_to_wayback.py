# Save rt to archive.org (unless already online)

import savepagenow as spn
import pandas as pd
from getpass import getpass
import mariadb

db_user = "scraper"
# localport = 3336


con = mariadb.connect(
    user = db_user,
    password = getpass(),
#    host = "127.0.0.1",
#    port = localport,
    database = "russian_media_database_rt"
)

url_list = pd.read_sql_query("SELECT link, doc_hash, available_online FROM page_data;", con)



for i in range(len(url_list)):
    spn.capture_or_cache(url_list)
