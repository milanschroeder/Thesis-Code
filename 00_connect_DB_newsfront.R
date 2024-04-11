# 00_connect_DB_newsfront.R

##### returns DB connection conn & connect function connect_mariadb

# connect DB ####

library(pacman)

connect_mariadb <- function(){
  p_load(RMariaDB)
  p_load(rstudioapi)
  
  conn <- DBI::dbConnect(drv = RMariaDB::MariaDB(),
                         dbname = "russian_media_database_newsfront",
                         username = "scraper",
                         password = rstudioapi::askForPassword("Database password"), port = 3336 # as long as not run from server directly
  )
  return(conn)
}

conn <- connect_mariadb()
