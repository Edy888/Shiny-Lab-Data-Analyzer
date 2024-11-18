library(DBI)
library(dplyr)


# Установление соединения с базой данных
connect_to_db <- function() {
  dbConnect(RSQLite::SQLite(), "my_database.sqlite") # Замените SQLite на нужную БД
}