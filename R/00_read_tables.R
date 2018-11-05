legal_entities <-
  readr::read_delim("00_tables/legal_entities_utf8_5.csv",";",
    col_types = readr::cols(.default = "c"), na = character())
usethis::use_data(legal_entities, overwrite = TRUE)


db_clean <- readr::read_delim("00_tables/datastream_clean_regex.csv",
                  ";", col_types = readr::cols(.default = "c"))
usethis::use_data(db_clean, overwrite = TRUE)


stand_table <- readr::read_delim("00_tables/standardization_table.csv", ";",
                                 col_types = readr::cols(.default = "c"))
usethis::use_data(stand_table, overwrite = TRUE)


word_replace <- readr::read_delim("00_tables/word_replace.csv", ";",
  col_types = readr::cols(.default = "c"), na = character())
usethis::use_data(word_replace, overwrite = TRUE)


table_stand_ae_be <- readr::read_delim("00_tables/ae_be.csv", ";",
  col_types = readr::cols(.default = "c"), na = character())
usethis::use_data(table_stand_ae_be, overwrite = TRUE)

datatream_countries <- readr::read_delim("00_tables/datastream_countries.csv", ";",
                                       col_types = readr::cols(.default = "c"), na = character())
usethis::use_data(datatream_countries, overwrite = TRUE)
