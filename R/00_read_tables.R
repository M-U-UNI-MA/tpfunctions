legal_entities <-
  readr::read_delim(
    "00_tables/legal_entities_utf8_3.csv",
    ";",
    col_types = readr::cols(.default = "c"),
    na = character()
  )

devtools::use_data(legal_entities, overwrite = TRUE)

db_clean <- readr::read_delim("00_tables/datastream_clean_regex.csv",
                  ";",
                  col_types = readr::cols(.default = "c"))

devtools::use_data(db_clean, overwrite = TRUE)

stand_table <- readr::read_delim("00_tables/standardization_table.csv", ";")
devtools::use_data(stand_table, overwrite = TRUE)

word_replace <- readr::read_delim(
  "00_tables/word_replace.csv",
  ";",
  col_types = readr::cols(.default = "c"),
  na = character()
)

devtools::use_data(word_replace, overwrite = TRUE)
