int_read <- function(file) {
  readr::read_delim(file,
                    ";",
                    col_types = readr::cols(.default = "c"),
                    na = character())
}

table_americanize <-int_read("data-raw/table_americanize.csv")
usethis::use_data(table_americanize, overwrite = TRUE)

table_repl_words <- int_read("data-raw/table_repl_words.csv")
usethis::use_data(table_repl_words, overwrite = TRUE)

table_lemmas <- int_read("data-raw/table_lemmas.csv")
usethis::use_data(table_lemmas, overwrite = TRUE)

table_lemmas_selected <- int_read("data-raw/table_lemmas_selected.csv")
usethis::use_data(table_lemmas_selected, overwrite = TRUE)

table_errors <- int_read("data-raw/table_errors.csv")
usethis::use_data(table_errors, overwrite = TRUE)

table_split <- int_read("data-raw/table_split.csv")
usethis::use_data(table_split, overwrite = TRUE)

table_sent_abbr <- int_read("data-raw/table_sent_tok_abbr.csv")
usethis::use_data(table_sent_abbr, overwrite = TRUE)



# table_db_names <- readr::read_rds("data-raw/00_match_names.rds")
# usethis::use_data(table_db_names, overwrite = TRUE)

# datatream_countries <- readr::read_delim("00_tables/datastream_countries.csv", ";",
#                                        col_types = readr::cols(.default = "c"), na = character())
# usethis::use_data(datatream_countries, overwrite = TRUE)
#
#   readr::read_delim(
#     "00_tables/legal_entities_utf8_3.csv",
#     ";",
#     col_types = readr::cols(.default = "c"),
#     na = character()
#   )
#
#   usethis::use_data(legal_entities, overwrite = TRUE)
#
# db_clean <- readr::read_delim("00_tables/datastream_clean_regex.csv",
#                   ";",
#                   col_types = readr::cols(.default = "c"))
#
# usethis::use_data(db_clean, overwrite = TRUE)
#
# stand_table <- readr::read_delim("00_tables/standardization_table.csv", ";")
# devtools::use_data(stand_table, overwrite = TRUE)
#
# word_replace <- readr::read_delim(
#   "00_tables/word_replace.csv",
#   ";",
#   col_types = readr::cols(.default = "c"),
#   na = character()
# )
# usethis::use_data(word_replace, overwrite = TRUE)


# legal_entities <- int_read("00_tables/legal_entities_utf8_5.csv")
# usethis::use_data(legal_entities, overwrite = TRUE)
#
# db_clean <- int_read("00_tables/datastream_clean_regex.csv")
# usethis::use_data(db_clean, overwrite = TRUE)
#
#
# stand_table <- int_read("00_tables/standardization_table.csv")
# usethis::use_data(stand_table, overwrite = TRUE)
#
# word_replace <- int_read("00_tables/word_replace.csv")
# usethis::use_data(word_replace, overwrite = TRUE)
