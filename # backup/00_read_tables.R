library(tidyverse); library(stringi)
int_read <- function(file) {
  readr::read_delim(file, ";",
                    col_types = readr::cols(.default = "c"),
                    na = character())
}
int_punct_complete  <- function(string) {
  tpfuns::top_punct(
    string      = string,
    lower       = TRUE,
    punct.stand = TRUE,
    punct.rem   = "complete",
    punct.sub   = TRUE
  )
}

# table: stopwords =======================================================================
fil.stop <- list.files("data-raw/", "^stop", FALSE, TRUE)
table_stop <- lapply(X = fil.stop, FUN = int_read) %>% dplyr::bind_rows() %>%
  rename(stop = stopword) %>% mutate(stop = int_punct_complete(stop)) %>%
  distinct(stop_type, stop, .keep_all = TRUE) %>%
  select(stop, stop_type, origin)
usethis::use_data(table_stop, overwrite = TRUE)

# table: americanize =====================================================================
table_americanize <- int_read("data-raw/table_americanize.csv") %>%
  mutate(uk = int_punct_complete(uk), us = int_punct_complete(us)) %>%
  select(uk, us, origin) %>%
  distinct(us, uk, .keep_all = TRUE)
usethis::use_data(table_americanize, overwrite = TRUE)

# table: errors ==========================================================================
table_errors <- int_read("data-raw/table_errors.csv") %>%
  mutate(error = int_punct_complete(error),
         correction = int_punct_complete(correction)) %>%
  select(error, correction) %>%
  mutate(origin = "own") %>% distinct(error, correction, .keep_all = TRUE)
usethis::use_data(table_errors, overwrite = TRUE)

# table: lemmas ==========================================================================
table_lemmas <- int_read("data-raw/table_lemmas.csv") %>%
  filter(!term == lemma) %>%
  filter(!stri_detect(lemma, fixed = " ")) %>%
  filter(!stri_detect(term, fixed = " "))
usethis::use_data(table_lemmas, overwrite = TRUE)

# table: split ===========================================================================
table_split <- int_read("data-raw/table_split.csv") %>%
  mutate(orig = int_punct_complete(orig), split = int_punct_complete(split)) %>%
  distinct(orig, split, .keep_all = TRUE) %>%
  unnest(word = stri_split(split, fixed = " ")) %>%
  group_by(orig) %>% mutate(id = paste0("word", row_number())) %>% ungroup()

word1 <- filter(table_split, id == "word1") %>% select(-id)
words <- filter(table_split, id != "word1") %>%
  left_join(table_lemmas %>% select(lemma, term), by = c("word" = "term")) %>%
  left_join(table_lemmas %>% select(lemma, term), by = "lemma") %>%
  mutate(term = if_else(is.na(term), word, term)) %>%
  select(-id, -lemma, -word)

table_split <- left_join(word1, words, by = c("orig", "split")) %>%
  mutate(orig = paste0(word, term), split = paste(word, term)) %>%
  distinct(orig, split, .keep_all = TRUE) %>%
  select(-word, -term)
rm(word1, words)
usethis::use_data(table_split, overwrite = TRUE)

# table: word_stand ======================================================================
table_word_stand <- int_read("data-raw/table_word_stand.csv") %>%
  mutate(word = int_punct_complete(word), replace = int_punct_complete(replace)) %>%
  distinct(word, replace, .keep_all = TRUE)
usethis::use_data(table_word_stand, overwrite = TRUE)

# table: sent_abbreviations ==============================================================
table_sent_abbr <- int_read("data-raw/table_sent_tok_abbr.csv")
usethis::use_data(table_sent_abbr, overwrite = TRUE)
