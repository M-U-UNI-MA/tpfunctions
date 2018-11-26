`%>%` <- magrittr::`%>%`
table_legal_entities <- readr::read_delim("data-raw/table_legal_entities.csv", ";") %>%
  dplyr::mutate(le_name = tpfuns::top_stand_punct(le_name)) %>%
  dplyr::mutate(le_name = tpfuns::top_rem_punct(le_name, punct.replacement = TRUE)) %>%
  dplyr::mutate(regex = tpfuns::escape_regex(le_name)) %>%
  dplyr::mutate(regex = dplyr::if_else(type == "abbr", stringi::stri_replace_all_regex(regex, "(\\w)", "$1\\\\s?"), regex)) %>%
  dplyr::mutate(regex = dplyr::case_when(occurance == "Start" ~ paste0("^", regex, "\\b"),
                                         occurance == "End" ~ paste0("\\b", regex, "$"))) %>%
  dplyr::mutate(regex = stringi::stri_replace_all_fixed(regex, " ", "\\s?")) %>%
  dplyr::mutate(ngram = stringi::stri_count_fixed(le_name, " ") + 1) %>%
  dplyr::mutate(ngram = dplyr::if_else(type == "abbr", ngram, 0)) %>%
  dplyr::group_by(le_stand) %>%
  dplyr::summarise(regex = stringi::stri_flatten(regex, "|"),
                   ngram = max(ngram)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(dplyr::desc(ngram))

usethis::use_data(table_legal_entities, overwrite = TRUE)
