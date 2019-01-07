# doc <- "00_tables/test_text_file.txt"
# term.list <- readr::read_delim("00_tables/test_term_list.csv", ";")
# terms <- term.list$term
#
# get_freq <- function(doc, terms) {
#
#   `%>%` <- magrittr::`%>%`
#
#   # prepare terms
#   term.table <- tibble::tibble(term = terms) %>%
#     dplyr::mutate(ngram  = stringi::stri_count_fixed(term, " ") + 1) %>%
#     dplyr::mutate(search = stringi::stri_replace_all_fixed(term, " ", ""))
#   ngrams <- sort(unique(term.table$ngram), decreasing = TRUE)
#
#   # prepare text
#   text <- readtext::readtext(doc) %>%
#     tidytext::unnest_tokens(words, text)
#   text.uni <- text %>%
#     dplyr::distinct(words) %>%
#     dplyr::mutate(words_stand = tpfuns::stand_chars(words)) %>%
#     dplyr::mutate(words_stand = tpfuns::stand_ae_be(words_stand)) %>%
#     dplyr::mutate(words_stand = textstem::lemmatize_words(words_stand))
#   text <- text %>%
#     dplyr::left_join(text.uni, by = "words") %>%
#     dplyr::group_by(doc_id) %>%
#     dplyr::summarise(text = stringi::stri_flatten(words_stand, " "))
#
#   # frequency count
#   freq <- lapply(ngrams, function(x) {
#     text.token <- text %>%
#       tidytext::unnest_tokens(phrase, text, "ngrams", n = x) %>%
#       dplyr::mutate(row_id = dplyr::row_number()) %>%
#       dplyr::mutate(phrase_space = stringi::stri_replace_all_fixed(phrase, " ", ""))
#
#     match <- text.token %>%
#       dplyr::inner_join(term.table, by = c("phrase_space" = "search")) %>%
#       dplyr::select(doc_id, ngram, row_id, term)
#   }) %>% dplyr::bind_rows()
#
#   freq <- freq %>%
#     dplyr::arrange(doc_id) %>%
#     dplyr::mutate(group_id = dplyr::group_indices(., doc_id))
#
#   freq <- split(freq, freq$group_id)
#
#   freq.adj <- lapply(freq, function(y) {
#     freq.adj <- y
#     for (i in ngrams[!ngrams == 1]) {
#       ids <- freq.adj %>%
#         dplyr::filter(ngram == i) %>%
#         dplyr::pull(row_id)
#
#       ids <- unlist(lapply(ids, function(x) 0:(i - 1) + x))
#       freq.adj <- freq.adj %>%
#         dplyr::filter(!row_id %in% ids | ngram >= i)
#     }
#     return(freq.adj)
#   })
#
#   freq <- dplyr::bind_rows(freq) %>%
#     dplyr::group_by(doc_id, term) %>%
#     dplyr::summarise(freq = dplyr::n())
#
#   freq.adj <- dplyr::bind_rows(freq.adj) %>%
#     dplyr::group_by(doc_id, term) %>%
#     dplyr::summarise(freq.adj = dplyr::n())
#
#   freq <- freq %>%
#     dplyr::left_join(freq.adj, by = c("doc_id", "term")) %>%
#     tidyr::replace_na(list(freq.adj = 0))
#
#   return(freq)
#
# }
#
#
#
#
#
#
#
#
#
#
#
#
#
#
