#' Staistics for text documents
#'
#' @description
#' This function counts the number of pages (n_page), the number of words (n_words) and
#' the number of characters (n_char). Based on this the ration words per page (n_word_page)
#' and characters per words (n_char_word) is calculated
#'
#' @param path
#' The path to the textfiles (get with list.files())
#'
#' @return
#' A Dataframe
#' @export
docs_stats <- function (path) {

  `%>%` <- magrittr::`%>%`

  docs <- readtext::readtext(path_docs) %>%
    dplyr::mutate(n_page = stringi::stri_count_regex(text, "\\f")) %>%
    tidytext::unnest_tokens(words, text) %>%
    dplyr::mutate(n_word = 1) %>%
    dplyr::mutate(n_char = nchar(words)) %>%
    dplyr::group_by(doc_id) %>%
    dplyr::summarise(n_page = dplyr::first(n_page),
                     n_word = sum(n_word),
                     n_char = sum(n_char)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(n_word_page = round(n_word / n_page, 4)) %>%
    dplyr::mutate(n_char_word = round(n_char / n_word, 4)) %>%
    dplyr::mutate(doc_id = stringi::stri_replace_all_regex(doc_id, "\\.txt$", ""))

    return(docs)

}
