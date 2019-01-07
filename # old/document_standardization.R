#' Document standardization
#'
#' @description
#' This function standardizes documents to be suitable for frequqncy counts
#'
#' @param string
#' A string
#' @param stand.table
#' A table with standardizations (needs to have a column for the chracters that needs to
#' be standardized (in hex values) and a column for the replacement) \cr
#' See the attached table as example. If no table is specified the default table will be used
#' @param replace.table
#' A table with British English and American English spelling
#' @return
#' A string
#' @export
stand_docs_fast <- function (docs, stand.table = NULL, replace.table = NULL) {

  `%>%` <- magrittr::`%>%`
  if (is.null(stand.table)) stand.table <- tpfuns::stand_table
  if (is.null(replace.table)) {
    replace.table <- tpfuns::table_stand_ae_be %>%
      dplyr::distinct(., .keep_all = TRUE)
  }

  stand.table       <- dplyr::filter(stand.table, !replace == "<none>")
  stand.table.space <- stand.table %>% dplyr::filter(replace == "<space>")
  stand.table.del   <- stand.table %>% dplyr::filter(replace == "<del>")
  stand.table.other <- stand.table %>%
    dplyr::filter(replace != "<del>" & replace != "<space>") %>%
    dplyr::mutate(replace = stringi::stri_replace_all_fixed(replace, "<space>", " "))

  regex.to.space <- stringi::stri_flatten(stand.table.space$hex, "|")
  regex.to.del   <- stringi::stri_flatten(stand.table.del$hex, "|")
  regex.other    <- stand.table.other$hex
  replace.other  <- stand.table.other$replace

  string.table <- tibble::tibble(id = 1:length(docs), text = docs)

  string.table.words <- string.table %>%
    tidytext::unnest_tokens(words, text)

  string.table.uni <- string.table.words %>%
    dplyr::distinct(words) %>%
    dplyr::mutate(words_stand = words) %>%
    dplyr::mutate(words_stand = stringi::stri_trans_tolower(words_stand)) %>%
    dplyr::mutate(words_stand = stringi::stri_replace_all_regex(words_stand, regex.to.space, " ")) %>%
    dplyr::mutate(words_stand = stringi::stri_replace_all_regex(words_stand, regex.to.del, "")) %>%
    dplyr::mutate(words_stand = textclean::replace_non_ascii(words_stand)) %>%
    dplyr::left_join(replace.table, by = c("words_stand" = "UK")) %>%
    dplyr::mutate(US = dplyr::if_else(is.na(US), words_stand, US)) %>%
    dplyr::mutate(words_stand = textstem::lemmatize_words(US)) %>%
    dplyr::select(words, words_stand)

  string.table <- string.table.words %>%
    dplyr::left_join(string.table.uni, by = "words") %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(text = stringi::stri_flatten(words_stand, " ")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(text = stringi::stri_replace_all_regex(text, regex.other, replace.other, vectorize_all = F)) %>%
    dplyr::mutate(text = stringi::stri_trim_both(text)) %>%
    dplyr::mutate(text = stringi::stri_replace_all_regex(text, "\\s+", " "))

  string <- string.table$text
  return(string)

}
