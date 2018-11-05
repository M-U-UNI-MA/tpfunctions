#' Term List Preparation
#'
#' @description
#' This function takes a vector of terms and performs standardization and deduplication
#' measures on it. The output (a dataframe) contains a key (term_stand_space) which
#' is a deduplicated version of the terms provided and accounts for several issues
#'
#' @param terms
#' A string with terms to be prepsared
#'
#' @return
#' A dataframe with standardized terms
#' @export
#'
#' @examples
#' prepare_term_list(c("Derecognize", "Dereconized", "Capitalize", "CAPITALIZED"))
prepare_term_list <- function(terms) {
  `%>%` <- magrittr::`%>%`

  term.table <- tibble::tibble(term = terms) %>%
    dplyr::distinct(term) %>%
    dplyr::mutate(term = stringi::stri_trans_tolower(stringi::stri_trim_both(term))) %>%
    dplyr::mutate(term_stand = tpfuns::term_operation_fast(term)) %>%
    dplyr::mutate(term_stand_space = stringi::stri_replace_all_fixed(term_stand, " ", "")) %>%
    dplyr::mutate(ngram = stringi::stri_count_fixed(term_stand, " ") + 1) %>%
    dplyr::mutate(nchar = nchar(term_stand)) %>%
    dplyr::arrange(term_stand, term, ngram, nchar) %>%
    dplyr::group_by(term_stand) %>%
    dplyr::summarize(term_stand_space = dplyr::first(term_stand_space),
                     term  = stringi::stri_flatten(term, " <> "),
                     ngram = dplyr::first(ngram),
                     nchar = dplyr::first(nchar)) %>%
    dplyr::arrange(term_stand_space, term_stand, term, ngram, nchar) %>%
    dplyr::group_by(term_stand_space) %>%
    dplyr::summarize(term_stand = stringi::stri_flatten(term_stand, " <> "),
                     term  = stringi::stri_flatten(term, " <> "),
                     ngram = dplyr::first(ngram),
                     nchar = dplyr::first(nchar)) %>%
    dplyr::mutate_all(stringi::stri_trim_both) %>%
    dplyr::select(ngram, nchar, dplyr::everything()) %>%
    dplyr::arrange(ngram, term_stand_space, nchar)

  return(term.table)

}
