#' Standardization of Strings
#'
#' @param string A string or a vector of strings to be standardized
#' @param stand.table A table with standardizations (needs to have a column for the
#' chracters that needs to be standardized (in hex values) and a column for the replacement)
#' See the attached table as example. If no table is specified the default table will be used
#'
#' @return A string
#' @export
#' @examples
#' # Replacement of non-ASCII characters to its nearest ASCII equivalent
#' stand_chars_fast("  ĆȦpītaĻĮSȆĎ")
#'
#' # Standardization of some characters
#' stand_chars_fast(c("A + B", "A AND B", "A & B", "( A and B  )"))
stand_chars_fast <- function(string, stand.table = NULL) {
  `%>%` <- magrittr::`%>%`
  if (is.null(stand.table)) stand.table <- tpfuns::stand_table

  stand.table       <- dplyr::filter(stand.table, !replace == "<none>")
  stand.table.space <- stand.table %>% dplyr::filter(replace == "<space>")
  stand.table.del   <- stand.table %>% dplyr::filter(replace == "<del>")
  stand.table.other <- stand.table %>%
    dplyr::filter(replace != "<del>" & replace != "<space>") %>%
    dplyr::mutate(replace = stringi::stri_replace_all_fixed(replace, "<space>", " "))

  regex.to.space <- stringi::stri_flatten(stand.table.space$hex, "|")
  regex.to.del <- stringi::stri_flatten(stand.table.del$hex, "|")
  regex.other   <- stand.table.other$hex
  replace.other <- stand.table.other$replace

  string.table <- tibble::tibble(id = 1:length(string), text = string) %>%
    dplyr::mutate(text = stringi::stri_trans_tolower(text)) %>%
    dplyr::mutate(text = stringi::stri_replace_all_regex(text, regex.to.space, " ")) %>%
    dplyr::mutate(text = stringi::stri_replace_all_regex(text, regex.to.del, "")) %>%
    dplyr::mutate(text = stringi::stri_replace_all_regex(text, regex.other, replace.other, vectorize_all = F)) %>%
    dplyr::mutate(text = textclean::replace_non_ascii(text)) %>%
    dplyr::mutate(text = stringi::stri_replace_all_regex(text, "\\s+", " ")) %>%
    dplyr::mutate(text = stringi::stri_trim_both(text))

  string <- string.table$text

  return(string)
}

#' Convert British English to Amrican English (Fast Version)
#' @description
#' Takes the input string and replaces British spelling with American spelling. In this
#' fast implementation of the function the exact punctuation (e.g. blanks) is not preserved
#'
#' @param string
#' The input string
#' @param replace.table
#' A table with British English and American English spelling
#' @return
#' A string with only American English spelling
#' @export
#' @examples
#' americanize_fast(c("capitalised asset", "profit centre"))
#'
americanize_fast <- function(string, replace.table = NULL) {
  `%>%` <- magrittr::`%>%`

  if (is.null(replace.table)) {
    replace.table <- tpfuns::table_stand_ae_be %>%
      dplyr::distinct(., .keep_all = TRUE)
  }
  string.table <- tibble::tibble(id = 1:length(string), text = string)

  string.table.words <- string.table %>%
    tidytext::unnest_tokens(words, text)

  string.table.uni <- string.table.words %>%
    dplyr::distinct(words) %>%
    dplyr::left_join(replace.table, by = c("words" = "UK")) %>%
    dplyr::mutate(US = dplyr::if_else(is.na(US), words, US))

  string.table <- string.table.words %>%
    dplyr::left_join(string.table.uni, by = "words") %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(text = stringi::stri_flatten(US, " "))

  string <- string.table$text

  return(string)
}

#' Lemmatization of english words
#'
#' @param string
#' The input string
#' @return
#' A character vector with lemmatized words
#' @export
#'
#' @examples
#' lemmatize_fast(c("assets", "depreciated"))
lemmatize_fast <- function(string) {
  `%>%` <- magrittr::`%>%`
  string.table <- tibble::tibble(id = 1:length(string), text = string)

  string.table.words <- string.table %>%
    tidytext::unnest_tokens(words, text)

  string.table.uni <- string.table.words %>%
    dplyr::distinct(words) %>%
    dplyr::mutate(words_stand = textstem::lemmatize_words(words))

  string.table <- string.table.words %>%
    dplyr::left_join(string.table.uni, by = "words") %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(text = stringi::stri_flatten(words_stand, " "))

  string <- string.table$text
  return(string)
}

#' Perform multiple operations on terms
#'
#' @description
#' This function combines the stand_chars_fast, americanize_fast, and lemmatize_fast function
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
#'
#' @examples
#' term_operation_fast(c("  ĆȦpītaĻĮSȆĎ", "centres"))
term_operation_fast <- function(string, stand.table = NULL, replace.table = NULL) {
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

  string.table <- tibble::tibble(id = 1:length(string), text = string) %>%
    dplyr::mutate(text = stringi::stri_trans_tolower(text)) %>%
    dplyr::mutate(text = stringi::stri_replace_all_regex(text, regex.to.space, " ")) %>%
    dplyr::mutate(text = stringi::stri_replace_all_regex(text, regex.to.del, "")) %>%
    dplyr::mutate(text = stringi::stri_replace_all_regex(text, regex.other, replace.other, vectorize_all = F)) %>%
    dplyr::mutate(text = textclean::replace_non_ascii(text)) %>%
    dplyr::mutate(text = stringi::stri_trim_both(text))

  string.table.words <- string.table %>%
    tidytext::unnest_tokens(words, text)

  string.table.uni <- string.table.words %>%
    dplyr::distinct(words) %>%
    dplyr::left_join(replace.table, by = c("words" = "UK")) %>%
    dplyr::mutate(US = dplyr::if_else(is.na(US), words, US)) %>%
    dplyr::mutate(words_stand = textstem::lemmatize_words(US)) %>%
    dplyr::select(words, words_stand)

  string.table <- string.table.words %>%
    dplyr::left_join(string.table.uni, by = "words") %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(text = stringi::stri_flatten(words_stand, " "))

  string <- string.table$text
  return(string)
}
