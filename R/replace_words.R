#' Replacement of commonly misspelled words
#'
#' @param string
#' The input string
#'
#' @param replace_table
#' A datframe with two columns: \cr
#' 1. column "word":    The word or phrase to search for in the string \cr
#' 2. column "replace": The word or phrase which replaces the the word or phrase in
#' the first column \cr
#' Note: If no table is specified the default table will be used \cr
#' Note: The table will be checked for incorrect replacenemts
#' Note: The table will be checked for replacement dependencies (A -> B, B -> C <=> A -> C)
#'
#' @return A string with replaced words
#' @export
#'
#' @examples
#' # Test String:
#' test <- c("investments", "assets")
#' replace_words(test)
#'
replace_words <- function(string, replace_table = NULL) {
  `%>%` <- magrittr::`%>%`

  # read table, either default of specified by replace_table
  if (is.null(replace_table)) {
    replace.word <- tpfuns::word_replace
  } else {
    replace.word <- replace_table
  }

  # get unique word and replacement combinations
  replace.word <- replace.word %>% dplyr::distinct(word, replace)

  # check inconsistencies in replacement table
  inc <- replace.word %>% dplyr::group_by(word) %>% dplyr::filter(n() > 1)
  if(nrow(inc) > 0) stop("Duplicated word-replacement pairs")

  # check inconsistencies in replacement table (A -> B, B -> C <=> A -> C)
  i = 0
  while (all(replace.word$replace %in% replace.word$word == FALSE) == FALSE) {
    i <- i + 1
    search <- paste0("\\b", replace.word$replace[i], "\\b")
    check <- stringi::stri_detect_regex(replace.word$word, search)
    if (all(check == FALSE)) next
    check.id <- which(check == TRUE)
    replace.word$replace[i] <- replace.word$replace[check.id]
    replace.word <- replace.word[-check.id, ]

    i <- i - 1

  }
    string.table <- tibble::tibble(phrases = string) %>%
      dplyr::mutate(phrase_id = dplyr::row_number()) %>%
      tidytext::unnest_tokens(words, phrases, "words")

    string.table.uni <- string.table %>%
      dplyr::distinct(words) %>%
      dplyr::left_join(replace.word, by = c("words" = "word")) %>%
      dplyr::mutate(replace = dplyr::if_else(is.na(replace), words, replace))

    string.table <- string.table %>%
      dplyr::left_join(string.table.uni, by = "words") %>%
      dplyr::group_by(phrase_id) %>%
      dplyr::summarise(phrase = stringi::stri_flatten(replace, " ")) %>%
      dplyr::arrange(phrase_id)
    string <- string.table$phrase
    string <- stringi::stri_trim_both(string)
    return(string)

  string <- stringi::stri_replace_all_regex(string, regex, replace, vectorize_all = FALSE)
  string <- stringi::stri_trim_both(string)

}
