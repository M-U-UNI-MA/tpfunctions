# CHECK INPUT TABLES FOR WORD REPLACEMENT ================================================
#' Check Input Tables for Word Replacement
#'
#' @param replace_table
#' A Replacement Table (needs 2 columns: "word" and "replace")
#'
#' @return
#' Errors
check_repl_list <- function(replace_table) {
  check.dup <- replace_table$word[which(duplicated(replace_table$word) == TRUE)]
  if (length(check.dup) > 0)
    stop(paste0("Duplicated word(s): ", stringi::stri_flatten(check.dup, " | ")))

  check.trans <- replace_table$replace[which(replace_table$replace %in% replace_table$word)]
  if (length(check.trans) > 0)
    stop(paste0("Transitive word(s) replacement:", stringi::stri_flatten(check.trans, " | ")))

}

# REPLACE MISSPELLED WORDS ===============================================================
#' Standardize Commonly Misspelled Words in Strings
#'
#' @description
#' This function is part of the 'company operations' (cop) function set\cr
#' This function replaces and standardizes commonly misspelled words in company names
#' @param string
#' A charcter string
#' @param replace_table
#' A Replacement Table (needs 2 columns: "word" and "replace")\cr
#' If NULL the default table will be used
#'
#' @return
#' A character string with standardized words
#'
#' @export
#'
#' @examples
#' cop_repl_words("1st")
cop_repl_words <- function(string, replace_table = NULL) {
  `%>%` <- magrittr::`%>%`
  # read table, either default of specified by replace_table -----------------------------
  if (is.null(replace_table)) {
    table.repl.words <- tpfuns::table_repl_words
  } else {
    table.repl.words <- replace_table
  }

  check_repl_list(table.repl.words)

  table.string <- tibble::tibble(term = string) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    tidytext::unnest_tokens(words, term, token = "regex", pattern = "\\b") %>%
    dplyr::left_join(table.repl.words, by = c("words" = "word")) %>%
    dplyr::mutate(words = dplyr::if_else(is.na(replace), words, replace)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(term = stringi::stri_flatten(words))

  string <- table.string$term
  return(string)
}

# REPLACE LEGAL ENTITIES =================================================================
#' Remove Legal Entities from String
#'
#' @description
#' This function is part of the 'company operations' (cop) function set\cr
#' This function removes legal entities from company names
#'
#' @param string
#' A chracter string
#'
#' @return
#' A character String without legal entities
#'
#' @export
#'
#' @examples
#' tpfuns::cop_rem_le("basf se")
cop_rem_le <- function(string) {
  regex <- tpfuns::table_legal_entities$regex
  string <- stringi::stri_replace_all_regex(string, regex, "", vectorize_all = FALSE)
  string <- stringi::stri_trim_both(string)

  return(string)

}


