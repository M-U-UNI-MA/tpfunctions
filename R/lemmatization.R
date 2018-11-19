#' Wrapper around the TreeTagger Lemmatizer
#'
#' @description
#' This function uses the TreeTagger Lemmatizer and is especially designed for term list
#' (i.e. seperate rows of terms are lemmatized and the POS tagging is returned)
#'
#' @param terms
#' A character string of terms to be tagged and lemmatized
#' @param lan
#' The language of the terms
#'
#' @param pos
#' Shall the part of speech tags be returned? Only reasonable for small terms not for
#' whole texts!
#'
#' @return
#' A dataframe with 3 columns:\cr
#' term:  The term to be lemmatized\cr
#' lemma: The lemmatized term\cr
#' pos:   The part of speech taggings for the tokens within the term\cr
#' Note:  There is some standardization of punctuation performed, so the 'term' column
#' does not necessarily hase the same punctuation as the original term. Nonetheless, the
#' order of the terms is preserved!
#'
#'
#' @export
#'
#' @examples
#' lem_term_tg(c("assets and liabilities", "something else, lots of"))
#' lem_term_tg(c("assets and liabilities", "something else, lots of"), pos = TRUE)
lem_term_tg <- function(terms, lan = c("en", "de", "es", "it", "fr"), pos = FALSE) {
  `%>%` <- magrittr::`%>%`

  suppressWarnings(
  tag <- if (lan == "en") {"tag-english"}
  else if   (lan == "de") {"tag-german"}
  else if   (lan == "es") {"tag-spanish"}
  else if   (lan == "it") {"tag-italian"}
  else if   (lan == "fr") {"tag-french"}
  )

  term <- lapply(1:length(terms), function(x){
    dplyr::bind_rows(tibble::tibble(term = terms[x]), tibble::tibble(term = paste0("TERMSEP_", x)))
  }) %>% dplyr::bind_rows() %>% dplyr::pull(term)

  tmp.file.in  <- paste0(tempfile(), ".txt"); tmp.file.out <- paste0(tempfile(), ".txt")

  writeLines(term, tmp.file.in)

  system(paste(tag, tmp.file.in, tmp.file.out), intern = TRUE)

  suppressWarnings(
    token <- readr::read_tsv(tmp.file.out, col_names = F, col_types = readr::cols(.default = "c"))
    )

  f_1 <- function(col) stringi::stri_detect_fixed(col, "TERMSEP_")
  f_2 <- function(col) as.integer(stringi::stri_replace_all_fixed(col, "TERMSEP_", ""))


  colnames(token) <- c("token", "pos", "lemma")

  suppressWarnings(
  token <- token %>%
    dplyr::mutate(id = ifelse(f_1(token), f_2(token), NA)) %>%
    tidyr::fill(id, .direction = "up") %>%
    dplyr::filter(!f_1(token))
  )

  if (isTRUE(pos)) {
  token <- token  %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      term  = stringi::stri_flatten(token, " "),
      lemma = stringi::stri_flatten(lemma, " "),
      pos   = stringi::stri_flatten(pos, " | ")
      )
  } else {
    token <- token  %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(
        term  = stringi::stri_flatten(token, " "),
        lemma = stringi::stri_flatten(lemma, " ")
      )
  }


  token <- token  %>%
    dplyr::ungroup() %>%
    dplyr::select(-id) %>%
    dplyr::mutate(
      term  = tpfuns::stand_chars_1(term),
      lemma = tpfuns::stand_chars_1(lemma)
    )

  unlink(c(tmp.file.in, tmp.file.out))
  return(token)

}
