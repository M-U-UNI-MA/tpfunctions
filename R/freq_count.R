#' Frequency Count for Terms in Text
#'
#' @description
#' This function takes a term list (either standardized or unstandardized) and counts the
#' occurances of terms in text files
#'
#' @param docs
#' A vector of paths to document text files (possibly retrieved by list.files())
#' @param terms
#' Either a vector of terms (if stand == FALSE) or a dataframe with the following columns: \cr
#' term: unstandardized terms\cr
#' term_stand: standardized terms\cr
#' term_stand_space: standardized terms without blanks (will be used for the frequency count)\cr
#' ngram: numbe rof words for the standardized term (term_stand)\cr
#' nchar: number of characters for the standardized term (term_stand)\cr
#'
#' If a vector with terms is provided the function prepare_term_list will be called first.
#' This function returns a dataframe with the columns shown above
#'
#' @param stand
#' Are the terms already standardized (see description of 'terms' above)? \cr
#' If TRUE a dataframe must be provided \cr
#' If FALSE terms will be standardized
#'
#' @return
#' A dataframe with adjusted and unadjusted frequency counts
#' @export
get_freq <- function(docs, terms, stand = FALSE) {
  `%>%` <- magrittr::`%>%`
  if (stand == FALSE) {
    term.table <- tpfuns::prepare_term_list(terms)
    cat("\rpreparing term list")
  } else {
    term.table <- terms
  }
  ngrams <- sort(unique(term.table$ngram), decreasing = TRUE)

  # reading text data  -------------------------------------------------------------------
  cat("\rTask: reading in text data                                                     ")
  text <- readtext::readtext(docs)

  cat("\rTask: standardizing text                                                       ")
  text$text <- tpfuns::stand_docs_fast(text$text)

  doc_names <- text$doc_id
  doc_number <- nrow(text)

  # frequency count (unadjusted) ---------------------------------------------------------
  freq <- lapply(ngrams, function(x) {
    cat("\r Task: getting unadjusted frequency - counting", x, "- grams                 ")
    text.token <- text %>%
      tidytext::unnest_tokens(phrase, text, "ngrams", n = x) %>%
      dplyr::mutate(row_id = dplyr::row_number()) %>%
      dplyr::mutate(phrase_space = stringi::stri_replace_all_fixed(phrase, " ", ""))

    match <- text.token %>%
      dplyr::inner_join(term.table, by = c("phrase_space" = "term_stand_space"))
    return(match)
  }) %>% dplyr::bind_rows()

  # preparing frequency for adjustement count --------------------------------------------
  freq <- freq %>%
    dplyr::arrange(doc_id) %>%
    dplyr::mutate(group_id = dplyr::group_indices(., doc_id))
  freq <- split(freq, freq$group_id)

  # getting adjusted frequencies ---------------------------------------------------------
  freq.adj <- lapply(freq, function(y) {
    doc_count <- which(doc_names == y$doc_id[1])
    cat("\rTask: adjusting frequency -", doc_count, "of", length(freq),
        "documents                                ")
    freq.adj <- y
    for (i in ngrams[!ngrams == 1]) {
      ids <- freq.adj %>% dplyr::filter(ngram == i) %>% dplyr::pull(row_id)
      ids <- unlist(lapply(ids, function(x) 0:(i - 1) + x))
      freq.adj <- freq.adj %>%
        dplyr::filter(!row_id %in% ids | ngram >= i)
    }
    return(freq.adj)
  })

  freq <- dplyr::bind_rows(freq) %>%
    dplyr::group_by(doc_id, phrase_space, phrase) %>%
    dplyr::summarise(freq = dplyr::n()) %>%
    dplyr::group_by(doc_id, phrase_space) %>%
    dplyr::summarise(phrase = stringi::stri_flatten(phrase, " <> "),
                     freq = sum(freq))


  freq.adj <- dplyr::bind_rows(freq.adj) %>%
    dplyr::group_by(doc_id, phrase_space) %>%
    dplyr::summarise(freq_adj = dplyr::n())

  freq <- freq %>%
    dplyr::left_join(freq.adj, by = c("doc_id", "phrase_space")) %>%
    tidyr::replace_na(list(freq_adj = 0)) %>%
    dplyr::rename(term_stand_space = phrase_space)

  return(freq)

}
