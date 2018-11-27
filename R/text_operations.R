# DOCUMENT STATISTICS ====================================================================
#' Text Statistics for Documents
#'
#' @description
#' This function is part of the 'text operation' (tox) dunction set\cr
#' This function counts pages, words and characters, it determines the filesize and
#' calculates ratios. (optional the language of the text is returned)
#'
#' @param path
#' The path to the textfiles (get with list.files())
#'
#' @param lan
#' If the language of the text is unknown it can be determined
#'
#' @return
#' A Dataframe with the following columns:
#' doc_id: the filename of the text file\cr
#' n_page: number of pages\cr
#' n_word: number of words\cr
#' n_char: number of character\cr
#' r_word_page: ratio of words per page\cr
#' r_char_word: ratio of characters per word\cr
#' size: size of the text file in KB\cr
#' lan_1: first detected language (optional)\cr
#' lan_2: second detected language (optional)\cr
#' rel_1: reliability of the first detected language (optional)\cr
#' rel_2: reliability of the second detected language (optional)
#' @export
tox_doc_stats <- function (path, lan = c("single", "mixed")) {
  `%>%` <- magrittr::`%>%`
  text <- readtext::readtext(path)

  text.stats <- tibble::tibble(
    doc_id = text$doc_id,
    n_page = stringi::stri_count_regex(text$text, "\\f"),
    n_word = stringi::stri_count_words(text$text),
    n_char = stringi::stri_count_boundaries(text$text, type = "character") - (n_word - 1),
    r_word_page = round(n_word / n_page, 4),
    r_char_word = round(n_char / n_word, 4),
    size   = round(file.size(path) / 1000, 4)
  )

  if (lan == "mixed") {
    lan.stats <- lapply(1:nrow(text), function(x) {
      if (text.stats$n_word[x] == 0) {
        lan <- tibble::tibble(
          lan_1 = "un",
          lan_2 = "un",
          rel_1 = 0,
          rel_2 = 0
        )
      } else {
        lan.detect <- cld2::detect_language_mixed(text$text[x])[["classificaton"]]
        lan <- tibble::tibble(
          lan_1 = lan.detect$code[1],
          lan_2 = lan.detect$code[2],
          rel_1 = lan.detect$proportion[1],
          rel_2 = lan.detect$proportion[2]
        )
      }
    }) %>% dplyr::bind_rows()
    text.stats <- dplyr::bind_cols(text.stats, lan.stats)
  }

  if (lan == "single") {
    text.stats$lan <- cld2::detect_language(text$text)
}
  return(text.stats)
}
