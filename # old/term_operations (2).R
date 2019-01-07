# STANDARDIZE PUNTUATION =================================================================
#' Standardize Punctuation for Words or Strings
#'
#' @description
#' This function is part of the 'term operations' (top) function set\cr
#' This function takes a charcter string and standardizes punctuation. (e.g. it ensures
#' correct placements of hyphens, standardizes space characters, lowers and trims strings,
#' ...)
#' @param string
#' A charcter string
#'
#' @param lower
#' Shall the string be lowered?
#'
#' @param ascii
#' Shall non-ascii characters be converted to ascii characters?
#'
#' @return
#' A standardized character string
#' @export
#'
#' @examples
#' top_stand_punct("Hallo    Paul , I´´m cUrrently looking for money ( cash   ) [ or equivalents ] ")
top_stand_punct <- function(string, lower = TRUE, ascii = TRUE) {

  repl <- function(string, pattern, replace)
    stringi::stri_replace_all_regex(string, pattern, replace)

  string.adj <- string
  # blank characters =====================================================================
  string.adj <- repl(string.adj, "([[:space:]]|[[:blank:]])+", " ")

  # quotes ===============================================================================
  string.adj <- repl(string.adj, "[\x91-\x94\xB4\x60]+", "'")

  # hyphens ==============================================================================
  string.adj <- repl(string.adj, "[\x96-\x97]+", "-")

  # ampersand ============================================================================
  string.adj <- repl(string.adj, "(\\s+)?\x26(\\s+)?", "&")

  # plus sign ============================================================================
  string.adj <- repl(string.adj, "(\\s+)?\\\x2B(\\s+)?", "+")

  # comma ================================================================================
  string.adj <- repl(string.adj, "(\\s+)?(?<=[a-zA-Z\\s])\x2C+(?=[a-zA-Z\\s])(\\s+)?", ", ")

  # forward slash ========================================================================
  string.adj <- repl(string.adj, "(\\s+)?\x2F(\\s+)?", "/")

  # left paranthesis =====================================================================
  string.adj <- repl(string.adj, "(\\s+)?\\\x28(\\s+)?", " (")

  # right paranthesis ====================================================================
  string.adj <- repl(string.adj, "(\\s+)?\\\x29(\\s+)?", ") ")

  # left square bracket ==================================================================
  string.adj <- repl(string.adj, "(\\s+)?\\\x5B(\\s+)?", " [")

  # right square bracket =================================================================
  string.adj <- repl(string.adj, "(\\s+)?\\\x5D(\\s+)?", "] ")

  # colon ================================================================================
  string.adj <- repl(string.adj, "(\\s+)?\3A(\\s+)?", ": ")

  # semicolon ============================================================================
  string.adj <- repl(string.adj, "(\\s+)?\3B(\\s+)?", "; ")

  # currencies ===========================================================================
  string.adj <- repl(string.adj, "(\\p{Sc})(\\s+)?([\\d,\\.]+)", "$1$3")

  # punctuation ==========================================================================
  string.adj <- repl(string.adj, "\\s([\\.\\!\\?])", "$1")

  # abbreviations ========================================================================
  string.adj <- repl(string.adj, "(\\b\\w\\b\\.)\\s?(?!\\w{2,})", "$1")

  # convert to ascii charcters ===========================================================
  string.adj <- stringi::stri_trans_general(string.adj, "latin-ascii")

  # trim and double spaces ===============================================================
  string.adj <- stringi::stri_trim_both(repl(string.adj, "\\s+", " "))

  # lower ================================================================================
  if (lower == TRUE) string.adj <- stringi::stri_trans_tolower(string.adj)

  # replace adjusted string with original, in case asjusted string  is empty =============
  string.adj[which(string.adj == "")] <- string[which(string.adj == "")]

  # return output ========================================================================
  return(string.adj)

}

# AMERICANIZE ============================================================================
#' Americanize Words or Strings
#'
#' @description
#' This function is part of the 'term operations' (top) function set\cr
#' This function converts words in character strings from British English spelling to
#' American English Spelling
#'
#' @param string
#' A charcter string
#'
#' @return
#' A character string with only American English spelling
#' @export
#'
#' @examples
#' americanize(c("summarised, assets", "yoghurt"))
top_americanize <- function(string) {
  `%>%` <- magrittr::`%>%`
  table.americanize <- tpfuns::table_americanize

  table.string <- tibble::tibble(term = string) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    tidytext::unnest_tokens(words, term, token = "regex", pattern = "\\b") %>%
    dplyr::left_join(table.americanize, by = c("words" = "term_uk")) %>%
    dplyr::mutate(words = dplyr::if_else(is.na(term_us), words, term_us)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(term = stringi::stri_flatten(words))

  string <- table.string$term
  return(string)
}
# REMOVE PUNCTUATION =====================================================================
#' Removes Punctuation in Words or Strings
#'
#' @description
#' This function is part of the 'term operations' (top) function set\cr
#' This function takes a charcter string and removes all punctuations (except the single
#' quote " ' " which is completely removed, all other punctuations are replaced by a space
#' character)
#'
#' @param string
#' A charcter string
#' @param punct.replacement
#' Shall specific punctuation be replaced?\cr
#' currently implemented: & -> and; + -> and
#'
#' @return
#' A character string without punctuations
#'
#' @export
#'
#' @examples
#' top_rem_punct("David's assets,, are   boring")
top_rem_punct <- function(string, punct.replacement = FALSE) {

  repl <- function(string, pattern, replace)
    stringi::stri_replace_all_regex(string, pattern, replace)

  string.adj <- string
  string.adj <- repl(string.adj, "'s", "")
  string.adj <- repl(string.adj, "'", "")
  string.adj <- repl(string.adj, "\\.", "")

  if (punct.replacement == TRUE) {
    string.adj <- repl(string.adj, "(\\s+)?[&\\+](\\s+)?", " and ")
  }

  string.adj <- repl(string.adj, "[[:punct:]]", " ")
  string.adj <- repl(string.adj, "([[:space:]]|[[:blank:]])+", " ")
  string.adj <- stringi::stri_trim_both(string.adj)

  string.adj[which(string.adj == "")] <- string[which(string.adj == "")]
  return(string.adj)

  return(string.adj)
}


# TOKENIZE AND LEMMATIZE WITH POS ========================================================
#' Helper Function for top_lem_doc() and top_lem
#'
#' @param string
#' A string
#'
#' @return
#' A dataframe
tok_pos_lem <- function(string) {
  `%>%` <- magrittr::`%>%`

  tag <- "tag-english_pos"

  # get tempfile names -------------------------------------------------------------------
  tmp.file.in  <- paste0(tempfile(), ".txt")

  # do lemmatization and POS tagging -----------------------------------------------------
  writeLines(string, tmp.file.in)
  token <- tibble::tibble(x = system(paste(tag, tmp.file.in), intern = TRUE)) %>%
    tidyr::separate(x, into = c("token", "pos", "lemma"), sep = "\t") %>%
    dplyr::mutate(lemma = dplyr::if_else(lemma == "@card@" | is.na(lemma), token, lemma))
  unlink(tmp.file.in)
  return(token)
}


# LEMMATIZE DOCUMENTS ====================================================================
#' Tokenize and Lemmatize Documents (with sentence split)
#'
#' @param string
#' The input document as a charcter string
#'
#' @return
#' A list with tokenized words and sentence split
#' @export
top_lem_doc <- function(string) {
  `%>%` <- magrittr::`%>%`

  token <- tok_pos_lem(string)
  token$pos[nrow(token)] <- "SENT"

  sent <- which(token$pos == "SENT")
  if (length(sent) == 1) {
    sent.id <- rep(1, nrow(token))
  } else {
    sent <- c(sent[1], sapply(2:length(sent), function(x) sent[x] - sent[x - 1]))
    sent.id <- unlist(lapply(1:length(sent), function(x) rep(x, sent[x])))
  }

  sentence <- token %>%
    dplyr::mutate(sent_id = sent.id) %>%
    dplyr::group_by(sent_id) %>%
    dplyr::summarise(sent_orig = stringi::stri_flatten(token, " "),
                     sent_lemma = stringi::stri_flatten(lemma, " ")) %>%
    dplyr::mutate(sent_orig = tpfuns::top_stand_punct(sent_orig, lower = FALSE)) %>%
    dplyr::mutate(sent_lemma = tpfuns::top_stand_punct(sent_lemma, lower = TRUE))

  out <- list(token, sentence)
  names(out) <- c("token", "sentence")

  return(out)

}

# LEMMATIZE TERMS ========================================================================
#' Tokenize and Lemmatize Terms
#'
#' @param string
#' A string of terms
#'
#' @return
#' A list with terms and tokens
#' @export
top_lem_term <- function(string) {
  `%>%` <- magrittr::`%>%`

  # ensure that in case strings are treated seperately -----------------------------------
  string <- lapply(1:length(string), function(x) {
    dplyr::bind_rows(tibble::tibble(term = string[x]),
                     tibble::tibble(term = "STRINGSEP"))
  }) %>% dplyr::bind_rows() %>% dplyr::pull(term)

  token <- tok_pos_lem(string)

  sep <- which(token$token == "STRINGSEP")
  if (length(sep) == 1) {
    sep.id <- rep(1, nrow(token))
  } else {
    sep <- c(sep[1], sapply(2:length(sep), function(x) sep[x] - sep[x - 1]))
    sep.id <- unlist(lapply(1:length(sep), function(x) rep(x, sep[x])))
  }

  token <- token %>%
    dplyr::mutate(string_id = sep.id) %>%
    dplyr::filter(!token == "STRINGSEP") %>%
    dplyr::mutate(lemma = stringi::stri_trans_tolower(lemma)) %>%
    dplyr::select(string_id, dplyr::everything())

  term <- token %>%
    dplyr::group_by(string_id) %>%
    dplyr::summarise(term_orig = stringi::stri_flatten(token, " "),
                     term_lemma = stringi::stri_flatten(lemma, " ")) %>%
    dplyr::mutate(term_orig = tpfuns::top_stand_punct(term_orig, lower = FALSE)) %>%
    dplyr::mutate(term_lemma = tpfuns::top_stand_punct(term_lemma, lower = TRUE))

  out <- list(token, term)
  names(out) <- c("token", "term")

  return(out)

}

#' Replace strings
#'
#' @param table a
#' @param col a
#' @param string.match a
#' @param string.repl a
#'
#' @return a
#' @export
#'
#' @examples a
top_repl_strings <- function(table, col, string.match, string.repl) {
  match.1 <- fastmatch::fmatch(table[[col]], string.match)
  match.2 <- which(!is.na(match.1))
  match.1 <- match.1[!is.na(match.1)]
  table[match.2, col] <- string.repl[match.1]
  return(table)
}
