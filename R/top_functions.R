# PUNCTUATIONS ===========================================================================

#' Punctuation Operations
#'
#' @description
#' This function is part of the 'term operations' (top) function set\cr
#' It takes and input string and standardizes / substitutes / replaces punctuations and
#' characters
#'
#' @param string
#' A character string
#' @param punct.stand
#' Shall the input string be standardized? (TRUE/FALSE)
#'
#' @param lower
#' Shall the string be transfored to lower case? (TRUE/FALSE) \cr
#' This option is only available if punct.stand == TRUE
#'
#' @param punct.sub
#' Shall certein punctuation characters be replaced? (TRUE/FALSE) \cr
#' If TRUE the ampersand (&) and plus (+) characters will be replace with 'and'
#'
#' @param punct.rem
#' Shall punctuation be removed? (One of none/partial/complete) \cr
#' none:     No removal of punctuations\cr
#' partial:  All punctuation except ampersand (&), forward slash (/) and hyphen (-) is removed\cr
#' complete: All punctuation is removed
#'
#' @return
#' A charcter string
#' @export
#'
#' @examples
#' library(tpfuns)
#' input.string <- "TeSt STring: ( This, ) String [ Is a ] Test & ít-is badlý written"
#'
#' top_punct(input.string, TRUE, TRUE, punct.rem = "none", FALSE)
#' top_punct(input.string, TRUE, TRUE, "partial", FALSE)
#' top_punct(input.string, TRUE, TRUE, "complete", FALSE)
#'
#' top_punct(input.string, TRUE, TRUE, "partial", TRUE)
top_punct <- function(string, lower = TRUE, punct.stand = TRUE,
                      punct.rem = c("none", "partial", "complete"),
                      punct.sub = FALSE) {

  # set internal functions ===============================================================
  repl <- function(string, pattern, replace)
    stringi::stri_replace_all_regex(string, pattern, replace)

  # checks ===============================================================================
  check <- c("none", "partial", "complete")
  if (length(punct.rem) > 1 | !any(punct.rem %in% check))
    stop('punct.rem must be ONE of "none" / "partial" / "complete"')

  ############################ PUNCTUATION STANDARDIZATION ###############################

  string.adj <- string
  if (isTRUE(punct.stand)) {

    # lower ==============================================================================
    if (lower == TRUE) string.adj <- stringi::stri_trans_tolower(string.adj)

    # blank characters ===================================================================
    string.adj <- repl(string.adj, "([[:space:]]|[[:blank:]])+", " ")

    # quotes =============================================================================
    string.adj <- repl(string.adj, "[\x91-\x94\xB4\x60]+", "'")

    # hyphens ============================================================================
    string.adj <- repl(string.adj, "(\\s+)?[\x2D\x96-\x97]+(\\s+)?", "-")

    # ampersand ==========================================================================
    string.adj <- repl(string.adj, "(\\s+)?\x26(\\s+)?", "&")

    # plus sign ==========================================================================
    string.adj <- repl(string.adj, "(\\s+)?\\\x2B(\\s+)?", "+")

    # comma ================================================================================
    string.adj <- repl(string.adj, "(\\s+)?(?<=[a-zA-Z\\s])\x2C+(?=[a-zA-Z\\s])(\\s+)?", ", ")

    # forward slash ======================================================================
    string.adj <- repl(string.adj, "(\\s+)?\x2F(\\s+)?", "/")

    # left paranthesis ===================================================================
    string.adj <- repl(string.adj, "(\\s+)?\\\x28(\\s+)?", " (")

    # right paranthesis ==================================================================
    string.adj <- repl(string.adj, "(\\s+)?\\\x29(\\s+)?", ") ")

    # left square bracket ================================================================
    string.adj <- repl(string.adj, "(\\s+)?\\\x5B(\\s+)?", " [")

    # right square bracket ===============================================================
    string.adj <- repl(string.adj, "(\\s+)?\\\x5D(\\s+)?", "] ")

    # colon ==============================================================================
    string.adj <- repl(string.adj, "(\\s+)?\3A(\\s+)?", ": ")

    # semicolon ==========================================================================
    string.adj <- repl(string.adj, "(\\s+)?\3B(\\s+)?", "; ")

    # currencies =========================================================================
    string.adj <- repl(string.adj, "(\\p{Sc})(\\s+)?([\\d,\\.]+)", "$1$3")

    # punctuation ========================================================================
    string.adj <- repl(string.adj, "\\s([\\.\\!\\?])", "$1")

    # abbreviations ======================================================================
    string.adj <- repl(string.adj, "(\\b\\w\\b\\.)\\s?(?!\\w{2,})", "$1")

    # convert to ascii charcters =========================================================
    string.adj <- stringi::stri_trans_general(string.adj, "latin-ascii")

    # trim and double spaces =============================================================
    string.adj <- stringi::stri_trim_both(repl(string.adj, "\\s+", " "))

  }
  ############################ PUNCTUATION SUBSTITUTION ##################################

  # punctuation substitution =============================================================
  if (isTRUE(punct.sub))
    string.adj <- repl(string.adj, "(\\s+)?[&\\+](\\s+)?", " and ")

  ############################ PUNCTUATION REPLACEMENT ###################################

  # punctuation replacement (partial / complete) =========================================
  if (punct.rem %in% c("partial", "complete")) {
    string.adj <- repl(string.adj, "(?<=\\w)'s|'|\\.", "")

    # punctuation replacement (partial) ==================================================
    if (punct.rem == "partial")
      string.adj <- gsub("(?![\\&\\/\\-])[[:punct:]]", " ", string.adj, perl = TRUE)

    if (punct.rem == "complete")
      string.adj <- gsub("[[:punct:]]", " ", string.adj, perl = TRUE)

    string.adj <- repl(string.adj, "([[:space:]]|[[:blank:]])+", " ")
    string.adj <- stringi::stri_trim_both(string.adj)
  }


  # replace adjusted string with original, in case asjusted string  is empty =============
  string.adj[which(string.adj == "")] <- string[which(string.adj == "")]

  # return output ========================================================================
  return(string.adj)

}


# TERM LOOKUP  ===========================================================================
#' Hash Table/Dictionary Lookup
#'
#' @description
#' Replaces the individual elements of the vector 'terms' by the reassign key
#' (largely based on qdapTools::lookup)
#'
#' @param terms
#' A vector of terms to undergo a lookup
#' @param key.match
#' Takes one of the following:\cr
#' (1) a two column data.frame of a match key and reassignment column\cr
#' (2) a named list of vectors (Note: if data.frame or named list supplied no key reassign needed)\cr
#' (3) a single vector match key.
#' @param key.reassign
#' A single reassignment vector supplied if key.match is not a two column data.frame/named list
#' @param tokenize
#' If TRUE terms will be first split by word boundaries and than put back together
#'
#' @return
#' A new vector with reassigned values
#' @export
#'
#' @examples
#' library(tpfuns)
#' a <- c("property and plant", "property", "plant")
#' b <- c("property", "plant")
#' c <- c("replace 1", "replace 2")
#' top_term_lookup(a, b, c)
#' top_term_lookup(a, b, c, TRUE)
top_term_lookup <- function(terms, key.match, key.reassign = NULL, tokenize = FALSE) {

  if (any(duplicated(key.match))) stop("matching string must not have duplicates")

  if (isTRUE(tokenize)) {
    a <- tokenizers::tokenize_regex(terms, "\\b")
    b <- unlist(lapply(1:length(a), function(y) rep(y, length(a[[y]]))))
    a <- unlist(a)
  } else {
    a <- terms
  }

  a <- qdapTools::lookup(a, key.match, key.reassign, NULL)

  if (isTRUE(tokenize)) {
    a <- unname(unlist(lapply(split(a, b), stringi::stri_flatten)))
  }
  return(a)
}



# LOOKUP WRAPPER =========================================================================
#' Wrapper around the top_term_lookup function
#'
#' @param string
#' Input string
#' @param type
#' Determines which lookup table to use
#' @param tokenize
#' If TRUE terms will be first split by word boundaries and than put back together
#'
#' @return
#' A new vector with reassigned values
#' @export
top_lookup_wrap <-
  function(string,
           type = c("lemmas", "lemmas_select", "americanize", "errors", "split"),
           tokenize = FALSE) {

    if (length(type) != 1)
      stop("you must select ONE type")
    if (!type %in% c("lemmas", "lemmas_select", "americanize", "errors", "split"))
      stop("wrong type selected")

    if (type == "lemmas") {
      match <- tpfuns::table_lemmas$term
      reass <- tpfuns::table_lemmas$lemma
    }
    if (type == "lemmas_select") {
      match <- tpfuns::table_lemmas_selected$term
      reass <- tpfuns::table_lemmas_selected$lemma
    }
    if (type == "errors") {
      match <- tpfuns::table_errors$match
      reass <- tpfuns::table_errors$reassign
    }
    if (type == "split") {
      match <- tpfuns::table_split$match
      reass <- tpfuns::table_split$reassign
    }
    if (type == "americanize") {
      match <- tpfuns::table_americanize$term_uk
      reass <- tpfuns::table_americanize$term_us
    }

    tpfuns::top_term_lookup(
      terms = string,
      key.match = match,
      key.reassign = reass,
      tokenize = tokenize
    )

  }



