#' Escape Punctuation for Regex
#'
#' @param string
#' A String
#' @return
#' A String
#' @export
escape_regex <- function(string) {
  string = stringi::stri_replace_all_regex(string, "([[:punct:]])", "\\\\$1")
}
