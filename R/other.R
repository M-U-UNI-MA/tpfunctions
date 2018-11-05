#' Trim and replace whitespaces
#'
#' @description
#' A simple wrapper around some stringi function \cr
#' - Replaces multiple blanks with a single blank \cr
#' - Trims both sides of the string \cr
#' - Convert all characters to lower case letters
#'
#' @param string
#' A character of character vector
#'
#' @return
#' @export
lower_ws <- function(string){
  string <- stringi::stri_trans_tolower(string)
  string <- stringi::stri_replace_all_regex(string, "\\s+", " ")
  string <- stringi::stri_trim_both(string)
}
