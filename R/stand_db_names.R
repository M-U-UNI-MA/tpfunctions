#' Standardizes Names from Accounting Data Provider (e.g. Datastream)
#'
#' @param string String with company names to be standardized
#'
#' @return A string with standardized names
#' @export
stand_db_names <- function(string, regex.table = NULL, standardized = FALSE) {
  clean.table <- tpfuns::db_clean
  regex <- clean.table$regex

  if (standardized == FALSE) {
    string <- tpfuns::stand_chars(string)
  }

  string <- stringi::stri_replace_all_regex(string, regex, "", vectorize_all = F)

return(string)

}
