#' Standardization of Strings
#'
#' @param string A string or a vector of strings to be standardized
#' @param stand.table A table with standardizations (needs to have a column for the
#' chracters that needs to be standardized (in hex values) and a column for the replacement)
#' See the attached table as example. If no table is specified the default table will be used
#'
#' @return A string
#' @export
stand_chars <- function(string, stand.table = NULL) {
  `%>%` <- magrittr::`%>%`
  if (is.null(stand.table)) stand.table <- tpfuns::stand_table

  stand.table <- dplyr::filter(stand.table, !replace == "<none>") %>%
    dplyr::mutate(replace = stringi::stri_replace_all_fixed(replace,
                                                            c("<space>", "<del>"),
                                                            c(" ", ""),
                                                            vectorize_all = FALSE)
                  )

  string <- stringi::stri_enc_toutf8(string)
  string <- stringi::stri_replace_all_regex(string,
                                            stand.table$hex,
                                            stand.table$replace,
                                            vectorize_all = FALSE
                                            )
  string <- stringi::stri_replace_all_regex(string, "\\s+", " ")
  string <- stringi::stri_trim_both(string)
  return(string)
}
