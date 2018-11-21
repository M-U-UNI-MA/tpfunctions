#' Extracts metadata from pdf
#'
#' @param file complete path to the pdf file
#'
#' @return a dataframe with 4 columns (readable (logical), n_page (integer),
#' encrypted (logical), locked (logical))
#' @export
get_pdf_info <- function(file) {
  suppressMessages(
    info <- tryCatch(pdftools::pdf_info(file), error = function(e) "error")
  )

  info.tbl <- tibble::tibble(readable = NA, n_page = NA, encrypted = NA, locked = NA )

  if (is.character(info)) {
    info.tbl$readable  = FALSE
    info.tbl$n_page    = NA
    info.tbl$encrypted = NA
    info.tbl$locked    = NA
  } else {
    info.tbl$readable  = TRUE
    info.tbl$n_page    = info[["pages"]]
    info.tbl$encrypted = info[["encrypted"]]
    info.tbl$locked    = info[["locked"]]
  }
  return(info.tbl)

}

