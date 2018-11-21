# GET PDF INFO ===========================================================================
#' Extracts metadata from pdf
#'
#' @description
#' This function is part of the 'pdf operations' (pop) function set\cr
#' This function reads a pdf and returns metadata
#'
#' @param file complete path to the pdf file
#'
#' @return a dataframe with 5 columns:\cr
#' readable:  binary\cr
#' n_page:    integer\cr
#' encrypted: binary\cr
#' locked:    binary\cr
#' size:      numeric (in KB)
#' @export
pop_pdf_info <- function(file) {
  `%>%` <- magrittr::`%>%`

  suppressMessages(
    info <- tryCatch(pdftools::pdf_info(file), error = function(e) "error")
  )

  info.tbl <- tibble::tibble(readable = NA, n_page = NA, encrypted = NA, locked = NA, size = NA)

  if (is.character(info)) {
    info.tbl$readable  = 0
    info.tbl$n_page    = NA
    info.tbl$encrypted = NA
    info.tbl$locked    = NA
    info.tbl$size      = round(file.size(file) / 1000, 2)
  } else {
    info.tbl$readable  = 1
    info.tbl$n_page    = info[["pages"]]
    info.tbl$encrypted = as.numeric(info[["encrypted"]])
    info.tbl$locked    = as.numeric(info[["locked"]])
    info.tbl$size      = round(file.size(file) / 1000, 2)

  }



  return(info.tbl)

}

