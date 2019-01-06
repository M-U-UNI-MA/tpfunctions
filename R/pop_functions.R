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
# usethis::use_package("pdftools")
pop_pdf_info <- function(file) {
  `%>%` <- magrittr::`%>%`
  suppressMessages(info <- tryCatch(pdftools::pdf_info(file), error = function(e) "error"))

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


# PDF TO TEXT ============================================================================
#' Converts a PDF to text file
#'
#' @description
#' This function is part of the 'pdf operations' (pop) function set\cr
#' This function converts pdf files into plain text files
#'
#' @param file
#' A PDF file
#'
#' @param outdir
#' The folder where the txt file should be saved\cr
#' (If NULL a new folder called 'txt' will be created in the same directory where the pdf
#' file is located)
#'
#' @param wait
#' Shall the program wait to trigger another instance of pdftotext.exe? (meaningfull if
#' a lot of different pdf files have to be converted, but is slower)
#'
#'
#' @return A text file (the text file will have the same name as the pdf file)
#' @export
pop_pdf_to_txt <- function(file, outdir = NULL, wait = FALSE) {
  if (is.null(outdir)) {
    outdir <- paste0(dirname(file), "/txt")
    if (!dir.exists(outdir)) dir.create(outdir, FALSE)
  } else {
    if (!dir.exists(outdir)) dir.create(outdir, FALSE)
  }

  outfile <- paste0(outdir, "/", gsub("\\.pdf", ".txt", basename(file)))
  file    <- paste0('"', file, '"')
  outfile <- paste0('"', outfile, '"')

  suppressWarnings(tryCatch(
    system(paste('"pdftotext.exe"', file, outfile), ignore.stderr = TRUE, wait = wait),
    error = function(e)
      "error"
  ))
}
# DECRYPT PDFs ===========================================================================
#' Decrypt pdfs
#'
#' @description
#' A wrapper around the qpdf command line tool.
#' - First:  The qpdf tool must be downloaded (https://sourceforge.net/projects/qpdf/files/qpdf/).\cr
#' - Second: The zipped file must be extracted.\cr
#' - Third, environment variables must be updated.\cr
#' Program is tested on qpdf version 8.1.0.
#'
#'
#' @param path.in complete path to the encrypted pdf file
#' (must be different than path.out)
#' @param path.out complete path where the decrypted file should be written
#' (must be different than path.in)
#'
#' @return A pdf file (decrypted)
#' @export
pop_decrypt_pdf <- function(file.in, file.out) {
  tryCatch({
    system(paste('qpdf --decrypt', file.in, file.out))
  }, error = function(e)
    "error")
}
