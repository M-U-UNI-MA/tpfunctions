#' Read Text Files
#'
#' @description
#' This function reads either a single of multiple text files into a dataframe. The content
#' of each textfile is stored in a single string in one row of the datframe
#'
#' @param path
#' The path to the textfiles (get with list.files())
#' @param pb
#' Shall a progress bar be shown?
#'
#' @return
#' A dataframe with two columns: \cr
#' doc_id: The document name of the text file
#' text:   The text of the text file
#'
#' @export
read_txt <- function(path, pb = FALSE) {
  if (pb == TRUE) {pboptions(type = "timer", char = "=", txt.width = 90)}
  else {pboptions(type = "none")}

  tibble::tibble(
    doc_id = stringi::stri_replace_last_fixed(basename(path), ".txt", ""),
    text   = unlist(pbapply::pblapply(path, readr::read_file)))
}

#' Read CSV Files
#'
#' @description
#' This function reads either a single of multiple csv files into a dataframe.
#'
#' @param path
#' The path to the textfiles (get with list.files())
#' @param pb
#' Shall a progress bar be shown?#'
#' @param delim
#' Delimiter, (mostly ';' or ',')
#' @param col_types
#' See ?readr::coltypes
#'
#' @return
#' A dataframe with the columns of the csv file plus an additional identifier column: \cr
#' doc_id: The document name of the csv file
#'
#' @export
read_csv <- function(path, pb = FALSE, delim = ";", col.types = "guess") {

  if (col.types == "guess") col.types <- readr::cols(.default = readr::col_guess())

  if (pb == TRUE) {pboptions(type = "timer", char = "=", txt.width = 90)}
  else {pboptions(type = "none")}

  df <- pblapply(path, function(x) {
    readr::read_delim(file = x, delim = delim, progress = F, col_types = col.types)})
  names(df) <- stringi::stri_replace_last_fixed(basename(path), ".csv", "")
  df <- dplyr::bind_rows(df, .id = "doc_id")
}
