# Read Text Files ========================================================================
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
#' doc_id: The document name of the text file\cr
#' text:   The text of the text file
#'
#' @export
#'
#' @examples
#' # get path to test files
#' file.path <- list.files(system.file("exdata/txt_files/", package = "tpfuns"), "txt$", F, T)
#'
#' # read files in dataframe (without progressbar)
#' files.1 <- tpfuns::read_txt(file.path)
#'
#' # read files in dataframe (with progressbar)
#' files.2 <- tpfuns::read_txt(file.path, TRUE)
read_txt <- function(path, pb = FALSE) {
  if (pb == TRUE) {
  tibble::tibble(
    doc_id = stringi::stri_replace_last_fixed(basename(path), ".txt", ""),
    text   = unlist(pbapply::pblapply(path, readr::read_file)))
  } else {
    tibble::tibble(
      doc_id = stringi::stri_replace_last_fixed(basename(path), ".txt", ""),
      text   = unlist(lapply(path, readr::read_file)))
  }
}

# Read Text Files ========================================================================
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

  if (pb == TRUE) {
    df <- pbapply::pblapply(path, function(x) {
      readr::read_delim(file = x, delim = delim, progress = pb, col_types = col.types)})
  } else {
    df <- lapply(path, function(x) {
      readr::read_delim(file = x, delim = delim, progress = pb, col_types = col.types)})
  }

  names(df) <- stringi::stri_replace_last_fixed(basename(path), ".csv", "")
  df <- dplyr::bind_rows(df, .id = "doc_id")

}

# Read Text Files ========================================================================
#' Function that saves files both as csv and rds
#'
#' @param x
#' A dataframe
#' @param path
#' The full path to the file (without file endings)
#' @return
#' two saved files
#' @export
write_csv_rds <- function(x, path, format = c("csv", "rds")) {

  if (!any(format %in% c("csv", "rds"))) stop("no valid file format")
  if ("csv" %in% format) readr::write_delim(x, paste0(path, ".csv"), ";", "")
  if ("rds" %in% format) readr::write_rds(x, paste0(path, ".rds"))

}
