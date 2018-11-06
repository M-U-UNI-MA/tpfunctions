#' List files in a dataframe with id column
#'
#' @description A helper function that is build around list.files (base)
#'
#' @param path path to the files
#' @param file.type file ending (e.g. txt, pdf, csv, ...)
#' @param id column name containing the file name
#'
#' @return a dataframe with two columns
#'
#' @export
#'
list_files_tbl <- function(path, file.type, id) {
  `%>%` <- magrittr::`%>%`

  pattern <- paste0("\\.", file.type, "$")

  files <- dplyr::tibble(path = list.files(path, pattern, F, T)) %>%
    dplyr::mutate(id = stringr::str_replace_all(basename(path), pattern, "")) %>%
    dplyr::select(id, path)

  path.name <- paste0("path_", file.type)
  colnames(files) <- c(id, path.name)

  return(files)

}
