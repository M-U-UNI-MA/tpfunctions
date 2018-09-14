#' Read multiple csv files into a dataframe
#'
#' @param path path to the csv files
#' @param recursive shall subfolders be included?
#' @param delim Delimiter, (mostly ';' or ',')
#' @param col_types see readr::coltypes
#'
#' @return A dataframe
#' @export
read_mult_csv <- function(path,
                          recursive = FALSE,
                          delim = ";",
                          col.types = cols(.default = col_guess())) {

  `%>%` <- magrittr::`%>%`

  v.files <- list.files(
    path = path,
    pattern = "\\.csv$",
    all.files = FALSE,
    full.names = TRUE,
    recursive = recursive
  )

  df <- lapply(v.files, function(x) {
    readr::read_delim(file = x, delim = ";", progress = FALSE, col_types = col.types)
  }) %>% dplyr::bind_rows()

  return(df)
}
