#' Bind dataframes contained in list with document id
#'
#' @param list List that contains dataframes
#' @param names The names of the dataframes
#' @param id Name of the id column
#'
#' @return A dataframe
#' @export
bind_df_list <- function(list, names, id) {

  names(list) <- names
  x <- dplyr::bind_rows(list, .id = id)
  return(x)
}
