#' Unique Combinations of all Elements within a Vector
#'
#' @description
#' Given a vector of elements this function calculates the unique combinations between all
#' element
#'
#' @param vec
#'
#' @return
#' A Dataframe with (n^2-n)/2 observations
#' @export
#'
#' @examples
#' # unique pairs of 3 companies, a dataframe with (3^2-3)/2 = 3 observations is returned
#' unique_pairs(c("comp_1", "comp_2", "comp_3"))
#'
unique_pairs <- function(vec) {

  `%>%` <- magrittr::`%>%`
  pairs <- matrix(1, length(vec), length(vec), dimnames = list(vec, vec))
  pairs[lower.tri(pairs, diag = TRUE)] <- NA
  pairs <- tibble::as_tibble(pairs) %>%
    dplyr::mutate(name_1 = colnames(.)) %>%
    tidyr::gather(name_2, value, -name_1) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::select(-value)

  return(pairs)

}
