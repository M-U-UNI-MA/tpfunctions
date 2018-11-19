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
unique_pairs <- function(vec) {

  `%>%` <- magrittr::`%>%`
  pairs <- matrix(1, length(vec), length(vec), dimnames = list(vec, vec))
  pairs[lower.tri(pairs, diag = TRUE)] <- NA
  pairs <- tibble::as_tibble(pairs) %>%
    dplyr::mutate(name_1 = colnames(.)) %>%
    tidyr::gather(name_2, value, -name_1) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::select(-value)
}


#' COsine Similarity between two Strings
#'
#' @param a
#' A String (or a vector of strings)
#' @param b
#' A String (or a vector of strings)
#'
#' @return
#' The cosine similarity (either a single number or a vector of numbers)
#' @export
cos_string_pair <- function(a, b) {
  `%>%` <- magrittr::`%>%`

  cos <- unlist(lapply(1:length(a), function(x) {

  table.a <- tibble::tibble(a = a[x]) %>%
    tidytext::unnest_tokens(word_a, a) %>%
    dplyr::anti_join(tidytext::stop_words, by = c("word_a" = "word")) %>%
    dplyr::count(word_a)

  table.b <- tibble::tibble(b = b[x]) %>%
    tidytext::unnest_tokens(word_b, b) %>%
    dplyr::anti_join(tidytext::stop_words, by = c("word_b" = "word")) %>%
    dplyr::count(word_b)

  table <- dplyr::full_join(table.a, table.b, by = c("word_a" = "word_b")) %>%
    dplyr::mutate(n.x = tidyr::replace_na(n.x, 0)) %>%
    dplyr::mutate(n.y = tidyr::replace_na(n.y, 0))

  cos.0 <- coop::cosine(table$n.x, table$n.y)
  return(cos.0)
  }))
  return(cos)
}
