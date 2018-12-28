#' Occurance Count of Substrings
#'
#' @param string
#' A character string with elements that are substrings of other elements
#' @param names
#' Shall the matrix have row/colmun names?
#' @param tri.lower
#' Shall only the lower triangle have non-zero values?
#'
#' @return
#' A squared matrix with row/column number equal the length of the input string
#' @export
#'
#' @examples
#' library(tpfuns)
#' a <- c("property plant and equipment", "plant and equipment", "plant")
#' fop_string_mat_count(a)
#' fop_string_mat_count(a, TRUE)
#' fop_string_mat_count(a, TRUE, FALSE)
#'
fop_string_mat_count <- function(string, names = FALSE, tri.lower = TRUE) {
  mat <- t(vapply(
    X = string,
    FUN = function(x)
      stringi::stri_count_regex(string, paste0("\\b", x, "\\b")),
    FUN.VALUE = numeric(length(string)),
    USE.NAMES = FALSE
  ))

  if (isTRUE(names)) {
    rownames(mat) <- string
    colnames(mat) <- string
  }

  if (isTRUE(tri.lower))
    mat[upper.tri(mat, diag = TRUE)] <- 0

  return(mat)
}

#' Adjust frequency for substring occurances
#'
#' @param df
#' The dataframe (output from the unadjusted frequency count)
#' @param group.ind
#' The groups in which each adjustemt should take place (e.g. per document and sentence).
#' This must be an integer vector with the same length as the input table
#' @param term.col
#' The column that contains the terms
#' @param count.col
#' The column that contains teh unadjusted frequencies for the term column
#'
#' @return
#' A numeric vector (adjusted frequencies)
#'
#' @export
#'
#' @examples
#' library(tpfuns)
#' table <- tibble::tibble(
#' doc_id = c(1, 1, 2, 2),
#' term = c("property plant and equipment", "property", "fixed asset", "asset"),
#' count = c(10, 12, 20, 51)
#' )
#'
#' table$adjust <- fop_freq_adjust(table, dplyr::group_indices(table, doc_id), "term", "count")
#' table
fop_freq_adjust <- function(df, group.ind, term.col, count.col) {
  # split terms and frequencies into groups
  list <- split(df[, c(term.col, count.col)], group.ind)

  unlist(lapply(1:length(list), function(x) {
    # get frequencies for each individual group
    n <- list[[x]][[count.col]]
    # get length of the string
    len <- length(n)
    # get the substring count matrix for each group
    mat <- fop_string_mat_count(list[[x]][[term.col]])
    # recursively adjust the frequencies
    adj.mat <-
      vapply(1:nrow(mat), function(i) {
        n <<- n + (n[i] * -mat[, i])
      }, numeric(len))
    # only take the last vector
    adj <- adj.mat[, len]
  }))
}
