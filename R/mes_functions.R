# EXTRACT PAIRS ==========================================================================
#' Extract Unique Pairs from Matrix
#'
#' @param mat
#' A named matrix
#'
#' @return
#' A dataframe with unique pairs
#' @export
#'
#' @examples
mes_uni_mat <- function(mat) {
  `%>%` <- magrittr::`%>%`
  if (is.null(dimnames(mat)[[1]]) | is.null(dimnames(m)[[2]]))
    stop("matrix must have column-names and row-names")
  mat[lower.tri(mat, diag = TRUE)] <- NA
  mat <- tibble::as_tibble(mat) %>%
    dplyr::mutate(n1 = colnames(.)) %>%
    tidyr::gather(n2, mat, -n1) %>%
    dplyr::filter(!is.na(mat))
  return(mat)
}

# COSINE SIMILARITY ======================================================================
#' Cosine Similarity
#'
#' @param freq.table A Table
#' @param return Table or Matrix
#'
#' @return
#' Table or Matrix
#' @export
#'
#' @examples
#' library(tpfuns)
#' freq.table <- tibble::tibble(
#' doc_id = c(1,1,1,1,1,2,2,2,2,3,3,3,3),
#' term   = c("a", "b", "c", "d", "e", "a", "b", "f", "g", "a", "n", "e", "g"),
#' freq   = c(21,12,58,32,14,21,14,66,14,12,85,100,12))
#' mes_cos(freq.table)
mes_cos <- function(freq.table, return = c("normal", "matrix")) {
  `%>%` <- magrittr::`%>%`
  colnames(freq.table) <- c("doc_id", "term", "freq")
  table <- tidyr::spread(freq.table, doc_id, freq, fill = 0)

  cos <- coop::cosine(as.matrix(table[, -1]))
  if (all(return == "matrix")) {
    return(cos)
  } else {
    cos <- mes_uni_mat(cos)
    return(cos)
  }
}


# PROFILE BASED STANDARDIZATION ==========================================================
#' Profile Based Standardization
#'
#' @param freq.table A Table
#' @param return Table or Matrix
#'
#' @return
#' Table or Matrix
#' @export
#'
#' @examples
#' library(tpfuns)
#' freq.table <- tibble::tibble(
#' doc_id = c(1,1,1,1,1,2,2,2,2,3,3,3,3),
#' con_id = c(1,1,2,2,2,1,1,3,3,1,3,2,3),
#' term   = c("a", "b", "c", "d", "e", "a", "b", "f", "g", "a", "f", "e", "g"),
#' freq   = c(21,12,58,32,14,21,14,66,14,12,85,100,12))
#'
#' mes_pbs(freq.table)
mes_pbs <- function(freq.table, return = c("normal", "matrix")) {
  `%>%` <- magrittr::`%>%`
  colnames(freq.table) <- c("doc_id", "con_id", "term", "freq")

  con.term <- freq.table %>% dplyr::distinct(con_id, term) %>%
    tidyr::crossing(doc_id = freq.table$doc_id) %>%
    dplyr::anti_join(freq.table, by = c("con_id", "term", "doc_id")) %>%
    dplyr::mutate(freq = 0)

  table <- dplyr::bind_rows(freq.table, con.term) %>%
    dplyr::group_by(doc_id, con_id) %>%
    dplyr::mutate(rel = freq / sum(freq)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-freq) %>%
    dplyr::filter(!is.nan(rel)) %>%
    tidyr::spread(doc_id, rel) %>%
    dplyr::arrange(con_id)

  con.list <- split(table, dplyr::group_indices(table, con_id))
  con.list <- lapply(con.list, function(x) {
    x[, !apply(x, 2, function(y)
      all(is.na(y)))]
  })

  dcb.con <- lapply(1:length(con.list), function(a) {
    man <- lapply(1:nrow(con.list[[a]]), function(x) {
      as.matrix(dist(t(as.matrix(con.list[[a]][x, -(1:2)])), method = "manhattan"))
    })
    man <- 1 - purrr::reduce(man, `+`) / 2
  })

  if (all(return == "matrix")) {
    names(dcb.con) <- unique(table$con_id)
    return(dcb.con)
  } else {
    pbs <- lapply(dcb.con, function(x) mes_uni_mat(x))
    names(pbs) <- unique(table$con_id)
    pbs <- dplyr::bind_rows(pbs, .id = "con_id")
    return(pbs)
  }
}


