# EXTRACT PAIRS ==========================================================================
#' Extract Unique Pairs from Matrix
#'
#' @param mat
#' A named matrix
#'
#' @return
#' A dataframe with unique pairs
#' @export
mes_uni_mat <- function(mat) {
  `%>%` <- magrittr::`%>%`
  if (is.null(dimnames(mat)[[1]]) | is.null(dimnames(mat)[[2]]))
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
#' @param obj Shall a matrix of dataframe be returned
#' @param normalize none or tf-idf
#' @param freq.table A Table
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
mes_cos <- function(freq.table, obj = c("normal", "matrix"), normalize = c("tf-idf", "none")) {
  `%>%` <- magrittr::`%>%`
  colnames(freq.table) <- c("doc_id", "term", "freq")

  if (normalize == "tf-idf") {
    table <- freq.table %>%
      tidytext::bind_tf_idf(term, doc_id, freq) %>%
      dplyr::select(-tf, -idf, -freq) %>%
      tidyr::spread(doc_id, tf_idf, fill = 0)
    mat <- as.matrix(table[, -1])
  }

  if (normalize == "none") {
    table <- tidyr::spread(freq.table, doc_id, freq, fill = 0)
    mat <- as.matrix(table[, -1])
  }


  cos <- coop::cosine(mat)
  if (all(obj == "matrix")) {
    return(cos)
  } else {
    cos <- mes_uni_mat(cos)
    return(cos)
  }
}


# PROFILE BASED STANDARDIZATION ==========================================================
#' Profile Based Standardization
#'
#' @param obj Matrix or dataframe
#' @param cons shall the output be consolidated (sum up concepts)
#' @param weigh shall the output be weighted (equally or by frequency)
#' @param freq.cap apply a minimum frequency
#' @param freq.table A Table
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
mes_pbs <-
  function(freq.table,
           obj = c("normal", "matrix"),
           cons = FALSE,
           weigh = c("equal", "freq"),
           freq.cap = NULL) {

    `%>%` <- magrittr::`%>%`
    colnames(freq.table) <- c("doc_id", "con_id", "term", "freq")

    # get concept frequencies ------------------------------------------------------------
    con.freq <- freq.table %>%
      dplyr::group_by(con_id) %>%
      dplyr::summarise(freq = sum(freq)) %>%
      dplyr::ungroup()

    # check ------------------------------------------------------------------------------
    if (!is.null(freq.cap) & all(con.freq$freq < freq.cap))
      stop("freqency cap too high")

    # get missing frequencies ------------------------------------------------------------
    miss <- freq.table %>% dplyr::distinct(con_id, term) %>%
      tidyr::crossing(doc_id = freq.table$doc_id) %>%
      dplyr::anti_join(freq.table, by = c("con_id", "term", "doc_id")) %>%
      dplyr::mutate(freq = 0)

    # calculate relative freq and change to long format ----------------------------------
    table <- dplyr::bind_rows(freq.table, miss) %>%
      dplyr::group_by(doc_id, con_id) %>%
      dplyr::mutate(rel = freq / sum(freq)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-freq) %>%
      dplyr::filter(!is.nan(rel)) %>%
      tidyr::spread(doc_id, rel) %>%
      dplyr::arrange(con_id)

    # split concepts ---------------------------------------------------------------------
    con.list <- split(table, dplyr::group_indices(table, con_id))

    # calculate profile based standardization --------------------------------------------
    pbs <- lapply(1:length(con.list), function(a) {
      man <- lapply(1:nrow(con.list[[a]]), function(x) {
        as.matrix(dist(t(as.matrix(con.list[[a]][x, -(1:2)])), method = "manhattan"))
      })
      man <- 1 - purrr::reduce(man, `+`) / 2
      man[is.na(man)] <- 0
      return(man)
    })
    names(pbs) <- unique(table$con_id)

    # get weighings and perform consolidation --------------------------------------------
    if (isTRUE(cons)) {
      if (weigh == "equal") {
        w <- rep(1 / length(unique(table$con_id)), length(unique(table$con_id)))
        if (!is.null(freq.cap)) {
          drop <- con.freq[which(con.freq$freq < freq.cap),]$con_id
          if (!purrr::is_empty(drop)) {
            pbs <- pbs[-drop]
            w <- rep(1 / length(w[-drop]), length(w[-drop]))
          }
        }
      } else {
        w <- dplyr::mutate(con.freq, rel = freq / sum(freq)) %>% dplyr::pull()
      }
      pbs <- lapply(1:length(w), function(x) pbs[[x]] * w[x])
      pbs <- purrr::reduce(pbs, `+`)
    }

    # return output ----------------------------------------------------------------------
    if (all(obj == "matrix")) {
      return(pbs)
    } else {
      if (isTRUE(cons)) {
        pbs <- tpfuns::mes_uni_mat(pbs)
      } else {
        pbs <- lapply(pbs, tpfuns::mes_uni_mat) %>%
          dplyr::bind_rows(., .id = "con_id")
      }
      pbs <- dplyr::rename(pbs, pbs = mat)
      return(pbs)
    }
  }
