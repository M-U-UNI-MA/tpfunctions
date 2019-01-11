# DEDUPLICATE CONCEPTS ===================================================================
#' Deduplicate Concepts
#'
#' @description
#' This function checks if two concepts in the dataframe contain exactly the same terms
#' and keeps only one of the concepts
#'
#' @param df
#' A dataframe containing terms and corresponding concept ids
#' @param col.term
#' The name of the column containing the terms as character string
#' @param col.conid
#' The name of the column containing the concept ids as character string
#'
#' @return
#' A dataframe with deduplicated concepts
#' @export
#'
#' @examples
#' library(tpfuns)
#' test <- tibble::tibble(
#' con_id = c( 1,   1,   1,   2,   2,   3,   3,   4,   4 ,  5,   5,   5,   6,   6 ),
#' term   = c("a", "c", "b", "a", "b", "d", "e", "e", "f", "e", "f", "g", "b", "a"))
#'
#' con_dedup(test, "term", "con_id")
con_dedup <- function(df, col.term, col.conid) {
  # build list per concept ---------------------------------------------------------------
  l <- split(df[[col.term]], dplyr::group_indices_(df, col.conid))
  names(l) <- unique(sort(df[[col.conid]]))
  l <- lapply(l, sort)

  # check if two concepts are exactly the same -------------------------------------------
  m <- sapply(1:length(l), function(x) {
    sapply(1:length(l), function(y) {
      as.numeric(identical(l[[y]], l[[x]]))
    })
  })
  m[lower.tri(m, diag = TRUE)] <- 0
  l[which(m == 1, arr.ind = TRUE)[, 1]] <- NULL
  df <- df[which(df[[col.conid]] %in% names(l)),]
  return(df)
}


# DISAMBIGUATE CONCEPTS ==================================================================
#' Disambiguate Concepts
#'
#' #' @description
#' This function checks two things:\cr
#' 1.: Whether a concept is a subset of another concept. In this case the short concept is
#' deleted while the longer concept is preserved\cr
#' 2.: Whether concepts share terms without being subsets of another. In this case all
#' concepts are deleted\cr#'
#' IMPORTANT NOTE: Concepts should be deduplicated before disambiguated!!
#'
#'
#' @param df
#' A dataframe containing terms and corresponding concept ids
#' @param col.term
#' The name of the column containing the terms as character string
#' @param col.conid
#' The name of the column containing the concept ids as character string
#'
#' @return
#' A dataframe with disambiguated concepts
#' @export
#'
#' @examples
#' library(tpfuns)
#' test <- tibble::tibble(
#' con_id = c( 1,   1,   1,   2,   2,   3,   3,   4,   4 ,  5,   5,   5,   6,   6 ),
#' term   = c("a", "c", "b", "a", "b", "d", "e", "e", "f", "e", "f", "g", "b", "a"))
#'
#' con_disambig(test, "term", "con_id")
con_disambig <- function(df, col.term, col.conid) {
  l <- split(df[[col.term]], dplyr::group_indices_(df, col.conid))
  names(l) <- unique(sort(df[[col.conid]]))

  # check if a concept is a subset of another concept ------------------------------------
  m <- sapply(1:length(l), function(x) {
    sapply(1:length(l), function(y) {
      as.numeric(all(l[[y]] %in% l[[x]]))
    })
  })
  diag(m) <- 0
  l[which(m == 1, arr.ind = TRUE)[, 1]] <- NULL

  # check if concepts overlap ------------------------------------------------------------
  m <- sapply(1:length(l), function(x) {
    sapply(1:length(l), function(y) {
      as.numeric(any(l[[y]] %in% l[[x]]))
    })
  })
  diag(m) <- 0
  l[which(m == 1, arr.ind = TRUE)[, 1]] <- NULL
  df <- df[which(df[[col.conid]] %in% names(l)), ]
  return(df)
}

# MERGE CONCEPTS =========================================================================
#' Merge Concepts
#'
#' @description
#' this function reassigns concept ids if different concepts contain same terms
#'
#' @param df
#' A dataframe containing terms and corresponding concept ids
#' @param col.term
#' The name of the column containing the terms as character string
#' @param col.conid
#' The name of the column containing the concept ids as character string
#'
#' @return
#' A dataframe with merged concepts (reassigned concept ids)
#' @export
#'
#' @examples
#' library(tpfuns)
#' test <- tibble::tibble(
#' con_id = c( 1,   1,   1,   2,   2,   3,   3,   4,   4 ,  5,   5,   5,   6,   6 ),
#' term   = c("a", "c", "b", "a", "b", "d", "e", "e", "f", "e", "f", "g", "b", "a"))
#'
#' con_merge(test, "term", "con_id")
con_merge <- function(df, col.term, col.conid) {
  # build list per concept ---------------------------------------------------------------
  l <- split(df[[col.term]], dplyr::group_indices_(df, col.conid))
  names(l) <- unique(sort(df[[col.conid]]))

  # check if concepts overlap ------------------------------------------------------------
  m <- sapply(1:length(l), function(x) {
    sapply(1:length(l), function(y) {
      as.numeric(any(l[[y]] %in% l[[x]]))
    })
  })
  m[lower.tri(m, diag = TRUE)] <- 0
  y <- which(m == 1, arr.ind = TRUE)

  if(!nrow(y) == 0) {
    # update dataframe -------------------------------------------------------------------
    lapply(1:nrow(y), function(x) df[which(df[col.conid] == y[x,1]), col.conid] <<- y[x,2])
  }

  return(df)
}



