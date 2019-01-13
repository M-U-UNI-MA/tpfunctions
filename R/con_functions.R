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
#' A list containing (a) the adjusted dataframe, (b) the removed concepts and c() the
#' adjustment matrix
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
  mat <- m
  dimnames(mat) <- list(names(l), names(l))
  m[lower.tri(m, diag = TRUE)] <- 0
  l[which(m == 1, arr.ind = TRUE)[, 1]] <- NULL

  if (length(l) == 0) {
    df.1 <- df[0,]
    df.2 <- df
  } else {
    df.1 <- df[which(df[[col.conid]] %in% names(l)),]
    df.2 <- df[-which(df[[col.conid]] %in% names(l)),]
  }

  return(list(adj = df.1, rem = df.2, mat = mat))
}

# REMOVE SUBSET CONCEPTS =================================================================
#' Remove Subset Concepts
#'
#' @param df
#' A dataframe containing terms and corresponding concept ids
#' @param col.term
#' The name of the column containing the terms as character string
#' @param col.conid
#' The name of the column containing the concept ids as character string
#'
#' @return
#' A list containing (a) the dataframe without removed concepts (b) a dataframe iwth the
#' removed concepts and (c) the adjusment matrix
#' @export
#'
#' @examples
#' library(tpfuns)
#' test <- tibble::tibble(
#' con_id = c( 1,   1,   1,   2,   2,   3,   3,   4,   4 ,  5,   5,   5,   6,   6 ),
#' term   = c("a", "c", "b", "a", "b", "d", "e", "e", "f", "e", "f", "g", "b", "a"))
#'
#' con_subset(test, "term", "con_id")
con_subset <- function(df, col.term, col.conid) {
  l <- split(df[[col.term]], dplyr::group_indices_(df, col.conid))
  names(l) <- unique(sort(df[[col.conid]]))

  # check if a concept is a subset of another concept ------------------------------------
  m <- sapply(1:length(l), function(x) {
    sapply(1:length(l), function(y) {
      as.numeric(all(l[[y]] %in% l[[x]]))
    })
  })
  mat <- m
  dimnames(mat) <- list(names(l), names(l))

  diag(m) <- 0
  l[which(m == 1, arr.ind = TRUE)[, 1]] <- NULL

  if (length(l) == 0) {
    df.1 <- df[0,]
    df.2 <- df
  } else {
    df.1 <- df[which(df[[col.conid]] %in% names(l)),]
    df.2 <- df[-which(df[[col.conid]] %in% names(l)),]
  }


  return(list(adj = df.1, rem = df.2, mat = mat))
}

# DISAMBIGUATE CONCEPTS 1 ================================================================
#' Disambiguate Concepts
#'
#' #' @description
#' This function checks whether concepts share terms. If two or more concepts share terms,
#' the whole concepts are deleted!\cr
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
#' A list containing (a) the dataframe without ambigious concepts (b) a dataframe with the
#' removed concepts and (c) the adjusment matrix
#' @export
#'
#' @examples
#' library(tpfuns)
#' test <- tibble::tibble(
#' con_id = c( 1,   1,   1,   2,   2,   3,   3,   4,   4 ,  5,   5,   5,   6,   6 ),
#' term   = c("a", "c", "b", "a", "b", "d", "e", "e", "f", "e", "f", "g", "b", "a"))
#'
#' con_disambig1(test, "term", "con_id")
#'
con_disambig1 <- function(df, col.term, col.conid) {
  l <- split(df[[col.term]], dplyr::group_indices_(df, col.conid))
  names(l) <- unique(sort(df[[col.conid]]))

  # check if concepts overlap ------------------------------------------------------------
  m <- sapply(1:length(l), function(x) {
    sapply(1:length(l), function(y) {
      as.numeric(any(l[[y]] %in% l[[x]]))
    })
  })
  mat <- m
  dimnames(mat) <- list(names(l), names(l))
  diag(m) <- 0
  l[which(m == 1, arr.ind = TRUE)[, 1]] <- NULL

  if (length(l) == 0) {
    df.1 <- df[0,]
    df.2 <- df
  } else {
    df.1 <- df[which(df[[col.conid]] %in% names(l)),]
    df.2 <- df[-which(df[[col.conid]] %in% names(l)),]
  }


  return(list(adj = df.1, rem = df.2, mat = mat))
}

# DISAMBIGUATE CONCEPTS 2 ================================================================
#' Disambiguate Concepts 2
#'
#' #' @description
#' This function checks whether concepts share terms and keeps only the common terms.\cr
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
#' A list containing (a) the dataframe without ambigious concepts and (b) a dataframe with the
#' removed terms
#' @export
#'
#' @examples
#' library(tpfuns)
#' test <- tibble::tibble(
#' con_id = c( 1,   1,   1,   2,   2,   3,   3,   4,   4 ,  5,   5,   5,   6,   6 ),
#' term   = c("a", "c", "b", "a", "b", "d", "e", "e", "f", "e", "f", "g", "b", "a"))
#'
#' con_disambig2(test, "term", "con_id")
#'
con_disambig2 <- function(df, col.term, col.conid) {
  l <- split(df[[col.term]], dplyr::group_indices_(df, col.conid))
  names(l) <- unique(sort(df[[col.conid]]))

  # check if concepts overlap ------------------------------------------------------------
  m <- sapply(1:length(l), function(x) {
    sapply(1:length(l), function(y) {
      as.numeric(any(l[[y]] %in% l[[x]]))
    })
  })
  m[lower.tri(m, diag = TRUE)] <- 0
  if (all(m == 0)) {
    df.1 <- df
    df.2 <- df[0, ]
  } else {

    x <- which(m == 1, arr.ind = TRUE)
    y <- cbind(c(x[, 1], x[, 2]), id = rep(1:nrow(x), 2))
    lapply(1:nrow(y), function(i)
      y[, 2][which(y[, 2] %in% y[, 2][which(y[, 1] %in% y[i, 1])])] <<- y[i, 2]
    )
    y <- y[!duplicated(y[, 1]), ]
    y <- split(y[, 1], y[, 2])
    z <- lapply(1:length(y), function(i) Reduce(intersect, l[y[[i]]]))
    y <- relist(as.integer(names(l)[unlist(y)]), y)

    check <- sapply(z, length) == 0

    if (any(check)) {
      rem <- unlist(y[which(check)])
      y[which(check)] <- NULL
      z[which(check)] <- NULL
      df.rem <- df[which(df[[col.conid]] %in% rem), ]
      df     <- df[-which(df[[col.conid]] %in% rem), ]
    }

    df.1 <- df[-which(df[[col.conid]] %in% unlist(y) & !df[[col.term]] %in% unlist(z)), ]
    df.2 <- df[which(df[[col.conid]] %in% unlist(y) & !df[[col.term]] %in% unlist(z)), ]

  }

  return(list(adj = df.1, rem = df.2))
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
#' A dataframe with reassigned concept ids
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
  terms   <- df[[col.term]]
  con.ids <- df[[col.conid]]
  lapply(1:length(terms), function(x)
    con.ids[which(con.ids %in% con.ids[which(terms %in% terms[x])])] <<- con.ids[x]
  )
  df[col.conid] <- con.ids
  return(df)

}


