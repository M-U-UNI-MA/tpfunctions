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

#' TBD
#'
#' @param input TBD
#' @param ident TBD
#' @param tokenize TBD
#' @param add.abbr TBD
#'
#' @return TBD
#' @export
fop_tok <- function(input = NULL, ident = NULL,
                    tokenize = c("page", "paragraph", "sentence", "word"),
                    add.abbr = NULL) {

  # check input format -------------------------------------------------------------------
  if (is.null(input)) stop("input must not be NULL")
  if (is.null(ident)) stop("identfier must not be NULL")
  if (!is.character(input)) stop("input must be a character string")
  if (any(duplicated(ident))) stop("identifier must be unique")
  if (!is.null(ident) & length(input) != length(ident)) stop("identifier has not the same length as input")

  # prepare abbreviation list ------------------------------------------------------------
  `%>%` <- magrittr::`%>%`
  abbr <- tpfuns::table_sent_abbr$abbr
  if (is.character(add.abbr)) abbr <- c(abbr, add.abbr)
  abbr <- stringi::stri_trans_tolower(abbr)
  abbr <- stringi::stri_flatten(paste0("\\b", tpfuns::escape_regex(unique(abbr)), "$"), "|")

  # prepare text -------------------------------------------------------------------------
  table.text <- tibble::tibble(
    doc_id = ident, pag_id = ident, par_id = ident, sen_id = ident, tok_id = ident, token = input
  )

  if ("page" %in% tokenize) {
    table.text <- table.text %>%
      dplyr::group_by(doc_id, pag_id, par_id, sen_id, tok_id) %>%
      tidytext::unnest_tokens(
        output = token, input = token, token = "regex", pattern  = "\f", to_lower = FALSE
      ) %>% dplyr::ungroup() %>%
      dplyr::group_by(doc_id, par_id, sen_id, tok_id) %>%
      dplyr::mutate(pag_id = dplyr::row_number()) %>%
      dplyr::ungroup()
  }

  if ("paragraph" %in% tokenize) {
    table.text <- table.text %>%
      dplyr::group_by(doc_id, pag_id, par_id, sen_id, tok_id) %>%
      tidytext::unnest_tokens(
        output = token, input = token, token = "paragraphs", to_lower = FALSE
      ) %>% dplyr::ungroup() %>%
      dplyr::group_by(doc_id, pag_id, sen_id, tok_id) %>%
      dplyr::mutate(par_id = dplyr::row_number()) %>%
      dplyr::ungroup()
  }

  if ("sentence" %in% tokenize) {
    table.text <- table.text %>%
      dplyr::group_by(doc_id, pag_id, par_id, sen_id, tok_id) %>%
      tidytext::unnest_tokens(
        output = token, input = token, token = "sentences", to_lower = FALSE, strip_punct = FALSE
      ) %>% dplyr::ungroup() %>%
      dplyr::group_by(doc_id, pag_id, par_id, tok_id) %>%
      dplyr::mutate(sen_id = dplyr::row_number()) %>%
      dplyr::ungroup()

    detect.abbr <- stringi::stri_detect_regex(table.text$token, abbr, case_insensitive = TRUE)
    # in case the last entry is an abbreviation set it to FALSE
    detect.abbr[length(detect.abbr)] <- FALSE

    adj.0 <- which(detect.abbr)
    adj.1 <- adj.0 + 1
    adj.0 <- table.text$sen_id[adj.0]
    adj.0 <- split(adj.0, cumsum(c(1, diff(adj.0) != 1)))
    adj.0 <- unlist(lapply(1:length(adj.0), function(x) rep.int(adj.0[[x]][1], length(adj.0[[x]]))))
    table.text$sen_id[adj.1] <- adj.0

    table.text <- table.text %>%
      dplyr::group_by(doc_id, pag_id, par_id, sen_id, tok_id) %>%
      dplyr::summarise(token = stringi::stri_flatten(token, " ")) %>%
      dplyr::ungroup()
  }

  if ("word" %in% tokenize) {
    table.text <- table.text %>%
      dplyr::mutate(token = tpfuns::top_punct(string = token, punct.rem = "partial")) %>%
      dplyr::group_by(doc_id, pag_id, par_id, sen_id, tok_id) %>%
      tidytext::unnest_tokens(
        output = token, input = token, token = stringr::str_split, pattern = " ", to_lower = FALSE
      ) %>% dplyr::ungroup() %>%
      dplyr::group_by(doc_id, pag_id, par_id, sen_id) %>%
      dplyr::mutate(tok_id = dplyr::row_number()) %>%
      dplyr::ungroup()
  }

  cols.return <- c("pag_id", "par_id", "sen_id", "tok_id")
  cols.return <- cols.return[which(c("page", "paragraph", "sentence", "word") %in% tokenize)]
  table.text <- table.text[, c("doc_id", cols.return, "token")]

  return(table.text)
}
