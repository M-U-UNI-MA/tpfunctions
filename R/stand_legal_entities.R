#' Standardize Legal Entities (Vectorized version)
#'
#' @param string The strings containing legal entities
#' @param le.table A table with legal entities, if NULL the default table will be used
#' @param standardized Is the input already standardized? If not the standardization function
#' stand_chars is used to standardize the input string
#' @param nthreads
#' Number of threads for paralell processing (as integer) \cr
#' nthreads = 1: No parallel processing (default) \cr
#' nthreads = 0: All but one cores will be used (parallel::detectCores() - 1)
#'
#' @return A string with legal entities removed
#' @export
#' @examples
#' a <- c("BASF SE", "BASF AG")
#'stand_le_2(a)
#'
stand_le_2 <- function(string, le.table = NULL, standardized = FALSE, nthreads = 1) {
  `%>%` <- magrittr::`%>%`
  # prepare the legal entity table

  if (is.null(le.table)) le.table <- tpfuns::legal_entities

  le <- le.table  %>%
    dplyr::mutate(le_name = tpfuns::stand_chars(le_name)) %>%
    dplyr::distinct(occurance, le_name, .keep_all = TRUE) %>%
    dplyr::mutate(ngram = stringi::stri_count_fixed(le_name, " ") + 1) %>%
    dplyr::arrange(type, dplyr::desc(ngram)) %>%
    dplyr::mutate(regex = stringi::stri_replace_all_fixed(le_name, "(", "\\(?")) %>%
    dplyr::mutate(regex = stringi::stri_replace_all_fixed(regex, ")", "\\)?"))

  le.abbr <- le %>%
    dplyr::filter(type == "abbr") %>%
    dplyr::mutate(regex = stringi::stri_replace_all_regex(regex, " ", "")) %>%
    dplyr::mutate(regex = stringi::stri_replace_all_regex(regex, "([a-z])", "$1\\(\\\\s+\\)?")) %>%
    dplyr::mutate(regex = stringi::stri_replace_all_regex(regex, "\\(\\\\s\\+\\)\\?$", "")) %>%
    dplyr::mutate(regex = dplyr::if_else(occurance == "End",
                                         paste0("\\b", regex, "$"),
                                         paste0("^", regex, "\\b")))

  le.full <- le %>%
    dplyr::filter(type == "full") %>%
    dplyr::mutate(regex = dplyr::if_else(occurance == "End",
                                         paste0("\\b", regex, "$"),
                                         paste0("^", regex, "\\b")))

  le <- dplyr::bind_rows(le.abbr, le.full)
  le.regex   <- le$regex

  if (nthreads == 1) {

  if (standardized == FALSE) string <- tpfuns::stand_chars(string, nthreads = nthreads)
  string <- stringi::stri_replace_all_regex(string, le.regex, "", vectorize_all = F)
  string <- stringi::stri_replace_all_regex(string, "\\s+", " ")
  string <- stringi::stri_trim_both(string)
  } else {
    if (nthreads == 0) nthreads <- parallel::detectCores() - 1
    if (standardized == FALSE) string <- tpfuns::stand_chars(string, nthreads = nthreads)

    split <- split(string, sort(1:length(string)%%nthreads))

    cl <- parallel::makeCluster(nthreads)
    doSNOW::registerDoSNOW(cl)

    pb <- txtProgressBar(max = length(length(split)), style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)

    `%dopar%` <- foreach::`%dopar%`

    string <- foreach::foreach(
      i = 1:length(split),
      .packages = ("stringi"),
      .options.snow = opts,
      .combine = c
    ) %dopar% {
      split[[i]] <- stringi::stri_replace_all_regex(split[[i]], le.regex, "", vectorize_all = F)
      split[[i]] <- stringi::stri_replace_all_regex(split[[i]], "\\s+", " ")
      split[[i]] <- stringi::stri_trim_both(split[[i]])
    }

    parallel::stopCluster(cl)

  }


  return(string)
}
