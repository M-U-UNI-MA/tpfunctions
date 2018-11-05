#' Convert British English to Amrican English
#'
#' @param string
#' The input string (the string must be lemmatized)
#' @param stand.table
#' A table with a pair of words in British English and American English \cr
#' Note: If no table is specified the default table will be used \cr
#' @param nthreads
#'
#' @return
#' A string with only American English spelling
#' @export
americanize_exact <- function(string, stand.table = NULL, nthreads = 1) {
  `%>%` <- magrittr::`%>%`
  ae.be <- tpfuns::table_stand_ae_be %>%
    dplyr::mutate(UK = textstem::lemmatize_words(tpfuns::stand_chars(stringi::stri_enc_toutf8(UK)))) %>%
    dplyr::mutate(US = textstem::lemmatize_words(tpfuns::stand_chars(stringi::stri_enc_toutf8(US)))) %>%
    dplyr::distinct(UK, .keep_all = TRUE) %>%
    dplyr::mutate(regex = paste0("\\b", UK, "\\b"))

  uk <- ae.be$UK
  us <- ae.be$US



  if (nthreads == 1) {
    string <- stringi::stri_replace_all_regex(string, uk, us, vectorize_all = FALSE)
    string <- stringi::stri_trim_both(string)
  } else {
    if (nthreads == 0) {
      nthreads <- parallel::detectCores() - 1
    }
    split <- split(string, sort(1:length(string)%%nthreads))

    cl <- parallel::makeCluster(3)
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
      stringi::stri_replace_all_regex(split[[i]], uk, us, vectorize_all = F)
    }

    parallel::stopCluster(cl)

  }

  return(string)
}
