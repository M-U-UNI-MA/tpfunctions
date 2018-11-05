#' Standardizes Names from Accounting Data Provider (e.g. Datastream)
#'
#' @param string String with company names to be standardized
#' @param regex.table A table with with regexes for string replacement, if NULL default table is used
#' @param standardized Is the input already standardized? If not the standardization function
#' stand_chars is used to standardize the input string
#' @param nthreads
#' Number of threads for paralell processing (as integer) \cr
#' nthreads = 1: No parallel processing (default) \cr
#' nthreads = 0: All but one cores will be used (parallel::detectCores() - 1)
#'
#' @return A string with standardized names
#' @export
#' @examples
#'a <- "BASF SE dead 31/12/2005"
#'stand_db_names(a)
#'
#'
stand_db_names <- function(string, regex.table = NULL, standardized = FALSE, nthreads = 1) {
  if (is.null(regex.table)) regex.table <- tpfuns::db_clean
  regex <- stringi::stri_flatten(regex.table$regex, "|")

  if (nthreads == 1) {

    if (standardized == FALSE) string <- tpfuns::stand_chars(string, nthreads = nthreads)
    string <- stringi::stri_replace_all_regex(string, regex, "")
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
      split[[i]] <- stringi::stri_replace_all_regex(split[[i]], regex, "")
      split[[i]] <- stringi::stri_replace_all_regex(split[[i]], "\\s+", " ")
      split[[i]] <- stringi::stri_trim_both(split[[i]])
    }

    parallel::stopCluster(cl)

  }


return(string)

}
