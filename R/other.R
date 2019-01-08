# TRIM AND LOWER =========================================================================
#' Trim and replace whitespaces
#'
#' @description
#' A simple wrapper around some stringi function \cr
#' - Replaces multiple blanks with a single blank \cr
#' - Trims both sides of the string \cr
#' - Convert all characters to lower case letters
#'
#' @param string
#' A character of character vector
#'
#' @return
#' A string
#' @export
oth_lower_ws <- function(string){
  string <- stringi::stri_trans_tolower(string)
  string <- stringi::stri_replace_all_regex(string, "\\s+", " ")
  string <- stringi::stri_trim_both(string)
}

# START PARALLEL FOR PBAPPLY =============================================================
#' Make Cluster and Progress Bar for PBAPPLY functions
#'
#' @param nthread
#' Number of cores to be used
#'
#' @return
#' Workers in the object 'cl'
#' @export
# usethis::use_package("parallel")
# usethis::use_package("pbapply")
# usethis::use_package("doSNOW")
par_start <- function(nthread = 1) {
  pbapply::pboptions(type = "timer", char = "=", txt.width = 90)
  if (nthread == 1) {
    cl <<- NULL
  } else {
    cl <<- parallel::makeCluster(nthread)
    doSNOW::registerDoSNOW(cl)
    return(cl)
  }
}
# STOP PARALLEL FOR PBAPPLY ==============================================================
#' Stops clusters made by par_start
#'
#' @return
#' NULL
#' @export
# usethis::use_package("parallel")
par_stop <- function() {
  if (!is.null(cl)) {
    parallel::stopCluster(cl)
  }
}

#' BACKUP NAME creation
#'
#' @param dir The folder where the file should be saved
#' @param file The file name with file ending
#'
#' @return
#' A character string
#' @export
#'
#' @examples
#' tpfuns::oth_bu_name("test_folder", "file.csv")
oth_bu_name <- function(dir, file) {
  dir <- gsub("\\\\+", "/", dir)
  if (!stringi::stri_detect(dir, regex = "/$"))
    dir <- paste0(dir, "/")
  paste0(dir, format(Sys.time(), "%Y%m%d-%H%M%S"), "_", file)

}


