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
par_start <- function(nthread = 1, pb = c("timer", "txt", "win", "tk", "none")) {
  if (!any(pb %in% c("timer", "txt", "win", "tk", "none")))
    stop("wrong progress bar option")
  if (length(pb) > 1)
    pb <- pb[1]
  pbapply::pboptions(type = pb, char = "=", txt.width = 90)
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

#' Wrapper Around the Split Function
#'
#' @description
#' Splits an object into chunk sizes and puts them into a list
#'
#' @param x
#' The object
#' @param chunk.size
#' The number of elements
#' @param type
#' How should the elements be ordered?
#'
#' @return
#' A list
#' @export
#'
#' @examples
#' libraray(tpfuns)
#'
#' split_chunk(1:10, 4, "sort")
#' split_chunk(1:10, 4, "equal")
#' split_chunk(1:10, 15, "rand")
split_chunk <- function(x, chunk.size, type = c("sort", "equal", "rand")) {
  if (!any(type %in% c("sort", "equal", "rand"))) stop("wrong split type")
  if (length(type) > 1) type <- type[1]

  if (is.table(x) | is.matrix(x)) {l <- nrow(x)} else {l <- length(x)}

  ids  <- rep(1:ceiling(l / chunk.size), chunk.size)[1:l]

  if (type == "sort") ids <- sort(ids)
  if (type == "equal") ids <- ids
  if (type == "rand") ids <- sample(ids)

  out <- split(x, ids)
  return(out)
}
