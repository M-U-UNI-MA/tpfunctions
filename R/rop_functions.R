# READ RDS ===============================================================================
#' Read Multiple RDS Files
#'
#' @param files
#' The full paths to the files
#' @param doc.id
#' Either NULL or a one-element charcter string
#'
#' @return
#' A Dataframe
#'
#' @export
rop_rds <- function(files, doc.id = NULL) {
  if (!is.null(doc.id))
    names(files) <- gsub(".rds", "", basename(files), fixed = TRUE)

  purrr::map_dfr(files, readr::read_rds, .id = doc.id)
}

# READ TEXT ==============================================================================
#' Read Multiple Text Files
#'
#' @param files
#' The full paths to the files
#' @param doc.id
#' Either NULL or a one-element charcter string
#'
#' @return
#' A Dataframe
#' @export
rop_txt <- function(files, doc.id = NULL) {
  txt <- readtext::readtext(files)
  if (is.null(doc.id)) {
    txt <- txt[-1]
  } else {
    names(txt) <- c(doc.id, "text")
    txt[1] <- gsub(".txt", "", txt[, 1], fixed = TRUE)
  }
  return(txt)
}

# READ CSV ===============================================================================
#' Read Multiple CSV files
#'
#' @param files
#' The full paths to the files
#' @param doc.id
#' Either NULL or a one-element charcter string
#' @param col.types
#' Either NULL, a single character or a character string for all columns
#' @param delim
#' The delimiter, default is ';'
#'
#' @return
#' A Dataframe
#' @export
rop_csv <- function(files, doc.id = NULL, col.types = NULL, delim = ";") {

  if (!is.null(doc.id)) names(files) <- gsub(".csv", "", basename(files), fixed = TRUE)
  if (is.null(col.types)) col <- readr::cols(.default = readr::col_guess())
  if (length(col.types) == 1) col <- readr::cols(.default = col.types)
  if (length(col.types) > 1) col <- readr::cols(col.types)

  read <- function(file) readr::read_delim(file, delim, col_types = col)

  purrr::map_dfr(files, read, .id = doc.id)
}
