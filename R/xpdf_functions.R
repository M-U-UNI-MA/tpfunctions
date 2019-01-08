
#' Get PDF Info XPDF TOOLS
#'
#' @param file A pdf file
#'
#' @return
#' A Dataframe
#' @export
xpdf_meta <- function(file) {
  doc.id <- gsub(".pdf", "", basename(file), fixed = TRUE)
  file <- paste0('"', file, '"')

  trim <- function(x) stringi::stri_trim_both(str = x)
  sub  <- function(x, start, stop) stringi::stri_sub(str = x, from = start , to = stop)

  suppressWarnings(
  x <- system(paste("pdfinfo -rawdates", file), intern = TRUE, wait = FALSE)
  )
  if (any(stringi::stri_detect(x, fixed = "Syntax Warning: May not be a PDF file"))) {
    cols <- "corrupted"
    vals <- 1
  } else if (any(stringi::stri_detect(x, fixed = "Command Line Error: Incorrect password"))) {
    cols <- "pw_protected"
    vals <- 1
  } else {
    cols <- trim(unlist(lapply(x, function(y) sub(y, 1 , 16))))
    cols <- stringi::stri_trans_tolower(gsub(":", "", cols, fixed = TRUE))
    vals <- unlist(lapply(x, function(y) sub(y, 17, nchar(y))))
  }
  table <- tibble::as_tibble(matrix(c(doc.id, vals), 1, ncol = length(vals) + 1))
  colnames(table) <- c("doc_id", cols)
  return(table)
}
