
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

  x <- system(paste("pdfinfo -rawdates", file), intern = TRUE, wait = FALSE)
  cols <- stringi::stri_trim_both(unlist(lapply(x, function(y) stringi::stri_sub(y, 1 , 16))))
  cols <- stringi::stri_trans_tolower(gsub(":", "", cols, fixed = TRUE))
  vals <- unlist(lapply(x, function(y) stringi::stri_sub(y, 17, nchar(y))))
  table <- tibble::as_tibble(matrix(c(doc.id, vals), 1, ncol = length(vals) + 1))
  colnames(table) <- c("doc_id", cols)
  return(table)
}
