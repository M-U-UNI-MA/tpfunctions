#' Converts a PDF to text file
#'
#' @param file A PDF file
#' @param outdir The folder where the txt file should be saved (If NULL a new folder
#' called 'txt' will be created in the same directory where the pdf file is located)
#'
#' @return A text file (the text file will have the same name as the pdf file)
#' @export
pdf_to_txt <- function(file, outdir = NULL) {
  if (is.null(outdir)) {
    outdir <- paste0(dirname(file), "/txt")
    if (!dir.exists(outdir)) dir.create(outdir, FALSE)
  } else {
    if (!dir.exists(outdir)) dir.create(outdir, FALSE)
  }

  outfile <- paste0(outdir, "/", gsub("\\.pdf$", ".txt", basename(file)))

  suppressWarnings(tryCatch(
    system(paste('"pdftotext.exe"', file, outfile), ignore.stderr = TRUE),
    error = function(e)
      "error"
  ))
}
