#' Decrypt pdfs
#'
#' @description A wrapper around the qpdf command line tool.
#' First, the qpdf tool must be downloaded (https://sourceforge.net/projects/qpdf/files/qpdf/).
#' Second, zipped file must be extracted to any folder.
#' Third, environment variables must be updated (...\qpdf-x.x.x\bin).
#' Program is tested on qpdf version 8.1.0.
#'
#'
#' @param path.in complete path to the encrypted pdf file
#' (must be different than path.out)
#' @param path.out complete path where the decrypted file should be written
#' (must be different than path.in)
#'
#' @return A pdf file (decrypted)
#' @export
decrypt_pdf <- function(file.in, file.out) {
  tryCatch({
    system(paste('qpdf --decrypt', file.in, file.out))
  }, error = function(e)
    "error")
}
