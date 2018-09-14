#' Converts a PDF to a tokenized csv table
#'
#' @param A PDF file
#' @param outdir The folder where the txt file should be saved (If NULL a new folder
#' called 'txt' will be created in the same directory where the pdf file is located)
#'
#' @return A csv file
#' @export
pdf_to_csv <- function(file, outdir) {
  `%>%` <- purrr::`%>%`

  outfile <- paste0(outdir, gsub("\\.pdf$", ".txt", basename(file)))
  if (!dir.exists(outdir)) dir.create(outdir, FALSE, TRUE)

  suppressWarnings(tryCatch(
    system(paste('"pdftotext.exe"', file, outfile), ignore.stderr = TRUE),
    error = function(e) "error"
  ))

  tryCatch({
    text <-
      readtext::readtext(outfile, ignore_missing_files = TRUE) %>%
      tidyr::separate_rows(text, sep = "\\f") %>%
      dplyr::mutate(page_num = dplyr::row_number()) %>%
      tidytext::unnest_tokens(paragraph, text, "paragraphs", to_lower = F) %>%
      dplyr::group_by(page_num) %>%
      dplyr::mutate(para_num = dplyr::row_number()) %>%
      dplyr::ungroup()

    if (nrow(text) == 0) {
      text <- dplyr::tibble(page_num = NA, para_num = NA, sent_num = NA,
                            word_num = NA, token = NA)
    } else {
      text <- text %>%
        tidytext::unnest_tokens(sentence,
                                paragraph,
                                "regex",
                                pattern = "(?<!\\w\\.\\w.)(?<![A-Z][a-z]\\.)(?<=\\.|\\?)\\s",
                                to_lower = F) %>%
        dplyr::group_by(page_num, para_num) %>%
        dplyr::mutate(sent_num = dplyr::row_number()) %>%
        dplyr::ungroup() %>%
        tidytext::unnest_tokens(token, sentence, "ngrams", n = 1) %>%
        dplyr::group_by(page_num, para_num, sent_num) %>%
        dplyr::mutate(word_num = dplyr::row_number()) %>%
        dplyr::ungroup() %>%
        dplyr::select(page_num, para_num, sent_num, word_num, token)
    }
  }, error = function(e)
    "error")


  unlink(outfile)
  readr::write_delim(text, gsub("\\.txt$", ".csv", outfile), ";", "")
  return(NULL)

}
