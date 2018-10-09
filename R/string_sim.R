#' String Similarity
#'
#' @param a Source String
#' @param b Target String
#' @param par Parellel Execution?
#'
#' @return A dataframe with the highest similarity between the source and the target string
#' @export
string_sim <- function (a, b, par = FALSE) {
  pbapply::pboptions(type = "timer", char = "=", txt.width = 90)
  `%>%` <- magrittr::`%>%`
  source <- a
  target <- b

  if (par == TRUE) {
    cl <- parallel::makeCluster(parallel::detectCores() - 1)
    doSNOW::registerDoSNOW(cl)
    parallel::clusterExport(cl, c("source", "target"), environment())
  } else {
    cl <- NULL
  }

  x.0 <- pbapply::pblapply(source, function(x) {
    y.0 <- stringdist::stringdistmatrix(x, target, method = "jw")
    y.1 <- apply(y.0, 1, which.min)
    y.2 <- apply(y.0, 1, min)
    z <- tibble::tibble(comp = x,
                        row_id = y.1,
                        sim = y.2)
    z$name = b[y.1]
    return(z)
  }, cl = cl)
  if (par == TRUE) parallel::stopCluster(cl)
  z.1 <- dplyr::bind_rows(x.0) %>%
    dplyr::select(comp, name, sim)
  return(z.1)

}
