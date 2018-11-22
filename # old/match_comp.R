#' Match company names
#'
#' @param a source string
#' @param b target string
#' @param approx Should approximate matching be included (takes significantly longer than exact matches)
#'
#' @return A dataframe containing matches and similarities
#' @export
#' @examples
#' # Company name vectors for matching
#' source <- c("BASF", "SIEMENS") # source vector
#' target <- c("BASF AG", "BASFSE", "SIEMENS AG", "BASF") # target vector
#'
#' # Company matching !without! approximate matches
#' match_comp(a = source, b = target)
#'
#' # Company matching !with! approximate matches
#' match_comp(a = source, b = target, approx = TRUE)
match_comp <- function(a, b, approx = FALSE){
  `%>%` <- magrittr::`%>%`
  x <- fastmatch::fmatch(a, b)
  a.match.exact <- a[which(!is.na(x))]
  b.match.exact <- b[x[!is.na(x)]]

  a.nonmatch <- a[!a %in% a.match.exact]
  b.nonmatch <- b[!b %in% b.match.exact]

  match.exact <- tibble::tibble(name       = a.match.exact,
                                name_match = b.match.exact,
                                sim        = rep(1, length(a.match.exact)),
                                type       = rep("exact", length(a.match.exact))
  )

  a.whitespace <- stringi::stri_replace_all_fixed(a.nonmatch, " ", "")
  b.whitespace <- stringi::stri_replace_all_fixed(b.nonmatch, " ", "")

  y <- fastmatch::fmatch(a.whitespace, b.whitespace)
  a.match.ws <- a.nonmatch[which(!is.na(y))]
  b.match.ws <- b.nonmatch[y[!is.na(y)]]

  a.nonmatch <- a.nonmatch[which(is.na(y))]
  b.nonmatch <- b.nonmatch[which(!(1:length(b.nonmatch)) %in% y)]

  match.ws <- tibble::tibble(name       = a.match.ws,
                             name_match = b.match.ws,
                             sim        = rep(1, length(a.match.ws)),
                             type       = rep("whitespace", length(a.match.ws))
  )

  match <- dplyr::bind_rows(match.exact, match.ws)
  if (approx == TRUE) {
    z <- stringdist::amatch(a.nonmatch, b.nonmatch, maxDist = 5, method = "lv")

    a.match.approx <- a.nonmatch[which(!is.na(z))]
    b.match.approx <- b.nonmatch[z[!is.na(z)]]

    match.approx <- tibble::tibble(name = a.match.approx, name_match = b.match.approx) %>%
      dplyr::mutate(sim = stringdist::stringsim(name, name_match, method = "lv")) %>%
      dplyr::mutate(type = "approx")

    match <- dplyr::bind_rows(match, match.approx)
    a.nonmatch  <- a[!a %in% match$name]
  }

  all <- dplyr::bind_rows(match, tibble::tibble(name = a.nonmatch))

  return(all)
}
