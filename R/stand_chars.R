#' Standardization of Strings
#'
#' @param string A string or a vector of strings to be standardized
#' @param stand.table A table with standardizations (needs to have a column for the
#' chracters that needs to be standardized (in hex values) and a column for the replacement)
#' See the attached table as example. If no table is specified the default table will be used
#' @param nthreads
#' Number of threads for paralell processing (as integer) \cr
#' nthreads = 1: No parallel processing (default) \cr
#' nthreads = 0: All but one cores will be used (parallel::detectCores() - 1)
#'
#' @return A string
#' @export
#' @examples
#' # Replacement of non-ASCII characters to its nearest ASCII equivalent
#' a <- "ḂḃḊḋḞḟṀṁṖṗṠṡṪṫẀẁẂẃẄẅẛỲỳƀƁƂƃƄƅƆƇƈƉƊƋƌƍƎƏƐƑƒƓƔƕƖƗƘƙƚƛƜƝƞƟƠơƢƣƤƥƦƧƨƩƪƫƬƭƮƯưƱƲƳƴƵƶƷƸƹ
#' ƺƻƼƽƾƿǀǁǂǃǄǅǆǇǈǉǊǋǌǍǎǏǐǑǒǓǔǕǖǗǘǙǚǛǜǝǞǟǠǡǢǣǤǥǦǧǨǩǪǫǬǭǮǯǰǱǲǳǴǵǶǷǸǹǺǻǼǽǾǿȀȁȂȃȄȅ
#' ȆȇȈȉȊȋȌȍȎȏȐȑȒȓȔȕȖȗȘșȚțȜȝȞȟȠȡȢȣȤȥȦȧȨȩȪȫȬȭȮȯȰȱȲȳȴȵȶȷȸȹȺȻȼȽȾȿɀɁɂɃɄɅɆɇɈɉɊɋɌɍɎɏĀāĂăĄą
#' ĆćĈĉĊċČčĎďĐđĒēĔĕĖėĘęĚěĜĝĞğĠġĢģĤĥĦħĨĩĪīĬĭĮįİıĲĳĴĵĶķĸĹĺĻļĽľĿŀŁłŃńŅņŇňŉŊŋŌōŎŏŐőŒœŔŕŖŗŘ
#' řŚśŜŝŞşŠšŢţŤťŦŧŨũŪūŬŭŮůŰűŲųŴŵŶŷŸŹźŻżŽžſ"
#' stand_chars(a)
#'
#' # Standardization of some characters
#' a <- c("A + B", "A AND B", "A & B", "( A and B  )")
#' stand_chars(a)
#'
#' # Standardization with parallel processing
#' a <- c("A + B", "A AND B", "A & B", "( A and B  )")
#' stand_chars(a, nthreads = 0)
#'
stand_chars <- function(string, stand.table = NULL, nthreads = 1) {
  `%>%` <- magrittr::`%>%`
  if (is.null(stand.table)) stand.table <- tpfuns::stand_table

  stand.table <- dplyr::filter(stand.table, !replace == "<none>")
  stand.table.space <- stand.table %>% dplyr::filter(replace == "<space>")

  regex.to.space <- stringi::stri_flatten(stand.table.space$hex, "|")

  stand.table.del <- stand.table %>% dplyr::filter(replace == "<del>")

  regex.to.del <- stringi::stri_flatten(stand.table.del$hex, "|")

  stand.table.other <- stand.table %>%
    dplyr::filter(replace != "<del>" & replace != "<space>") %>%
    dplyr::mutate(replace = stringi::stri_replace_all_fixed(replace, "<space>", " "))

  regex.other   <- stand.table.other$hex
  replace.other <- stand.table.other$replace


  if (nthreads == 1) {
    string <- stringi::stri_trans_tolower(string)
    string <- stringi::stri_replace_all_regex(string, regex.to.space, " ")
    string <- stringi::stri_replace_all_regex(string, regex.to.del, "")
    string <- stringi::stri_replace_all_regex(string, regex.other, replace.other, vectorize_all = F)
    string <- textclean::replace_non_ascii(string)
    string <- stringi::stri_replace_all_regex(string, "\\s+", " ")
    string <- stringi::stri_trim_both(string)

  } else {
    if (nthreads == 0) {
      nthreads <- parallel::detectCores() - 1
    }

    split <- split(string, sort(1:length(string)%%nthreads))

    cl <- parallel::makeCluster(nthreads)
    doSNOW::registerDoSNOW(cl)

    pb <- txtProgressBar(max = length(length(split)), style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)

    `%dopar%` <- foreach::`%dopar%`

    string <- foreach::foreach(
      i = 1:length(split),
      .packages = ("stringi"),
      .options.snow = opts,
      .combine = c
    ) %dopar% {
      split[[i]] <- stringi::stri_trans_tolower(split[[i]])
      split[[i]] <- stringi::stri_replace_all_regex(split[[i]], regex.to.space, " ")
      split[[i]] <- stringi::stri_replace_all_regex(split[[i]], regex.to.del, "")
      split[[i]] <- stringi::stri_replace_all_regex(split[[i]], regex.other, replace.other, vectorize_all = F)
      split[[i]] <- textclean::replace_non_ascii(split[[i]])
      split[[i]] <- stringi::stri_replace_all_regex(split[[i]], "\\s+", " ")
      split[[i]] <- stringi::stri_trim_both(split[[i]])
    }

    parallel::stopCluster(cl)

  }
  return(string)
}
