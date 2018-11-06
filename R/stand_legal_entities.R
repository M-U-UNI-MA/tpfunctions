#' Standardize Legal Entities
#'
#' @param string The strings containing legal entities
#' @param le.table A table with legal entities, if NULL the default table will be used
#' @param standardized Is the input standardized?
#'
#' @return A dataframe with standardized legal entities
#' @export
stand_le <- function(string, le.table = NULL, standardized = FALSE) {
  `%>%` <- magrittr::`%>%`
  # prepare the legal entity table -------------------------------------------------------
  legal.entity <- tpfuns::legal_entities %>%
    dplyr::mutate(le_abbr = tpfuns::stand_chars(le_abbr)) %>%
    dplyr::mutate(le_full = tpfuns::stand_chars(le_full)) %>%
    dplyr::select(-source) %>%
    dplyr::distinct(., .keep_all = TRUE)

  legal.entity.full <- legal.entity %>%
    dplyr::select(-le_abbr) %>%
    dplyr::distinct(., .keep_all = TRUE) %>%
    dplyr::mutate(le_regex = le_full) %>%
    dplyr::mutate(le_regex = stringi::stri_replace_all_fixed(le_regex, " ", "\\s")) %>%
    dplyr::mutate(le_regex = stringi::stri_replace_all_fixed(le_regex, "(", "\\(")) %>%
    dplyr::mutate(le_regex = stringi::stri_replace_all_fixed(le_regex, ")", "\\)")) %>%
    dplyr::mutate(le_regex = paste0("\\b", le_regex, "$")) %>%
    dplyr::arrange(dplyr::desc(stringi::stri_count_fixed(le_full, " ") + 1))


  legal.entity.abbr <- legal.entity %>%
    dplyr::group_by(le_abbr) %>%
    dplyr::summarise(
      le_stand   = dplyr::first(le_stand),
      le_country = stringi::stri_flatten(le_country, " | ")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(le_regex = le_abbr) %>%
    dplyr::mutate(le_regex = stringi::stri_replace_all_regex(le_regex, "([a-z])", "$1\\\\s?")) %>%
    dplyr::mutate(le_regex = stringi::stri_replace_last_fixed(le_regex, "\\s?", "")) %>%
    dplyr::mutate(le_regex = stringi::stri_replace_all_fixed(le_regex, " ", "\\s")) %>%
    dplyr::mutate(le_regex = paste0("\\b", le_regex, "$")) %>%
    dplyr::arrange(dplyr::desc(stringi::stri_count_fixed(le_stand, " ") + 1))


  table <- tibble::tibble(name_orig = string, detect = FALSE) %>%
    dplyr::mutate(legal_entity = "<NONE>") %>%
    dplyr::mutate(country = "<NONE>")  %>%
    dplyr::mutate(id = dplyr::row_number())

  if (standardized == FALSE) {
    table <- table %>%
      dplyr::mutate(name = tpfuns::stand_chars(name_orig))
  } else {
    table <- table %>%
      dplyr::mutate(name = name_orig)
  }

  table <- table %>%
    dplyr::mutate(name = stringi::stri_replace_all_fixed(name, "(", "")) %>%
    dplyr::mutate(name = stringi::stri_replace_all_fixed(name, ")", ""))

  for (i in 1:nrow(legal.entity.abbr)) {
    legal.regex   <- legal.entity.abbr$le_regex[i]
    legal.stand   <- legal.entity.abbr$le_stand[i]
    legal.country <- legal.entity.abbr$le_country[i]

    table <- table %>%
      dplyr::mutate(detect = dplyr::if_else(
        detect == FALSE,
        stringi::stri_detect_regex(name, legal.regex),
        TRUE
      )) %>%
      dplyr::mutate(
        name = dplyr::if_else(
          detect == TRUE & legal_entity == "<NONE>",
          stringi::stri_replace_all_regex(name, legal.regex, ""),
          name
        )
      ) %>%
      dplyr::mutate(
        legal_entity = dplyr::if_else(
          detect == TRUE & legal_entity == "<NONE>",
          legal.stand,
          legal_entity
        )
      ) %>%
      dplyr::mutate(country = dplyr::if_else(detect == TRUE &
                                               country == "<NONE>",
                                             legal.country,
                                             country))
  }
  for (i in 1:nrow(legal.entity.abbr)) {
    legal.regex   <- legal.entity.full$le_regex[i]
    legal.stand   <- legal.entity.full$le_stand[i]
    legal.country <- legal.entity.full$le_country[i]

    table <- table %>%
      dplyr::mutate(detect = dplyr::if_else(
        detect == FALSE,
        stringi::stri_detect_regex(name, legal.regex),
        TRUE
      )) %>%
      dplyr::mutate(
        name = dplyr::if_else(
          detect == TRUE & legal_entity == "<NONE>",
          stringi::stri_replace_all_regex(name, legal.regex, ""),
          name
        )
      ) %>%
      dplyr::mutate(
        legal_entity = dplyr::if_else(
          detect == TRUE & legal_entity == "<NONE>",
          legal.stand,
          legal_entity
        )
      ) %>%
      dplyr::mutate(country = dplyr::if_else(detect == TRUE & country == "<NONE>",
                               legal.country,
                               country))
  }

  table <- table %>%
    dplyr::mutate(country = stringi::stri_replace_all_fixed(country, "<NONE>", "")) %>%
    dplyr::mutate(legal_entity = stringi::stri_replace_all_fixed(legal_entity, "<NONE>", "")) %>%
    dplyr::arrange(id) %>%
    dplyr::select(name_orig, name, legal_entity, country) %>%
    dplyr::mutate(name = stringi::stri_trim_both(name)) %>%
    dplyr::mutate(name = stringi::stri_replace_all_regex(name, "\\s+", " "))

  return(table)
}


#' Standardize Legal Entities (Vectorized version)
#'
#' @param string The strings containing legal entities
#' @param le.table A table with legal entities, if NULL the default table will be used
#' @param standardized Is the input already standardized? If not the standardization function
#' stand_chars is used to standardize the input string
#' @param nthreads
#' Number of threads for paralell processing (as integer) \cr
#' nthreads = 1: No parallel processing (default) \cr
#' nthreads = 0: All but one cores will be used (parallel::detectCores() - 1)
#'
#' @return A string with legal entities removed
#' @export
#' @examples
#' a <- c("BASF SE", "BASF AG")
#'stand_le_2(a)
#'
stand_le_2 <- function(string, le.table = NULL, standardized = FALSE, nthreads = 1) {
  `%>%` <- magrittr::`%>%`
  # prepare the legal entity table

  if (is.null(le.table)) le.table <- tpfuns::legal_entities

  le <- le.table  %>%
    dplyr::mutate(le_name = tpfuns::stand_chars(le_name)) %>%
    dplyr::distinct(occurance, le_name, .keep_all = TRUE) %>%
    dplyr::mutate(ngram = stringi::stri_count_fixed(le_name, " ") + 1) %>%
    dplyr::arrange(type, dplyr::desc(ngram)) %>%
    dplyr::mutate(regex = stringi::stri_replace_all_fixed(le_name, "(", "\\(?")) %>%
    dplyr::mutate(regex = stringi::stri_replace_all_fixed(regex, ")", "\\)?"))

  le.abbr <- le %>%
    dplyr::filter(type == "abbr") %>%
    dplyr::mutate(regex = stringi::stri_replace_all_regex(regex, " ", "")) %>%
    dplyr::mutate(regex = stringi::stri_replace_all_regex(regex, "([a-z])", "$1\\(\\\\s+\\)?")) %>%
    dplyr::mutate(regex = stringi::stri_replace_all_regex(regex, "\\(\\\\s\\+\\)\\?$", "")) %>%
    dplyr::mutate(regex = dplyr::if_else(occurance == "End",
                                         paste0("\\b", regex, "$"),
                                         paste0("^", regex, "\\b")))

  le.full <- le %>%
    dplyr::filter(type == "full") %>%
    dplyr::mutate(regex = dplyr::if_else(occurance == "End",
                                         paste0("\\b", regex, "$"),
                                         paste0("^", regex, "\\b")))

  le <- dplyr::bind_rows(le.abbr, le.full)
  le.regex   <- le$regex
  le.table <- le.table %>%
    dplyr::mutate(le_abbr = tpfuns::stand_chars(le_abbr)) %>%
    dplyr::mutate(le_full = tpfuns::stand_chars(le_full)) %>%
    dplyr::mutate(abbr_ngram = stringi::stri_count_fixed(le_abbr, " ") + 1) %>%
    dplyr::mutate(full_ngram = stringi::stri_count_fixed(le_full, " ") + 1) %>%
    dplyr::mutate(le_abbr = stringi::stri_replace_all_fixed(le_abbr, " ", "\\s")) %>%
    dplyr::mutate(le_abbr = stringi::stri_replace_all_regex(le_abbr, "([a-z])", "$1\\\\s?")) %>%
    dplyr::mutate(le_abbr = stringi::stri_replace_last_fixed(le_abbr, "\\s?", "")) %>%
    dplyr::mutate(le_full = stringi::stri_replace_all_fixed(le_full, " ", "\\s")) %>%
    dplyr::mutate(le_abbr = stringi::stri_replace_all_fixed(le_abbr, "(", "\\(")) %>%
    dplyr::mutate(le_abbr = stringi::stri_replace_all_fixed(le_abbr, ")", "\\)")) %>%
    dplyr::mutate(le_full = stringi::stri_replace_all_fixed(le_full, "(", "\\(")) %>%
    dplyr::mutate(le_full = stringi::stri_replace_all_fixed(le_full, ")", "\\)")) %>%
    dplyr::mutate(le_abbr = paste0("\\b", le_abbr, "$")) %>%
    dplyr::mutate(le_full = paste0("\\b", le_full, "$")) %>%
    dplyr::distinct(., .keep_all = TRUE)

  le.abbr <- le.table %>%
    dplyr::select(le_abbr, abbr_ngram) %>%
    dplyr::arrange(desc(abbr_ngram)) %>%
    dplyr::distinct(le_abbr)

  le.abbr.regex <- stringi::stri_flatten(le.abbr$le_abbr, "|")

  le.full <- le.table %>%
    dplyr::select(le_full, full_ngram) %>%
    dplyr::arrange(desc(full_ngram)) %>%
    dplyr::distinct(le_full)

  le.full.regex <- stringi::stri_flatten(le.full$le_full, "|")

  le.regex <- paste0(le.abbr.regex, le.full.regex, collapse = "|")

  if (nthreads == 1) {

  if (standardized == FALSE) string <- tpfuns::stand_chars(string, nthreads = nthreads)
  string <- stringi::stri_replace_all_regex(string, le.regex, "", vectorize_all = F)
  string <- stringi::stri_replace_all_regex(string, le.regex, "")
  string <- stringi::stri_replace_all_regex(string, "\\s+", " ")
  string <- stringi::stri_trim_both(string)
  } else {
    if (nthreads == 0) nthreads <- parallel::detectCores() - 1
    if (standardized == FALSE) string <- tpfuns::stand_chars(string, nthreads = nthreads)

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

      split[[i]] <- stringi::stri_replace_all_regex(split[[i]], le.regex, "", vectorize_all = F)
      split[[i]] <- stringi::stri_replace_all_regex(split[[i]], le.regex, "")
      split[[i]] <- stringi::stri_replace_all_regex(split[[i]], "\\s+", " ")
      split[[i]] <- stringi::stri_trim_both(split[[i]])
    }

    parallel::stopCluster(cl)

  }


  return(string)
}
