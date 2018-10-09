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
#' @param standardized Is the input standardized?
#'
#' @return A dataframe with standardized legal entities
#' @export
stand_le_2 <- function(string, le.table = NULL, standardized = FALSE) {
  `%>%` <- magrittr::`%>%`
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


  table <- tibble::tibble(name_orig = string) %>%
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

  table <- table %>%
    dplyr::arrange(id) %>%
    dplyr::mutate(
      name = stringi::stri_replace_all_regex(name, legal.entity.abbr$le_regex, "", vectorize_all = FALSE)
    ) %>%
    dplyr::mutate(
      name = stringi::stri_replace_all_regex(name, legal.entity.full$le_regex, "", vectorize_all = FALSE)
    ) %>%
    dplyr::mutate(name = stringi::stri_trim_both(name)) %>%
    dplyr::mutate(name = stringi::stri_replace_all_regex(name, "\\s+", " ")) %>%
    dplyr::select(name_orig, name)



  return(table$name)
}

