# CHECK INPUT TABLES FOR WORD REPLACEMENT ================================================
#' Check Input Tables for Word Replacement
#'
#' @param replace_table
#' A Replacement Table (needs 2 columns: "word" and "replace")
#'
#' @return
#' Errors
check_repl_list <- function(replace_table) {
  check.dup <- replace_table$word[which(duplicated(replace_table$word) == TRUE)]
  if (length(check.dup) > 0)
    stop(paste0("Duplicated word(s): ", stringi::stri_flatten(check.dup, " | ")))

  check.trans <- replace_table$replace[which(replace_table$replace %in% replace_table$word)]
  if (length(check.trans) > 0)
    stop(paste0("Transitive word(s) replacement:", stringi::stri_flatten(check.trans, " | ")))

}

# REPLACE MISSPELLED WORDS ===============================================================
#' Standardize Commonly Misspelled Words in Strings
#'
#' @description
#' This function is part of the 'company operations' (cop) function set\cr
#' This function replaces and standardizes commonly misspelled words in company names
#' @param string
#' A charcter string
#' @param replace_table
#' A Replacement Table (needs 2 columns: "word" and "replace")\cr
#' If NULL the default table will be used
#'
#' @return
#' A character string with standardized words
#'
#' @export
#'
#' @examples
#' cop_repl_words("1st")
cop_repl_words <- function(string, replace_table = NULL) {
  `%>%` <- magrittr::`%>%`
  # read table, either default of specified by replace_table -----------------------------
  if (is.null(replace_table)) {
    table.repl.words <- tpfuns::table_repl_words
  } else {
    table.repl.words <- replace_table
  }

  check_repl_list(table.repl.words)

  table.string <- tibble::tibble(term = string) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    tidytext::unnest_tokens(words, term, token = "regex", pattern = "\\b") %>%
    dplyr::left_join(table.repl.words, by = c("words" = "word")) %>%
    dplyr::mutate(words = dplyr::if_else(is.na(replace), words, replace)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(term = stringi::stri_flatten(words))

  string <- table.string$term
  return(string)
}

# REPLACE LEGAL ENTITIES =================================================================
#' Remove Legal Entities from String
#'
#' @description
#' This function is part of the 'company operations' (cop) function set\cr
#' This function removes legal entities from company names
#'
#' @param string
#' A chracter string
#'
#' @return
#' A character String without legal entities
#'
#' @export
#'
#' @examples
#' tpfuns::cop_rem_le("basf se")
cop_rem_le <- function(string) {
  regex <- tpfuns::table_legal_entities$regex
  string <- stringi::stri_replace_all_regex(string, regex, "", vectorize_all = FALSE)
  string <- stringi::stri_trim_both(string)

  return(string)

}



# STANDARDIZE DB NAMES ===================================================================
#' Standardizes Names from Accounting Data Provider (e.g. Datastream)
#'
#' @param string String with company names to be standardized
#' @param regex.table A table with with regexes for string replacement, if NULL default table is used
#' @param standardized Is the input already standardized? If not the standardization function
#' stand_chars is used to standardize the input string
#' @param nthreads
#' Number of threads for paralell processing (as integer) \cr
#' nthreads = 1: No parallel processing (default) \cr
#' nthreads = 0: All but one cores will be used (parallel::detectCores() - 1)
#'
#' @return A string with standardized names
#' @export
#' @examples
#'a <- "BASF SE dead 31/12/2005"
#'stand_db_names(a)
#'
#'
stand_db_names <- function(string, regex.table = NULL, standardized = FALSE, nthreads = 1) {
  if (is.null(regex.table)) regex.table <- tpfuns::db_clean
  regex <- stringi::stri_flatten(regex.table$regex, "|")

  if (nthreads == 1) {

    if (standardized == FALSE) string <- tpfuns::stand_chars(string, nthreads = nthreads)
    string <- stringi::stri_replace_all_regex(string, regex, "")
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
      split[[i]] <- stringi::stri_replace_all_regex(split[[i]], regex, "")
      split[[i]] <- stringi::stri_replace_all_regex(split[[i]], "\\s+", " ")
      split[[i]] <- stringi::stri_trim_both(split[[i]])
    }

    parallel::stopCluster(cl)

  }


  return(string)

}

# MATCH COMPANIES ========================================================================
#' Matches Companies against Datastream and Orbis
#'
#' @param string
#' A character string with company names to match
#' @param match.table
#' The match table (not part of the package, needs to be defined seperately!)
#'
#' @return
#' A dtaframe with matched companies
#' @export
cop_match_comp <- function(names, match.table = NULL) {
    `%>%` <- magrittr::`%>%`

    if (is.null(match.table)) stop("function needs matching table, see documentation")

    name.match.ds  <- match.table %>%
      dplyr::filter(db_type == "ds") %>%
      dplyr::rename(match_type_db = name_id)

    name.match.bvd <- match.table %>%
      dplyr::filter(db_type == "bvd") %>%
      dplyr::rename(match_type_db = name_id)

    cat("\r matching procedure 1 (datastream)                                             ")
    match.table.0 <- tibble::tibble(name = names) %>%
      dplyr::mutate(name_1 =tpfuns::top_rem_punct(tpfuns::top_stand_punct(name))) %>%
      dplyr::mutate(name_1 =tpfuns::cop_repl_words(name_1)) %>%
      dplyr::mutate(name_1 =tpfuns::top_americanize(name_1))
    match <- fastmatch::fmatch(match.table.0$name_1, name.match.ds$comp_name_stand)
    match.table.1 <- tibble::tibble(name = match.table.0$name) %>%
      dplyr::bind_cols(name.match.ds[match, -1]) %>%
      dplyr::filter(!is.na(ident)) %>%
      dplyr::mutate(match_type_comp = 1)

    cat("\r matching procedure 2 (datastream)                                             ")
    match.table.0 <- dplyr::anti_join(match.table.0, match.table.1 %>% select(name), by = "name") %>%
      dplyr::mutate(name_2 = tpfuns::cop_rem_le(name_1))
    match <- fastmatch::fmatch(match.table.0$name_2, name.match.ds$comp_name_stand)
    match.table.2 <- tibble::tibble(name = match.table.0$name) %>%
      dplyr::bind_cols(name.match.ds[match, -1]) %>%
      dplyr::filter(!is.na(ident)) %>%
      dplyr::mutate(match_type_comp = 2)

    cat("\r matching procedure 3 (datastream)                                             ")
    match.table.0 <- dplyr::anti_join(match.table.0, match.table.2 %>% select(name), by = "name") %>%
      dplyr::mutate(name_3 = stringi::stri_replace_all_fixed(name_1, " ", ""))
    match <- fastmatch::fmatch(match.table.0$name_3, name.match.ds$comp_name_stand)
    match.table.3 <- tibble::tibble(name = match.table.0$name) %>%
      dplyr::bind_cols(name.match.ds[match, -1]) %>%
      dplyr::filter(!is.na(ident)) %>%
      dplyr::mutate(match_type_comp = 3)

    cat("\r matching procedure 4 (datastream)                                             ")
    match.table.0 <- dplyr::anti_join(match.table.0, match.table.3 %>% select(name), by = "name") %>%
      dplyr::mutate(name_4 = stringi::stri_replace_all_fixed(name_2, " ", ""))
    match <- fastmatch::fmatch(match.table.0$name_4, name.match.ds$comp_name_stand)
    match.table.4 <- tibble::tibble(name = match.table.0$name) %>%
      dplyr::bind_cols(name.match.ds[match, -1]) %>%
      dplyr::filter(!is.na(ident)) %>%
      dplyr::mutate(match_type_comp = 4)

    cat("\r matching procedure 1 (orbis)                                                  ")
    match.table.0 <- dplyr::anti_join(match.table.0, match.table.4 %>% select(name), by = "name")
    match <- fastmatch::fmatch(match.table.0$name_1, name.match.bvd$comp_name_stand)
    match.table.5 <- tibble::tibble(name = match.table.0$name) %>%
      dplyr::bind_cols(name.match.bvd[match, -1]) %>%
      dplyr::filter(!is.na(ident)) %>%
      dplyr::mutate(match_type_comp = 1)

    cat("\r matching procedure 2 (orbis)                                                  ")
    match.table.0 <- dplyr::anti_join(match.table.0, match.table.5 %>% select(name), by = "name")
    match <- fastmatch::fmatch(match.table.0$name_1, name.match.bvd$comp_name_stand)
    match.table.6 <- tibble::tibble(name = match.table.0$name) %>%
      dplyr::bind_cols(name.match.bvd[match, -1]) %>%
      dplyr::filter(!is.na(ident)) %>%
      dplyr::mutate(match_type_comp = 2)

    cat("\r matching procedure 3 (orbis)                                                  ")
    match.table.0 <- dplyr::anti_join(match.table.0, match.table.6 %>% select(name), by = "name")
    match <- fastmatch::fmatch(match.table.0$name_1, name.match.bvd$comp_name_stand)
    match.table.7 <- tibble::tibble(name = match.table.0$name) %>%
      dplyr::bind_cols(name.match.bvd[match, -1]) %>%
      dplyr::filter(!is.na(ident)) %>%
      dplyr::mutate(match_type_comp = 3)

    cat("\r matching procedure 4 (orbis)                                                  ")
    match.table.0 <- dplyr::anti_join(match.table.0, match.table.7 %>% select(name), by = "name")
    match <- fastmatch::fmatch(match.table.0$name_1, name.match.bvd$comp_name_stand)
    match.table.8 <- tibble::tibble(name = match.table.0$name) %>%
      dplyr::bind_cols(name.match.bvd[match, -1]) %>%
      dplyr::filter(!is.na(ident)) %>%
      dplyr::mutate(match_type_comp = 4)

    match.table <- bind_rows(match.table.0 %>% select(name), match.table.1, match.table.2, match.table.3,
                             match.table.4, match.table.5, match.table.6, match.table.7,
                             match.table.8)
    return(match.table)
  }
