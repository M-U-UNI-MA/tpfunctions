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
cop_match_comp <- function(names, match.table = NULL, sub.ratio = 0.5, sim.ratio = 0.5) {
  if (is.null(match.table)) stop("function needs matching table, see documentation")
  no <- names
  # set internal functions ===============================================================
  `%>%`   <- magrittr::`%>%`
  `%fin%` <- fastmatch::`%fin%`
  f_1 <- function(x) {tpfuns::top_stand_punct(string = x, lower = TRUE, ascii = TRUE)}
  f_2 <- function(x) {tpfuns::top_rem_punct(string = x, punct.replacement = TRUE)}
  f_3 <- function(x) {tpfuns::cop_repl_words(string = x, replace_table = NULL)}
  f_4 <- function(x) {tpfuns::top_americanize(string = x)}
  f_5 <- function(x) {stringi::stri_replace_all_fixed(str = x, pattern = " ", replacement = "")}
  f_match_table <- function(name, match, match.type) {
    tibble::tibble(name = name) %>%
      dplyr::bind_cols(match.table[match,]) %>%
      dplyr::filter(!is.na(ident)) %>%
      dplyr::mutate(match_type_comp = 1)
  }
  f_match_table_sub <- function(names, match.type) {
    lapply(max(nchar(names)):4, function(i) {
      name.sub <- stringi::stri_sub(names, 1, i)
      match.table <- match.table %>%
        dplyr::filter(match_type_db == 1) %>%
        dplyr::mutate(name_match = stringi::stri_sub(comp_name_stand, 1, i))

      match <- tibble::tibble(name = no, name_match = name.sub, co = nchar(names)) %>%
        dplyr::inner_join(match.table, by = "name_match") %>%
        dplyr::mutate(match_type_comp = match.type) %>%
        dplyr::mutate(cs = i) %>%
        dplyr::mutate(sim = round(cs / co, 4)) %>%
        dplyr::filter(sim >= sub.ratio) %>%
        dplyr::top_n(3, sim)
    }) %>% dplyr::bind_rows() %>%
      dplyr::arrange(name, ident, dplyr::desc(cs)) %>%
      dplyr::distinct(name, ident, .keep_all = TRUE)
  }

  # no:   names (names_original)
  # ns:   standardized names (name_stand)
  # nsl:  standardized names w/o legal entity (name_stand_le)
  # nss:  standardized names w/o spaces (name_stand_space)
  # nsls: standardized names w/o legal entity and spaces (name_stand_le_space)

  # 1: matching standardized names =======================================================
  cat("\r1: matching standardized names ------------------------------------------------")
  ns            <- f_4(f_3(f_2(f_1(no))))
  match.1       <- fastmatch::fmatch(ns, match.table$comp_name_stand)
  match.table.1 <- f_match_table(name = no, match = match.1, match.type = 1)

  select <- which(is.na(match.1))
  no <- no[select]; ns <- ns[select]
  match.table <- dplyr::filter(match.table, !ident %fin% match.table.1$ident)

  # 2: matching standardized names (w/o legal entity) ====================================
  cat("\r2: matching standardized names (w/o legal entity) -----------------------------")
  nsl           <- tpfuns::cop_rem_le(ns)
  match.2       <- fastmatch::fmatch(nsl, match.table$comp_name_stand)
  match.table.2 <- f_match_table(name = no, match = match.2, match.type = 2)

  select <- which(is.na(match.2))
  no <- no[select]; ns <- ns[select]; nsl <- nsl[select]
  match.table <- dplyr::filter(match.table, !ident %fin% match.table.2$ident)

  # 3: matching standardized names (w/o space) ===========================================
  cat("\r3: matching standardized names (w/o space) ------------------------------------")
  nss           <- f_5(ns)
  match.3       <- fastmatch::fmatch(nss, match.table$comp_name_stand)
  match.table.3 <- f_match_table(name = no, match = match.3, match.type = 3)

  select <- which(is.na(match.3))
  no <- no[select]; ns <- ns[select]; nsl <- nsl[select]; nss <- nss[select]
  match.table <- dplyr::filter(match.table, !ident %fin% match.table.3$ident)

  # 4: matching standardized names (w/o legal entity and space) ==========================
  cat("\r4: matching standardized names (w/o legal entity and space) ----------------")
  nsls          <- f_5(nsl)
  match.4       <- fastmatch::fmatch(nsls, match.table$comp_name_stand)
  match.table.4 <- f_match_table(name = no, match = match.4, match.type = 4)

  select <- which(is.na(match.4))
  no  <- no[select];  ns   <- ns[select]; nsl <- nsl[select]
  nss <- nss[select]; nsls <- nsls[select]
  match.table <- dplyr::filter(match.table, !ident %fin% match.table.4$ident)

  # matching equalized length ============================================================
  # 5: equalized matching standardized names ---------------------------------------------
  cat("\r5: equalized matching standardized names --------------------------------------")
  match.table.5 <- f_match_table_sub(names = ns, match.type = 5) %>%
    dplyr::mutate(sim_type = "sub")

  # 6: approximate matching standardized names -------------------------------------------
  cat("\r6: approximate matching standardized names ------------------------------------")
  match.table.6 <- lapply(1:length(ns), function(x){
    y <- tibble::tibble(sim = stringdist::stringsim(a = ns[x],
                                                    b = match.table$comp_name_stand,
                                                    method = "lv")) %>%
      dplyr::mutate(id = dplyr::row_number()) %>%
      dplyr::mutate(name = no[x]) %>%
      dplyr::top_n(n = 3, wt = sim) %>%
      dplyr::filter(sim >= sim.ratio) %>%
      dplyr::mutate(match_type_comp = 1)
    y <- dplyr::bind_cols(y, match.table[y$id, ])
  }) %>% dplyr::bind_rows() %>%
    dplyr::select(-id) %>%
    dplyr::mutate(sim_type = "lev")

  # append tables ========================================================================
  match.table.exact <- dplyr::bind_rows(match.table.1, match.table.2, match.table.3, match.table.4) %>%
    dplyr::select(ident, name_comp = name, name_match = comp_name_stand, name_db = comp_name,
                  match_type_comp, match_type_db, name_type_db = name_type)

  match.table.sub <- dplyr::bind_rows(match.table.5, match.table.6) %>%
    dplyr::select(ident, name_comp = name, name_match, name_db = comp_name, match_type_comp,
                  match_type_db, name_type_db = name_type, sim, sim_type) %>%
    arrange(name_comp, match_type_comp, dplyr::desc(sim))

  return(list(match.table.exact, match.table.sub))
}
