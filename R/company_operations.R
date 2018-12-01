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

# STANDARDIZE LEGAL ENTITIES =============================================================
#' Remove Legal Entities from String
#'
#' @description
#' This function is part of the 'company operations' (cop) function set\cr
#' This function standardizes or removes legal entities from character strings with company
#' name
#'
#' @param string
#' A chracter string
#'
#' @param le.op
#' Legal Entity Operation: Should the legal entity be removed or standardized?
#'
#' @param table.return
#' Should the whole table with legal entity information be returned?
#'
#' @return
#' Either a string in the case of le.op = "remove" or le.op = "stand", or a dataframe in
#' case le.op = c("remove", "stand") or table.return = TRUE
#'
#' @export
#'
#' @examples
#' companies <- c("basf co ltd", "basf company ltd", "basf ag", "basf se", "basf aktiengesellschaft")
#' tpfuns::cop_stand_le(companies, le.op = "remove")
#' tpfuns::cop_stand_le(companies, le.op = "stand")
#' tpfuns::cop_stand_le(companies)
#' tpfuns::cop_stand_le(companies,  table.return = TRUE)
#'
cop_stand_le <- function(string, le.op = c("remove", "stand"), table.return = FALSE) {
  if (!any(c("remove", "stand") %in% le.op)) stop("wrong operation on legal entities")

  `%>%` <- magrittr::`%>%`
  regex <- tpfuns::table_legal_entities$regex
  string.adj <- string
  string.adj <- stringi::stri_replace_all_regex(string.adj, regex, "", vectorize_all = FALSE)
  string.adj <- stringi::stri_trim_both(string.adj)

  if (any(string.adj == "")) {
    string.adj[which(string.adj == "")] <- string[which(string.adj == "")]
  }

  table <-
    tibble::tibble(comp_orig = string, comp_adj = string.adj) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::mutate(legal_entity = stringi::stri_replace_all_fixed(comp_orig, comp_adj, "")) %>%
    dplyr::mutate(legal_entity = stringi::stri_trim_both(legal_entity)) %>%
    naniar::replace_with_na(replace = list(legal_entity = "")) %>%
    fuzzyjoin::fuzzy_left_join(
      tpfuns::table_legal_entities,
      by = c("legal_entity" = "regex"),
      match_fun = stringi::stri_detect_regex
    ) %>%
    dplyr::arrange(id, dplyr::desc(ngram)) %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::select(-regex, -ngram, -id) %>%
    dplyr::mutate(comp_le_stand =
                    dplyr::if_else(
                      is.na(legal_entity),
                      comp_adj,
                      paste(comp_adj, stringi::stri_trans_tolower(le_stand))
                    ))

  if (table.return == TRUE) {
    return(table)
  } else if (all(le.op == "remove")) {
    return(table$comp_adj)
  } else if (all(le.op == "stand")){
    return(table$comp_le_stand)
  } else {
    return(dplyr::select(table, comp_adj, comp_le_stand))
  }
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
#' @param match.type
#' Type of match, any combination of 'full', 'sub' and 'approx'
#'
#' @return
#' A dtaframe with matched companies
#' @export
cop_match_comp <- function(names, match.table = NULL,
                           match.type = c("full", "sub", "approx")) {

  if (!all(match.type %in%  c("full", "sub", "approx")))
    stop("wrong specification of matching type")

  # set internal functions ===============================================================
  int_stand_names <- function(string) {
    tpfuns::top_americanize(
      tpfuns::cop_repl_words(
        tpfuns::top_rem_punct(
          tpfuns::top_stand_punct(string), TRUE)))
  }
  int_rem_space <- function(string) {
    stringi::stri_replace_all_fixed(string, " ", "")
  }

  # adjust company names =================================================================
  names <- tibble::tibble(no = names)
  cat("\rstandardize company names -----------------------------------------------------")
  names <- dplyr::mutate(names, ns = int_stand_names(no))

  cat("\rstandardize and remove legal entities -----------------------------------------")
  le.stand <- tpfuns::cop_stand_le(names$ns)
  names <- dplyr::mutate(names, ls = le.stand$comp_le_stand, lr = le.stand$comp_adj)

  cat("\rremove spaces from company names ----------------------------------------------")
  names <- dplyr::mutate(names,
                         nss = int_rem_space(names$ns),
                         lss = int_rem_space(names$ls),
                         lrs = int_rem_space(names$lr)
  ) %>% dplyr::mutate(id = dplyr::row_number())

  match.cols  <- c("ns", "ls", "lr", "nss", "lss", "lrs")

  if ("full" %in% match.type) {
    # get main matches ===================================================================
    table.match.full <- tibble::tibble()
    for (i in 1:length(match.cols)) {
      cat("\rmain matching procedure:", i, "--------------------------------------------")
      match.names <- names[match.cols[i]] %>% dplyr::pull()

      match <- fastmatch::fmatch(match.names, match.table$comp_name_stand)

      temp.match <- dplyr::bind_cols(names, match.table[match,]) %>%
        dplyr::filter(!is.na(ident)) %>%
        dplyr::mutate(match_type_comp = i)
      table.match.full <- dplyr::bind_rows(table.match.full, temp.match)

      names       <- anti_join(names, table.match.full %>% select(id), by = "id")
      match.table <- anti_join(match.table, table.match.full %>% select(ident), by = "ident")

    }
    table.match.full <- table.match.full %>%
      select(ident, name = no, name_match = comp_name_stand, name_type, match_type_comp,
             match_type_db)
  }

  if ("sub" %in% match.type) {
    # get substring matches ==============================================================
    table.match.sub <- list()
    for (i in 1:length(match.cols)) {
      cat("\rsubstring matching procedure:", i, "----------------------------------------")
      match.table.sub <- dplyr::filter(match.table, match_type_db == i)
      names.sub <- names[match.cols[i]] %>% dplyr::pull()

      table.match.sub[[i]] <- lapply(max(nchar(names.sub)):4, function(x) {
        sub <- stringi::stri_sub(names.sub, 1, x)
        match.table.sub <- match.table.sub %>%
          dplyr::mutate(name_match = stringi::stri_sub(comp_name_stand, 1, x))

        match <- tibble::tibble(name = names$no, name_match = sub) %>%
          dplyr::inner_join(match.table.sub, by = "name_match") %>%
          dplyr::mutate(match_type_comp = i)
      }) %>% dplyr::bind_rows()
    }
    table.match.sub <- dplyr::bind_rows(table.match.sub) %>%
      dplyr::mutate(sim = round(nchar(name_match) / nchar(comp_name_stand), 4)) %>%
      dplyr::arrange(name, dplyr::desc(sim)) %>%
      dplyr::distinct(name, .keep_all = TRUE)
  }

  if ("approx" %in% match.type) {
    # get approximate matches ============================================================
    cat("\rapproximate matching procedure ----------------------------------------------")
    dist.matrix <- stringdist::stringdistmatrix(names$ns, match.table$comp_name_stand, method = "lv")
    min.dist <- sapply(1:nrow(dist.matrix), function(x){which.min(dist.matrix[x, ])})
    table.match.approx <- tibble::tibble(name = names$no, name_stand = names$ns) %>%
      bind_cols(match.table[min.dist, ]) %>%
      dplyr::mutate(sim = stringdist::stringsim(name_stand, comp_name_stand, method = "lv")) %>%
      dplyr::mutate(match_type_comp = 1)
  }

  if (!"full" %in% match.type) table.match.full <- NULL
  if (!"sub" %in% match.type) table.match.sub <- NULL
  if (!"approx" %in% match.type) table.match.approx <- NULL

  return.list <- list(table.match.full, table.match.sub, table.match.approx)
  names(return.list) <- c("full", "sub", "approx")

  return(return.list)
}
