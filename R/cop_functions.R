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
# PREPARE NAMES ==========================================================================
#' Name Preparation for Company Matching Procedure
#'
#' @description
#' This function takes a character string and prepares a deduplicated list with maximum 24
#' different variations of the name. The different name varieties are stored in the 'ntype'
#'
#' @param names
#' A character string of company names
#' @param adj.regex
#' Adjustment Regex
#'
#' @return
#' A dataframe with different 3 columns containing different name varieties.\cr
#' Col 1 ('n0'):    Original Name of the company.\cr
#' Col 2 ('n1'):    Adjusted Name of the company.\cr
#' Col 3 ('ntype'): Adjustemt type.
#'
#' @export
#'
#' @examples
#' library(tpfuns)
#' copm_prep_names(names = c("BASF GMBH", "BASF AG", "BASF SE (GER)"))
copm_prep_names <-
  function(names, adj.regex = "\\s?holdings?\\s?|\\s?groups?\\s?|\\s?units?$") {

  `%>%` <- magrittr::`%>%`

  # set internal functions ----------------------------------------------------------------
  int_rem_bracket <- function(string) {
    string.adj <- stringi::stri_trim_both(stringi::stri_replace_all_regex(string, "\\s?\\(.*?\\)\\s?", " "))
    string.adj[which(string.adj == "")] <- string[which(string.adj == "")]
    return(string.adj)
  }
  int_rem_adj     <- function(string) {
    string.adj <- stringi::stri_trim_both(stringi::stri_replace_all_regex(string, adj.regex, " "))
    string.adj[which(string.adj == "")] <- string[which(string.adj == "")]
    return(string.adj)
  }
  int_rem_space   <- function(string) {
    string.adj <- stringi::stri_replace_all_fixed(string, " ", "")
    string.adj[which(string.adj == "")] <- string[which(string.adj == "")]
    return(string.adj)
  }
  int_punct_1     <- function(string) {
    tpfuns::top_punct(string, TRUE, TRUE, "none", FALSE)
  }
  int_punct_2     <- function(string) {
    tpfuns::top_punct(string, TRUE, TRUE, "complete", TRUE)
  }
  int_americanize <- function(string) {
    tpfuns::top_term_lookup(terms = string,
                            key.match = tpfuns::table_americanize$uk,
                            key.reassign = tpfuns::table_americanize$us,
                            tokenize = TRUE)
  }
  int_stand_words <- function(string) {
    tpfuns::top_term_lookup(terms = string,
                            key.match = tpfuns::table_word_stand$word,
                            key.reassign = tpfuns::table_word_stand$replace,
                            tokenize = TRUE)
  }

  # first name operations -----------------------------------------------------------------
  cat("\r performing first name operation                                                ")
  table.names <- tibble::tibble(n0 = names) %>%
    dplyr::mutate(
      n1 = int_punct_1(n0),
      n2 = int_rem_bracket(n1),
      n3 = int_rem_adj(n1),
      n4 = int_rem_adj(n2)
      ) %>% tidyr::gather(p1, n1, n1:n4) %>%
    dplyr::distinct(n0, n1, .keep_all = TRUE)

  # second name operations ----------------------------------------------------------------
  cat("\r performing second name operation                                               ")
  table.names <- table.names %>%
    dplyr::mutate(
      n1 = int_punct_2(n1),
      n1 = int_stand_words(n1),
      n1 = int_americanize(n1)
      )

  le.stand <- tpfuns::cop_stand_le(table.names$n1)

  table.names <- table.names %>%
    dplyr::mutate(
      n2 = le.stand$comp_le_stand,
      n3 = le.stand$comp_adj
      ) %>% tidyr::gather(p2, n1, n1:n3) %>%
    dplyr::distinct(n0, n1, .keep_all = TRUE)

  # third name operations -----------------------------------------------------------------
  cat("\r performing third name operation                                                ")
  table.names <- table.names %>%
    dplyr::mutate(n2 = int_rem_space(n1)) %>%
    tidyr::gather(p3, n1, n1:n2) %>%
    dplyr::distinct(n0, n1, .keep_all = TRUE)

  # asign priorities ----------------------------------------------------------------------
  table.names <- table.names %>%
    dplyr::mutate(
      ntype = dplyr::case_when(
        p1 == "n1" & p2 == "n1" & p3 == "n1" ~ 1,
        p1 == "n1" & p2 == "n2" & p3 == "n1" ~ 2,
        p1 == "n1" & p2 == "n3" & p3 == "n1" ~ 3,
        p1 == "n2" & p2 == "n1" & p3 == "n1" ~ 4,
        p1 == "n2" & p2 == "n2" & p3 == "n1" ~ 5,
        p1 == "n2" & p2 == "n3" & p3 == "n1" ~ 6,
        p1 == "n3" & p2 == "n1" & p3 == "n1" ~ 7,
        p1 == "n3" & p2 == "n2" & p3 == "n1" ~ 8,
        p1 == "n3" & p2 == "n3" & p3 == "n1" ~ 9,
        p1 == "n4" & p2 == "n1" & p3 == "n1" ~ 10,
        p1 == "n4" & p2 == "n2" & p3 == "n1" ~ 11,
        p1 == "n4" & p2 == "n3" & p3 == "n1" ~ 12,
        p1 == "n1" & p2 == "n1" & p3 == "n2" ~ 13,
        p1 == "n1" & p2 == "n2" & p3 == "n2" ~ 14,
        p1 == "n1" & p2 == "n3" & p3 == "n2" ~ 15,
        p1 == "n2" & p2 == "n1" & p3 == "n2" ~ 16,
        p1 == "n2" & p2 == "n2" & p3 == "n2" ~ 17,
        p1 == "n2" & p2 == "n3" & p3 == "n2" ~ 18,
        p1 == "n3" & p2 == "n1" & p3 == "n2" ~ 19,
        p1 == "n3" & p2 == "n2" & p3 == "n2" ~ 20,
        p1 == "n3" & p2 == "n3" & p3 == "n2" ~ 21,
        p1 == "n4" & p2 == "n1" & p3 == "n2" ~ 22,
        p1 == "n4" & p2 == "n2" & p3 == "n2" ~ 23,
        p1 == "n4" & p2 == "n3" & p3 == "n2" ~ 24
      )
    ) %>% dplyr::arrange(ntype) %>%
    dplyr::select(-p1, -p2, -p3)
  return(table.names)
}

# PREPARE DATABASE NAMES =================================================================
#' Preparation of Database names for Company Matching Procedure
#'
#' @param match.table
#' A table with company names and identifiers
#' @param col.name
#' A chracter string of the name column
#' @param col.ident
#' A character string of the identifier columns
#' @param adj.regex
#' Adjustemtn REgex
#'
#' @return
#' A dataframe suitable for the matching algorithm
#'
#' @export
#'
#' @examples
#' library(tpfuns)
#' table <- tibble::tibble(id = c(1,2,3), firm = c("BASF GMBH", "BASF AG", "BASF SE (GER)"))
#' copm_prep_db(table, "firm", "id")
copm_prep_db <- function(match.table, col.name = NULL, col.ident = NULL,
                              adj.regex = "default") {
  `%>%` <- magrittr::`%>%`

  if (is.null(col.name))  stop("name column must not be NULL!")
  if (is.null(col.ident)) stop("identifier columns must not be NULL!")
  if (!col.name %in% colnames(match.table)) stop("wrong name column")
  if (!all(col.ident %in% colnames(match.table))) stop("wrong identifier columns")

  # set internal functions ---------------------------------------------------------------
  regex <- c(
    "(?i)\\sdead\\b.*",
    "(?i)\\sdelisted\\b.*",
    "(?i)\\smerger\\b.*",
    "(?i)\\ssusp.+supended\\b.*",
    "(?i)\\ssuspended\\b.*",
    "(?i)\\sexpired\\b.*"
  )
  regex <- paste0(regex, collapse = "|")
  int_rem_db <- function(string) {
    string.adj <- stringi::stri_trim_both(stringi::stri_replace_all_regex(string, regex, " "))
    string.adj[which(string.adj == "")] <- string[which(string.adj == "")]
    return(string.adj)
  }

  # rename columns -----------------------------------------------------------------------
  colnames(match.table) <-
    stringi::stri_replace_all_regex(colnames(match.table), paste0("^", col.name, "$"), "n0")

  # prepare identifier column ------------------------------------------------------------
  ident <- character(nrow(match.table))
  for (i in 1:length(col.ident)) {
    ident <- paste0(ident, match.table[[col.ident[i]]])
  }
  match.table$ident <- ident

  # remove database additions ------------------------------------------------------------
  match.table$n0 <- int_rem_db(match.table$n0)

  # check duplicates ---------------------------------------------------------------------
  check.ident <- duplicated(match.table[["ident"]])
  check.name  <- duplicated(match.table[[col.name]])

  if (any(check.name)) {
    warning("matching table contains duplicated names - deduplicating and continuing anyways ...")
    match.table <- dplyr::distinct_(match.table, no, .keep_all = TRUE)
  }
  if (any(check.ident)) {
    warning("matching table contains duplicated identifiers - continuing anyways ...")
    }

  table.names <- copm_prep_names(names = match.table$n0)
  match.table <- dplyr::left_join(match.table, table.names, by = "n0") %>%
    dplyr::arrange(ntype) %>%
    dplyr::distinct(n0, n1, .keep_all = TRUE) %>%
    dplyr::select(n0, n1, ntype, dplyr::everything())

  return(match.table)
}

# MATCH COMPANY NAMES ====================================================================
#' Match company names against Databses
#'
#' @param names
#' A charcter string of company names
#' @param match.table
#' A table prepared by copm_prep_names
#' @param match.type
#' Any combination of c("full", "sub", "approx")
#'
#' @return
#' A list with matches
#' @export
#'
#' @examples
#' library(tpfuns)
#' table <- tibble::tibble(id = c(1,2,3), firm = c("BASF GMBH", "BASF AG", "BASF SE (GER)"))
#' match.table <- copm_prep_db(table, "firm", "id")
#' names <- c("BASF GMBH", "BASF AG", "Siemens AG")
#' copm_match(names, match.table, "full")
copm_match <- function(names, match.table = NULL, match.type = c("full", "sub", "approx"),
                       nthreads = 1) {
  `%>%` <- magrittr::`%>%`
  pbapply::pboptions(type = "timer", char = "=", txt.width = 90)
  if (!all(match.type %in%  c("full", "sub", "approx")))
    stop("wrong specification of matching type")


  # prepare names ------------------------------------------------------------------------
  table.names <- copm_prep_names(names, adj.regex = "default")

  # full matching ------------------------------------------------------------------------
  if ("full" %in% match.type) {
    table.match.full <- list()
    pb <- pbapply::startpb(min(unique(table.names$ntype)), max(unique(table.names$ntype)))

    for (i in sort(unique(table.names$ntype))) {
      pbapply::setpb(pb, i)
      n0 <- table.names %>% dplyr::filter(ntype == i) %>% dplyr::select(n0) %>% dplyr::pull()
      n1 <- table.names %>% dplyr::filter(ntype == i) %>% dplyr::select(n1) %>% dplyr::pull()

      match <- fastmatch::fmatch(n1, match.table$n1)
      if (!all(is.na(match)))  {

        table.match.full[[i]] <-
          tibble::tibble(name_co = n0, name_match = n1) %>%
          dplyr::bind_cols(match.table[match,]) %>%
          dplyr::mutate(ntype_co = i) %>%
          dplyr::select(name_co, name_db = n0, name_match, ntype_co,
                        ntype_db = ntype, dplyr::everything(), -n1) %>%
          dplyr::filter(!is.na(ident))

        join.table  <- table.match.full[[i]] %>% dplyr::select(name_co)
        table.names <- dplyr::anti_join(table.names, join.table, by = c("n0" = "name_co"))

        join.table  <- table.match.full[[i]] %>% dplyr::select(ident)
        match.table <- dplyr::anti_join(match.table, join.table, by = "ident")
      }
    }
    table.match.full <- dplyr::bind_rows(table.match.full)
  }
  # substring matching -------------------------------------------------------------------
  if ("sub" %in% match.type) {
    table.match.sub <- list()
    pb <- pbapply::startpb(min(unique(table.names$ntype)), max(unique(table.names$ntype)))

    for (i in sort(unique(table.names$ntype))) {
      pbapply::setpb(pb, i)
      match.table.sub <- dplyr::filter(match.table, ntype == i)
      n0 <- table.names %>% dplyr::filter(ntype == i) %>% dplyr::select(n0) %>% dplyr::pull()
      n1 <- table.names %>% dplyr::filter(ntype == i) %>% dplyr::select(n1) %>% dplyr::pull()

      chars <- max(nchar(n1)):4
      chars <- chars[c(seq(1, length(chars), 2), length(chars))]

      table.match.sub[[i]] <- lapply(chars, function(x) {

        sub <- stringi::stri_sub(n1, 1, x)
        match.table.sub <- dplyr::mutate(match.table.sub, name_match = stringi::stri_sub(n1, 1, x))

        match <- tibble::tibble(name_co = n0, name_co_stand = n1, name_match = sub) %>%
          dplyr::inner_join(match.table.sub, by = "name_match") %>%
          dplyr::mutate(sim = round(nchar(name_match) / nchar(name_co_stand), 4)) %>%
          dplyr::arrange(name_co, dplyr::desc(sim)) %>%
          dplyr::distinct(name_co, .keep_all = TRUE)


      }) %>% dplyr::bind_rows() %>%
        dplyr::arrange(name_co, dplyr::desc(sim)) %>%
        dplyr::distinct(name_co, .keep_all = TRUE) %>%
        dplyr::mutate(ntype_co = i) %>%
        dplyr::select(name_co, name_co_stand, name_match, name_db = n0, name_db_stand = n1,
                      ntype_co, ntype_db = ntype, dplyr::everything())

    }
    table.match.sub <- table.match.sub %>%
      dplyr::bind_rows() %>%
      dplyr::arrange(name_co, dplyr::desc(sim)) %>%
      dplyr::distinct(name_co, .keep_all = TRUE)
  }

  # approximate matching -----------------------------------------------------------------
  if ("approx" %in% match.type) {

    if (nthreads == 1) {
      cl <- NULL
    } else {
      cl <- parallel::makeCluster(nthreads)
      doSNOW::registerDoSNOW(cl)
    }

    match.table.approx <- dplyr::filter(match.table, ntype == 1)
    n0 <- table.names %>% dplyr::filter(ntype == 1) %>% dplyr::select(n0) %>% dplyr::pull()
    n1 <- table.names %>% dplyr::filter(ntype == 1) %>% dplyr::select(n1) %>% dplyr::pull()

    table.match.approx <- pbapply::pblapply(1:length(n1), function(x){
      dist <- which.min(
        stringdist::stringdist(table.names$n1[x], match.table.approx$n1, method = "lv")
        )

      table.match.approx <- tibble::tibble(name_co = n0[x], name_co_stand = n1[x]) %>%
        dplyr::bind_cols(match.table.approx[dist,]) %>%
        dplyr::mutate(ntype_co = 1) %>%
        dplyr::select(name_co, name_co_stand, name_db = n0, name_db_stand = n1,
                      ntype_co, ntype_db = ntype, dplyr::everything())

    }, cl = cl) %>% dplyr::bind_rows() %>%
      dplyr::mutate(sim = stringdist::stringsim(name_co_stand, name_db_stand, method = "lv")) %>%
      dplyr::mutate(sim = round(sim, 4))

    if (nthreads != 1) parallel::stopCluster(cl)
  }

  # prepare output -----------------------------------------------------------------------
  if (!"full" %in% match.type)   table.match.full   <- NULL
  if (!"sub" %in% match.type)    table.match.sub    <- NULL
  if (!"approx" %in% match.type) table.match.approx <- NULL

  return.list <- list(table.match.full, table.match.sub, table.match.approx)
  names(return.list) <- c("full", "sub", "approx")
  return(return.list)

}
