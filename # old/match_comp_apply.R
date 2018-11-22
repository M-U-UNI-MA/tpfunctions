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
cop_match_comp <- function(string, match.table = NULL) {

  if (is.null(match.table)) stop("function needs matching table, see documentation")
  `%>%` <- magrittr::`%>%`
  name.match <- match.table
  match.names <- name.match$comp_name_stand

  pbapply::pboptions(type = "timer", char = "=", txt.width = 90)
  match <- pbapply::pblapply(string, function(x) {
    string.adj <- tpfuns::top_rem_punct(tpfuns::top_stand_punct(x))
    string.adj <- tpfuns::cop_repl_words(string.adj)
    string.adj <- tpfuns::top_americanize(string.adj)

    match <- fastmatch::fmatch(string.adj, match.names)
    match.temp <- tibble::tibble(
      comp_name       = x,
      ident           = name.match$ident[match],
      ident_type      = name.match$ident_type[match],
      db_type         = name.match$db_type[match],
      match_type_db   = name.match$name_id[match],
      match_type_comp = 1
    )


    if (is.na(match)) {
      string.adj.le <- tpfuns::cop_rem_le(string.adj)
      match <- fastmatch::fmatch(string.adj.le, match.names)
      match.temp <- tibble::tibble(
        comp_name       = x,
        ident           = name.match$ident[match],
        ident_type      = name.match$ident_type[match],
        db_type         = name.match$db_type[match],
        match_type_db   = name.match$name_id[match],
        match_type_comp = 2
      )

      if (is.na(match)) {
        string.adj.space <- stringi::stri_replace_all_fixed(string.adj, " ", "")
        match <- fastmatch::fmatch(string.adj.space, match.names)
        match.temp <- tibble::tibble(
          comp_name       = x,
          ident           = name.match$ident[match],
          ident_type      = name.match$ident_type[match],
          db_type         = name.match$db_type[match],
          match_type_db   = name.match$name_id[match],
          match_type_comp = 3
        )

        if (is.na(match)) {
          string.adj.le.space <- stringi::stri_replace_all_fixed(string.adj.le, " ", "")
          match <- fastmatch::fmatch(string.adj.le.space, match.names)
          match.temp <- tibble::tibble(
            comp_name       = x,
            ident           = name.match$ident[match],
            ident_type      = name.match$ident_type[match],
            db_type         = name.match$db_type[match],
            match_type_db   = name.match$name_id[match],
            match_type_comp = 4
          )

          if (is.na(match)) {
            match.temp <- tibble::tibble(
              comp_name       = x,
              ident           = NA,
              ident_type      = NA,
              db_type         = NA,
              match_type_db   = NA,
              match_type_comp = NA
            )
          }
        }
      }
    }
    return(match.temp)
  }) %>% dplyr::bind_rows()

  return(match)
}
