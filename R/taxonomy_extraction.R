#' Extract Terms from Taxonomy Files
#'
#' @description
#' This funnction extracts terms from taxonomy xml files.\cr
#' (Tested on US GAAP, UK GAAP and IFRS taxonomies)
#'
#' @param lab
#' The xml file of the taxonomy, mostly label with lab_...
#'
#' @return
#' A dataframe with three columns:\cr
#' label: The labels of the taxonomy files (used as an identifier)\cr
#' term:  The terms of the taxonomy files
#' role:  An identification of the type of the term
#'
#' @export
tax_get_term_us_gaap <- function(lab.file, pb = FALSE) {
  `%>%` <- magrittr::`%>%`

  taxon <- xml2::read_xml(lab.file) %>% xml2::xml_children() %>% xml2::xml_children()

  text <- tibble::tibble(term = unlist(lapply(taxon, try(xml2::xml_text))))

  role <- tibble::tibble(role = unlist(lapply(taxon, function(x)
    try(xml2::xml_attrs(x)[["role"]], silent = TRUE))))

  label <- tibble::tibble(label = unlist(lapply(taxon, function(x)
    try(xml2::xml_attrs(x)[["label"]], silent = TRUE))))

  terms <- dplyr::bind_cols(label, text, role) %>%
    dplyr::filter(stringi::stri_detect_fixed(role, "http://www.xbrl.org/2003/role")) %>%
    dplyr::mutate(role = basename(role))
}

#' Extract Terms from Taxonomy Files
#'
#' @description
#' This funnction extracts terms from taxonomy xml files.\cr
#' (Tested on US GAAP, UK GAAP and IFRS taxonomies)
#'
#' @param lab
#' The xml file of the taxonomy, mostly label with lab_...
#'
#' @return
#' A dataframe with three columns:\cr
#' label: The labels of the taxonomy files (used as an identifier)\cr
#' term:  The terms of the taxonomy files
#' role:  An identification of the type of the term
#'
#' @export
tax_get_term_ifrs <- function(lab.file, pb = FALSE) {
  `%>%` <- magrittr::`%>%`

  taxon <- xml2::read_xml(lab.file) %>% xml2::xml_children() %>% xml2::xml_children()

  text <- tibble::tibble(term = unlist(lapply(taxon, try(xml2::xml_text))))

  role <- tibble::tibble(role = unlist(lapply(taxon, function(x)
    try(xml2::xml_attrs(x)[["role"]], silent = TRUE))))

  label <- tibble::tibble(label = unlist(lapply(taxon, function(x)
    try(xml2::xml_attrs(x)[["id"]], silent = TRUE))))

  terms <- dplyr::bind_cols(label, text, role) %>%
    dplyr::filter(stringi::stri_detect_fixed(role, "http://www.xbrl.org/2003/role")) %>%
    dplyr::mutate(role = basename(role))
}

#' Extract Definitions from Taxonomy Files
#'
#' @description
#' This funnction extracts definitions from taxonomy xml files.\cr
#' (Tested on US GAAP, UK GAAP and IFRS taxonomies)
#'
#' @param doc.file
#' The xml file of the taxonomy, mostly label with doc_...
#' @return
#' A dataframe with two columns:\cr
#' label: The labels of the taxonomy files (used as an identifier)\cr
#' def:   The definitions of the taxonomy files
#' @export
tax_get_def_us_gaap <- function(doc.file) {
  `%>%` <- magrittr::`%>%`
  taxon <- xml2::read_xml(doc.file) %>%
    xml2::xml_children() %>%
    xml2::xml_children()

  def  <- tibble::tibble(def   = unlist(lapply(taxon, try(xml2::xml_text))))
  label <- tibble::tibble(label = unlist(lapply(taxon, function(x)
    try(xml2::xml_attrs(x)[["label"]], silent = TRUE))))

  defs <- dplyr::bind_cols(label, def) %>%
    dplyr::filter(!def == "")
}

#' Extract Definitions from Taxonomy Files
#'
#' @description
#' This funnction extracts definitions from taxonomy xml files.\cr
#' (Tested on US GAAP, UK GAAP and IFRS taxonomies)
#'
#' @param doc.file
#' The xml file of the taxonomy, mostly label with doc_...
#' @return
#' A dataframe with two columns:\cr
#' label: The labels of the taxonomy files (used as an identifier)\cr
#' def:   The definitions of the taxonomy files
#' @export
tax_get_def_ifrs <- function(doc.file) {
  `%>%` <- magrittr::`%>%`
  taxon <- xml2::read_xml(doc.file) %>%
    xml2::xml_children() %>%
    xml2::xml_children()

  def  <- tibble::tibble(def   = unlist(lapply(taxon, try(xml2::xml_text))))
  label <- tibble::tibble(label = unlist(lapply(taxon, function(x)
    try(xml2::xml_attrs(x)[["id"]], silent = TRUE))))

  defs <- dplyr::bind_cols(label, def) %>%
    dplyr::filter(!def == "")
}

#' Extract Terms and Definitions from Taxonomy Files
#'
#' @description
#' This funnction conbines the tax_get_term() and tax_get_def() functions\cr
#'
#' @param lab.file
#' The xml file of the taxonomy, mostly label with lab_...
#' @param doc.file
#' The xml file of the taxonomy, mostly label with doc_...
#' @return
#' A dataframe with four columns:\cr
#' label: The labels of the taxonomy files (used as an identifier)\cr
#' term:  The terms of the taxonomy files
#' role:  An identification of the type of the term
#' def:   The definitions of the taxonomy files
#' @export
tax_get_term_def <- function(lab.file, doc.file) {
  term <- tax_get_term_us_gaap(lab.file)
  def  <- tax_get_def_us_gaap(doc.file)
  term <- left_join(term, def, by = "label")
}

#' Extract References from Taxonomy Files
#'
#' @param ref.file
#' The xml file of the taxonomy, mostly label with ref_...
#' @return
#' A dataframe with several reference columns and one identification column used for
#' reconsilation to the terms and definition files from the functions tax_get_term()
#' and tax_get_def()
#' @export
tax_get_ref_us_gaap <- function(ref.file) {
  taxon <- xml2::read_xml(ref.file) %>%
    xml2::xml_children() %>%
    xml2::xml_children()

  from <- tibble::tibble(from = unlist(lapply(taxon, function(x)
    try(xml2::xml_attrs(x)[["from"]], silent = TRUE))))

  to <- tibble::tibble(to = unlist(lapply(taxon, function(x)
    try(xml2::xml_attrs(x)[["to"]], silent = TRUE))))

  from_to <- dplyr::bind_cols(from, to) %>%
    dplyr::filter(!stringi::stri_detect_regex(from, "^Error"))

  ref <-  lapply(taxon, function(x) {
    ref        <- try(xml2::xml_text(xml2::xml_children(x)))
    names(ref) <- try(xml2::xml_name(xml2::xml_children(x)))
    ref <- dplyr::as_tibble(t(as.matrix(ref)))
    to <- try(xml2::xml_attrs(x)[["label"]], silent = TRUE)
    if (stringi::stri_detect_regex(to, "ref")) ref$to <- to
    return(ref)
  })

  ref <- ref %>% bind_rows() %>% filter(!is.na(to))

  table <- left_join(from_to, ref, by = "to") %>%
    rename(label = from, reference = to) %>%
    mutate(label = stringi::stri_replace_all_fixed(label, "us-gaap_", "lab_"))
}


#' Level Determination of Strings - Substrings
#'
#' @param string
#' A character string containing terms
#'
#' @return
#' A dataframe with two columns:\cr
#' term: the terms of the original string
#' level: the level assignement
#'
#' @export
tax_levels <- function(string) {
  if (isTRUE(any(duplicated(string))))
    stop("string has duplicated entries")

  string <- sort(string)
  regex <- stringi::stri_replace_all_regex(string, "([[:punct:]])", "\\\\$1")
  regex <- paste0("^", regex, "\\b")


  table <-tibble::tibble(term = string, level = 1, regex = regex) %>%
    dplyr::mutate(start = stringi::stri_sub(term, 1, 1)) %>%
    dplyr::mutate(id = group_indices(., start))

  table.split <- split(table, table$id)

  table <- pbapply::pblapply(table.split, function(x) {
    if (nrow(x) > 1) {
      for (i in 1:(nrow(x) - 1)) {
        detect.false  <- rep(FALSE, i)
        detect.search <-stri_detect_regex(x$term[(i + 1):nrow(x)], x$regex[i])
        detect.all    <- c(detect.false, detect.search)

        x <- x %>%
          dplyr::mutate(detect = detect.all) %>%
          dplyr::mutate(level = dplyr::if_else(detect == TRUE, level + 1, level)) %>%
          dplyr::select(-detect)
      }
    }

    return(x)

  }) %>% dplyr::bind_rows() %>% dplyr::select(term, level)

  return(table)
}


