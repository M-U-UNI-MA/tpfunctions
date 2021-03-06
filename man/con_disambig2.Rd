% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/con_functions.R
\name{con_disambig2}
\alias{con_disambig2}
\title{Disambiguate Concepts 2}
\usage{
con_disambig2(df, col.term, col.conid)
}
\arguments{
\item{df}{A dataframe containing terms and corresponding concept ids}

\item{col.term}{The name of the column containing the terms as character string}

\item{col.conid}{The name of the column containing the concept ids as character string}
}
\value{
A list containing (a) the dataframe without ambigious concepts and (b) a dataframe with the
removed terms
}
\description{
#' @description
This function checks whether concepts share terms and keeps only the common terms.\cr
IMPORTANT NOTE: Concepts should be deduplicated before disambiguated!!
}
\examples{
library(tpfuns)
test <- tibble::tibble(
con_id = c( 1,   1,   1,   2,   2,   3,   3,   4,   4 ,  5,   5,   5,   6,   6 ),
term   = c("a", "c", "b", "a", "b", "d", "e", "e", "f", "e", "f", "g", "b", "a"))

con_disambig2(test, "term", "con_id")

}
