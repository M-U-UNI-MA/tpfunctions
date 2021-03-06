% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/con_functions.R
\name{con_subset}
\alias{con_subset}
\title{Remove Subset Concepts}
\usage{
con_subset(df, col.term, col.conid)
}
\arguments{
\item{df}{A dataframe containing terms and corresponding concept ids}

\item{col.term}{The name of the column containing the terms as character string}

\item{col.conid}{The name of the column containing the concept ids as character string}
}
\value{
A list containing (a) the dataframe without removed concepts (b) a dataframe iwth the
removed concepts and (c) the adjusment matrix
}
\description{
Remove Subset Concepts
}
\examples{
library(tpfuns)
test <- tibble::tibble(
con_id = c( 1,   1,   1,   2,   2,   3,   3,   4,   4 ,  5,   5,   5,   6,   6 ),
term   = c("a", "c", "b", "a", "b", "d", "e", "e", "f", "e", "f", "g", "b", "a"))

con_subset(test, "term", "con_id")
}
