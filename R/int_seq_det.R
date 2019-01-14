#' Detect Integer Sequence in Vector
#'
#' @param vec
#' An integer vector
#' @param seq
#' A sequence of integer values to search in 'vec'
#' @param type
#' Which algorithm to employ?
#'
#' @return
#' An integer vector with the indices of the sequence
#' @export
#'
#' @examples
#' library(tpfuns)
#' seq_det(c(1,2,3,5,4,1), c(5,4,1), type = 1)
#' seq_det(c(1,2,3,5,4,1), c(5,4,1), type = 2)
#' seq_det(c(1,2,3,5,4,1), c(5,4,1), type = 3)
seq_det <- function(vec, seq, type = c("jaap1", "jaap2", "frank", "cpp")) {

  if (type == "jaap1" | type == 1) {
    l <- length(seq);
    w <- which(rowSums(mapply('==', data.table::shift(vec, type = 'lead', n = 0:(length(seq) - 1)), seq) ) == length(seq));
    out <- rep(w, each = l) + 0:(l-1)
    return(out)
  }

  if (type == "jaap2" | type == 2) {
    l <- length(seq);
    w <- which(Reduce("+", Map('==', data.table::shift(vec, type = 'lead', n = 0:(length(seq) - 1)), seq)) == length(seq));
    out <- rep(w, each = l) + 0:(l-1)
    return(out)
  }

  if (type == "frank" | type == 3) {
    l <- length(seq);
    w = seq_along(vec);
    for (i in seq_along(seq)) w = w[vec[w+i-1L] == seq[i]];
    out <- rep(w, each = l) + 0:(l-1)
    return(out)
  }



}








