VecIn <- function(a, b){
  which(
    Reduce('+', lapply(seq_along(y <- lapply(b, '==', a)), function(x){
      y[[x]][x:(length(a) - length(b) + x)]
    }
    )
    ) == length(b)
  )
}
IndVecIn <- function(matrow, seq.end) {
  unlist(lapply(VecIn(matrow, 1:seq.end), function(x) x + 0:(seq.end - 1)))
}

# f <- pbapply::pblapply(sequences, function(i){
#   list.idx <- VecIn(table.search$word_id, i)
#   list.idx <- unlist(lapply(list.idx, function(x) x +  0:(length(i) - 1)))
#
#   x <- matrix(tokids[list.idx], ncol = length(i), byrow = TRUE)
#   x <- x - matrix(rep(1:length(i), nrow(x)), ncol = length(i), nrow = nrow(x), byrow = TRUE)
#   x <- x - matrix(rep(x[, 1], length(i)), ncol = length(i), nrow = nrow(x))
#   x <- which(apply(x, 1, sum) != 0)
#   if (!is_empty(x)) {
#     x <- unlist(lapply(x, function(a) a +  0:(length(i) - 1)))
#     list.idx <- list.idx[-x]
#   }
#   return(list.idx)
# })


