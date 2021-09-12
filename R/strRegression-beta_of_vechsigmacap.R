beta_of_vechsigmacap <- function(x) {
  stopifnot(is.vector(x))
  k <- 0.5 * (sqrt(1 + 8 * length(x)) - 1)
  sigmacap <- matrix(
    data = 0,
    nrow = k,
    ncol = k
  )
  if (nrow(sigmacap) != k) {
    stop("Length of \"x\" is not valid.")
  }
  sigmacap[lower.tri(sigmacap, diag = TRUE)] <- x
  sigmacap[upper.tri(sigmacap)] <- t(sigmacap)[upper.tri(sigmacap)]
  return(
    drop(
      solve(
        sigmacap[2:k, 2:k, drop = FALSE],
        sigmacap[2:k, 1, drop = FALSE]
      )
    )
  )
}
