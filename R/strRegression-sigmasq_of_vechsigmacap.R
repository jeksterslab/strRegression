#' Error Variance
#' as a Function of the Half-Vectorization
#' of the Covariance Matrix
#'
#' @details
#' # Dependencies
#' * [rmvn_chol()] (test)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric vector.
#'   Half-vectorization of the covariance matrix
#'   \eqn{\boldsymbol{\Sigma}}
#'   of
#'   \eqn{\left\{y, x_1, \dots, x_p \right\}^{\prime}}.
#'
#' @returns A numeric vector of length 1.
#'
#' @export
#' @family Structure of Regression Functions
#' @keywords strRegression
sigmasq_of_vechsigmacap <- function(x) {
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
  sigmaysq <- sigmacap[1, 1]
  sigmayx <- sigmacap[1, 2:k]
  sigmacapx <- sigmacap[2:k, 2:k, drop = FALSE]
  beta <- drop(
    solve(
      sigmacapx,
      sigmayx
    )
  )
  return(
    drop(
      sigmaysq - (
        tcrossprod(beta, sigmacapx) %*% beta
      )
    )
  )
}
