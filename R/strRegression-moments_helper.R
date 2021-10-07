#' Helper Function to Convert a Vector of Moments
#' to the Mean Vector and Covariance Matrix
#'
#' @param x Numeric vector.
#'   Vector of parameters
#'   \eqn{
#'     \boldsymbol{\theta}
#'     =
#'     \left\{
#'       \boldsymbol{\mu} ,
#'       \mathrm{vech} \left( \boldsymbol{\Sigma} \right)
#'     \right\}^{\prime}
#'   }.
#'
#' @returns A list with elements `mu` and `sigmacap`.
#'
#' @export
#' @family Structure of Regression Functions
#' @keywords strRegression
moments_helper <- function(x) {
  stopifnot(is.vector(x))
  q <- length(x)
  stopifnot(
    q >= 5
  )
  k <- 0.5 * (sqrt(8 * q + 9) - 3)
  sigmacap <- matrix(
    data = 0,
    nrow = k,
    ncol = k
  )
  if (nrow(sigmacap) != k) {
    stop("Length of \"x\" is not valid.")
  }
  sigmacap[lower.tri(sigmacap, diag = TRUE)] <- x[(k + 1):q]
  sigmacap[upper.tri(sigmacap)] <- t(sigmacap)[upper.tri(sigmacap)]
  mu <- x[1:k]
  list(
    mu = mu,
    sigmacap = sigmacap
  )
}
