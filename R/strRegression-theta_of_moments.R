#' Parameters Assuming Stochastic Regressors
#' as a Function of the Mean Vector and the Covariance Matrix
#'
#' @details
#' # Dependencies
#' * [rmvn_chol()] (test)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric vector.
#'   Vector of moments
#'   \eqn{
#'     \{
#'       \boldsymbol{\mu}
#'       \mathrm{vech} \left( \boldsymbol{\Sigma} \right)
#'     \}^{\prime}
#'   }.
#'
#' @returns A numeric vector.
#'
#' @export
#' @family Structure of Regression Functions
#' @keywords strRegression
theta_of_moments <- function(x) {
  moments <- moments_helper(x)
  mu <- moments$mu
  sigmacap <- moments$sigmacap
  k <- dim(sigmacap)[1]
  beta <- drop(
    solve(
      sigmacap[2:k, 2:k, drop = FALSE],
      sigmacap[2:k, 1, drop = FALSE]
    )
  )
  sigmacapx <- sigmacap[2:k, 2:k, drop = FALSE]
  sigmaysq <- sigmacap[1, 1]
  sigmasq <- drop(
    sigmaysq - (
      tcrossprod(beta, sigmacapx) %*% beta
    )
  )
  cov_str <- c(
    beta,
    sigmasq,
    sigmacapx[lower.tri(sigmacapx, diag = TRUE)]
  )
  mean_str <- c(
    mu[1] - crossprod(mu[-1], beta),
    mu[-1]
  )
  c(
    cov_str,
    mean_str
  )
}
