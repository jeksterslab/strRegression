#' Standardized Regression Coefficients
#' as a Function of the Vector of Regression Parameters
#'
#' @details
#' # Dependencies
#' * [sym_of_vech()]
#' * [rmvn_chol()] (test)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric vector.
#'   Vector of parameters
#'   \eqn{\boldsymbol{theta}
#'   =
#'   \{
#'     \boldsymbol{\beta},
#'     \sigma^2,
#'     \mathrm{\vech}
#'     \left( \boldsymbol{\Sigma}_{\mathbf{X}, \mathbf{X}} \right)
#'   \}^{\prime}
#'   }.
#'
#' @returns A numeric vector.
#'
#' @export
#' @family Structure of Regression Functions
#' @keywords strRegression
betastar_of_theta <- function(x) {
  stopifnot(is.vector(x))
  n <- length(x)
  stopifnot(n >= 3)
  p <- floor(0.5 * (sqrt(1 + 8 * n) - 3))
  k <- p + 1
  beta <- x[1:p]
  sigmasq <- x[k]
  sigmacapx <- sym_of_vech(x[(k + 1):n])
  sigmaxsq <- diag(sigmacapx)
  if (any(sigmaxsq <= 0)) {
    sigmax <- NA
  } else {
    sigmax <- sqrt(sigmaxsq)
  }
  sigmaysq <- sigmaysq(
    sigmasq = sigmasq,
    beta = beta,
    sigmacapx = sigmacapx
  )
  if (sigmaysq <= 0) {
    sigmay <- NA
  } else {
    sigmay <- sqrt(sigmaysq)
  }
  if (is.na(sigmax) || is.na(sigmay)) {
    return(
      rep(
        x = NA,
        times = p
      )
    )
  } else {
    return(
      beta * (sigmax / sigmay)
    )
  }
}
