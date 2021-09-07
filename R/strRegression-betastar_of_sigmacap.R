#' Standardized Regression Coefficients
#' as a Function of the Covariance Matrix
#'
#' @details
#' # Dependencies
#' * [rmvn_chol()] (test)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams beta_of_sigmacap
#'
#' @returns A numeric vector.
#'
#' @export
#' @family Structure of Regression Functions
#' @keywords strRegression
betastar_of_sigmacap <- function(x) {
  stopifnot(
    is.matrix(x)
  )
  k <- dim(x)[1]
  stopifnot(
    k == dim(x)[2],
    x == t(x)
  )
  d <- diag(k - 1)
  diag(d) <- sqrt(
    diag(x)[-1] / x[1:1]
  )
  return(
    drop(
      d %*% solve(
        x[2:k, 2:k, drop = FALSE],
        x[2:k, 1, drop = FALSE]
      )
    )
  )
}
