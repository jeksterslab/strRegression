#' Parameters Assuming Stochastic Regressors
#' as a Function of the Covariance Matrix
#'
#' @details
#' # Dependencies
#' * [rmvn_chol()] (test)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric matrix.
#'   Covariance matrix
#'   \eqn{\boldsymbol{\Sigma}}
#'   of
#'   \eqn{\{y, x_1, \cdots, x_p \}^{\prime}}.
#'
#' @returns A numeric vector.
#'
#' @export
#' @family Structure of Regression Functions
#' @keywords strRegression
theta_of_sigmacap <- function(x) {
  stopifnot(
    is.matrix(x)
  )
  k <- dim(x)[1]
  p <- k - 1
  stopifnot(
    k == dim(x)[2],
    x == t(x)
  )
  beta <- drop(
    solve(
      x[2:k, 2:k, drop = FALSE],
      x[2:k, 1, drop = FALSE]
    )
  )
  sigmacapx <- x[2:k, 2:k, drop = FALSE]
  sigmaysq <- x[1, 1]
  sigmasq <- drop(
    sigmaysq - (
      tcrossprod(beta, sigmacapx) %*% beta
    )
  )
  return(
    c(
      beta,
      sigmasq,
      sigmacapx[lower.tri(sigmacapx, diag = TRUE)]
    )
  )
}
