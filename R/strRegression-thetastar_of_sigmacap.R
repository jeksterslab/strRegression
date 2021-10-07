#' Standardized Parameters Assuming Stochastic Regressors
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
thetastar_of_sigmacap <- function(x) {
  stopifnot(
    is.matrix(x)
  )
  k <- dim(x)[1]
  p <- k - 1
  stopifnot(
    k == dim(x)[2],
    x == t(x)
  )
  rhocap <- stats::cov2cor(x)
  rhocapx <- rhocap[2:k, 2:k, drop = FALSE]
  betastar <- drop(
    solve(
      rhocap[2:k, 2:k, drop = FALSE],
      rhocap[2:k, 1, drop = FALSE]
    )
  )
  sigmay <- sqrt(x[1, 1])
  sigmax <- sqrt(diag(x[2:k, 2:k, drop = FALSE]))
  if (p > 1) {
    vechsrhocapx <- rhocapx[lower.tri(rhocapx, diag = FALSE)]
    output <- c(
      betastar,
      sigmay,
      sigmax,
      vechsrhocapx
    )
    names(output) <- c(
      paste0("betastar", seq_len(p)),
      "sigmay",
      paste0("sigmax", seq_len(p)),
      vechsnames(paste0("x", seq_len(p)))
    )
    return(output)
  } else {
    output <- c(
      betastar,
      sigmay,
      sigmax
    )
    names(output) <- c(
      paste0("betastar", seq_len(p)),
      "sigmay",
      paste0("sigmax", seq_len(p))
    )
    return(output)
  }
}
