#' Standardized Parameters Assuming Stochastic Regressors
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
#' @returns A numeric vector.
#'
#' @export
#' @family Structure of Regression Functions
#' @keywords strRegression
thetastar_of_vechsigmacap <- function(x) {
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
  p <- k - 1
  rhocap <- stats::cov2cor(sigmacap)
  rhocapx <- rhocap[2:k, 2:k, drop = FALSE]
  betastar <- drop(
    solve(
      rhocap[2:k, 2:k, drop = FALSE],
      rhocap[2:k, 1, drop = FALSE]
    )
  )
  sigmay <- sqrt(sigmacap[1, 1])
  sigmax <- sqrt(diag(sigmacap[2:k, 2:k, drop = FALSE]))
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
