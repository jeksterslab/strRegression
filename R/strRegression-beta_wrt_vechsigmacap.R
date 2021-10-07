#' Jacobian Matrix of Beta
#' with respect to the Half-Vectorization
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
beta_wrt_vechsigmacap <- function(x) {
  stopifnot(
    is.vector(x)
  )
  sigmacap <- sym_of_vech(x)
  k <- dim(sigmacap)[1]
  p <- k - 1
  s <- p * (p + 1) / 2
  sigmacapx <- sigmacap[2:k, 2:k, drop = FALSE]
  beta <- drop(
    solve(
      sigmacap[2:k, 2:k, drop = FALSE],
      sigmacap[2:k, 1, drop = FALSE]
    )
  )
  betasq <- tcrossprod(beta)
  qcap <- chol2inv(chol(sigmacapx))
  dcap <- dcap(dim(sigmacapx)[1])
  tdcap <- t(dcap)
  beta_wrt_sigmaysq <- rep(x = 0, times = p)
  beta_wrt_sigmayx <- qcap
  beta_wrt_sigmacapx <- t(
    tdcap %*% (
      kronecker(
        -1 * beta,
        qcap
      )
    )
  )
  output <- cbind(
    beta_wrt_sigmaysq,
    beta_wrt_sigmayx,
    beta_wrt_sigmacapx
  )
  return(output)
}
