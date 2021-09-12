#' Parameters Assuming Stochastic Regressors
#' as a Function of the Mean Vector and the Covariance Matrix
#'
#' @details
#' # Dependencies
#' * [rmvn_chol()] (test)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param sigmacap Numeric matrix.
#'   Covariance matrix
#'   \eqn{\boldsymbol{\Sigma}}
#'   of
#'   \eqn{\{y, x_1, \cdots, x_p \}^{\prime}}.
#' @param mu Numeric vector
#'   Mean vector
#'   \eqn{\boldsymbol{\mu}}
#'   of
#'   \eqn{\{y, x_1, \cdots, x_p \}^{\prime}}.
#'
#' @returns A numeric vector.
#'
#' @export
#' @family Structure of Regression Functions
#' @keywords strRegression
theta <- function(sigmacap,
                  mu = NULL) {
  stopifnot(
    is.matrix(sigmacap)
  )
  k <- dim(sigmacap)[1]
  p <- k - 1
  stopifnot(
    k == dim(sigmacap)[2],
    sigmacap == t(sigmacap)
  )
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
  if (is.null(mu)) {
    return(cov_str)
  } else {
    mean_str <- c(
      mu[1] - crossprod(mu[-1], beta),
      mu[-1]
    )
    return(
      c(
        cov_str,
        mean_str
      )
    )
  }
}
