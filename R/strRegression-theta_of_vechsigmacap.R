#' Parameters Assuming Stochastic Regressors
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
#'   Half-vectorization of the covariance matrix of
#'   \eqn{\{y, x_1, \cdots, x_p \}^{\prime}}.
#'
#' @returns A numeric vector.
#'
#' @export
#' @family Structure of Regression Functions
#' @keywords strRegression
theta_of_vechsigmacap <- function(x) {
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
  return(
    c(
      beta,
      sigmasq,
      sigmacapx[lower.tri(sigmacapx, diag = TRUE)]
    )
  )
}
