#' R-Squared as a Function of Regression Parameters
#'
#' @details
#' # Dependencies
#' * [rmvn_chol()] (test)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param sigmayx Numeric vector.
#'   Covariances between the regressand and regressor variables
#'   \eqn{
#'     \boldsymbol{\sigma}_{y , \mathbf{X}}
#'     =
#'     \{ \sigma_{y, x_1}, \sigma_{y, x_j}, \sigma_{y, x_p} \}^{\prime}
#'   }
#'   where
#'   \eqn{j = \{ 1, \cdots, p \}}.
#' @inheritParams sigmasq
#' @inheritParams sigmaysq
#'
#' @returns A numeric vector of length 1.
#'
#' @export
#' @family Structure of Regression Functions
#' @keywords strRegression
rsq <- function(beta,
                sigmasq,
                sigmayx,
                sigmacapx) {
  stopifnot(
    is.vector(beta),
    is.vector(sigmasq),
    is.vector(sigmayx),
    is.matrix(sigmacapx)
  )
  p <- length(beta)
  k <- p + 1
  stopifnot(
    dim(sigmacapx) == c(p, p),
    p == length(sigmayx)
  )
  sigmacap <- matrix(
    0,
    nrow = k,
    ncol = k
  )
  sigmayx <- drop(
    tcrossprod(
      beta,
      sigmacapx
    )
  )
  sigmacap[1, 1] <- drop(
    sigmasq + (
      sigmayx %*% beta
    )
  )
  sigmacap[1, 2:k] <- sigmayx
  sigmacap[2:k, 1] <- sigmayx
  sigmacap[2:k, 2:k] <- sigmacapx
  return(
    rsq_of_sigmacap(sigmacap)
  )
}
