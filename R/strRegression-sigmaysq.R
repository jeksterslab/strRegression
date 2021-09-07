#' Variance of the Regressand Variable
#'
#' @details
#' # Dependencies
#' * [rmvn_chol()] (test)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param sigmasq Numeric.
#'   Error variance
#'   \eqn{\sigma^2}.
#' @inheritParams sigmasq
#'
#' @returns A numeric vector of length 1.
#'
#' @export
#' @family Structure of Regression Functions
#' @keywords strRegression
sigmaysq <- function(sigmasq,
                     beta,
                     sigmacapx) {
  stopifnot(
    is.vector(sigmasq),
    is.vector(beta),
    is.matrix(sigmacapx)
  )
  return(
    drop(
      sigmasq + (tcrossprod(beta, sigmacapx) %*% beta)
    )
  )
}
