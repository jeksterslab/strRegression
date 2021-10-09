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
#' @param beta Numeric vector.
#'   Partial regression slopes
#'   \eqn{
#'     \boldsymbol{\beta}
#'     =
#'     \{ \beta_1, \cdots, \beta_p \}^{\prime}
#'   }.
#' @param sigmacapx Numeric matrix.
#'   Covariance matrix of the regressor variables
#'   \eqn{
#'     \boldsymbol{\Sigma}_{\mathrm{X}, \mathrm{X}}
#'   }.
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
