#' Error Variance
#'
#' @details
#' # Dependencies
#' * [rmvn_chol()] (test)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param sigmaysq Numeric.
#'   Variance of the regressand variable
#'   \eqn{\sigma_{y}^{2}}.
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
sigmasq <- function(sigmaysq,
                    beta,
                    sigmacapx) {
  stopifnot(
    is.vector(sigmaysq),
    is.vector(beta),
    is.matrix(sigmacapx)
  )
  return(
    drop(
      sigmaysq - (
        tcrossprod(beta, sigmacapx) %*% beta
      )
    )
  )
}
