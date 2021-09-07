#' Standardized Regression Coefficients
#' as a Function of the Half-Vectorization
#' of the Covariance Matrix
#'
#' @details
#' # Dependencies
#' * [sym_of_vech()]
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
betastar_of_vechsigmacap <- function(x) {
  stopifnot(
    is.vector(x)
  )
  return(
    betastar_of_sigmacap(
      sym_of_vech(x)
    )
  )
}
