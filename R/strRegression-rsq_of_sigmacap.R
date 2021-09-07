#' R-Squared as a Function of the Covariance Matrix
#'
#' @details
#' # Dependencies
#' * [rmvn_chol()] (test)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams beta_of_sigmacap
#'
#' @returns A numeric vector of length 1.
#'
#' @export
#' @family Structure of Regression Functions
#' @keywords strRegression
rsq_of_sigmacap <- function(x) {
  stopifnot(
    is.matrix(x),
    dim(x)[1] == dim(x)[2],
    x == t(x)
  )
  k <- dim(x)[1]
  return(
    1 - (
      det(x) / det(x[2:k, 2:k, drop = FALSE])
    ) / x[1, 1]
  )
}
