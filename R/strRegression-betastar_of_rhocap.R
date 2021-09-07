#' Standardized Regression Coefficients
#' as a Function of the Correlation Matrix
#'
#' @details
#' # Dependencies
#' * [rmvn_chol()] (test)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric matrix.
#'   Correlation matrix
#'   \eqn{\mathbf{P}}
#'   of
#'   \eqn{\{y, x_1, \cdots, x_p \}^{\prime}}.
#'
#' @returns A numeric vector.
#'
#' @export
#' @family Structure of Regression Functions
#' @keywords strRegression
betastar_of_rhocap <- function(x) {
  stopifnot(
    is.matrix(x)
  )
  k <- dim(x)[1]
  stopifnot(
    k == dim(x)[2],
    x == t(x),
    all(diag(x) == 1),
    all(abs(as.vector(x)) <= 1)
  )

  return(
    drop(
      solve(
        x[2:k, 2:k, drop = FALSE],
        x[2:k, 1, drop = FALSE]
      )
    )
  )
}
