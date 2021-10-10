#' Regression Coefficients
#' as a Function of the Covariance Matrix
#'
#' @details
#' # Dependencies
#' * [rmvn_chol()] (test)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric matrix.
#'   Covariance matrix
#'   \eqn{\boldsymbol{\Sigma}}
#'   of
#'   \eqn{\{y, x_1, \cdots, x_p \}^{\prime}}.
#' @param verbose Logical.
#'   If `verbose = TRUE`, print message if error occurs.
#'
#' @returns A numeric vector.
#'
#' @export
#' @family Structure of Regression Functions
#' @keywords strRegression
beta_of_sigmacap <- function(x,
                             verbose = TRUE) {
  stopifnot(
    is.matrix(x)
  )
  k <- dim(x)[1]
  stopifnot(
    k == dim(x)[2],
    x == t(x)
  )
  tryCatch(
    {
      return(
        drop(
          solve(
            x[2:k, 2:k, drop = FALSE],
            x[2:k, 1, drop = FALSE]
          )
        )
      )
    },
    error = function(x) {
      if (verbose) {
        message(
          paste0(
            "Error in inverting the matrix.\n",
            "Returning a vector of NAs.\n"
          )
        )
      }
      return(
        rep(
          x = NA,
          times = k - 1
        )
      )
    }
  )
}
