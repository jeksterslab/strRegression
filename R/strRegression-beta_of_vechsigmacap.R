#' Regression Coefficients
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
#'   Half-vectorization of the covariance matrix
#'   \eqn{\boldsymbol{\Sigma}}
#'   of
#'   \eqn{\left\{y, x_1, \dots, x_p \right\}^{\prime}}.
#' @param verbose Logical.
#'   If `verbose = TRUE`, print message if error occurs.
#'
#' @returns A numeric vector.
#'
#' @export
#' @family Structure of Regression Functions
#' @keywords strRegression
beta_of_vechsigmacap <- function(x,
                                 verbose = TRUE) {
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
  tryCatch(
    {
      return(
        drop(
          solve(
            sigmacap[2:k, 2:k, drop = FALSE],
            sigmacap[2:k, 1, drop = FALSE]
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
