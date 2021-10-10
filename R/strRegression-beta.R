#' Regression Coefficients
#' as a Function of the Covariance Matrix
#'
#' @details
#' # Dependencies
#' * [rmvn_chol()] (test)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param sigmacapx Numeric matrix.
#'   Covariance matrix of the regressors.
#' @param sigmayx Numeric vector.
#'   Covariances between the regressand and regressor variables
#'   \eqn{
#'     \boldsymbol{\sigma}_{y , \mathbf{X}}
#'     =
#'     \{ \sigma_{y, x_1}, \sigma_{y, x_j}, \sigma_{y, x_p} \}^{\prime}
#'   }
#'   where
#'   \eqn{j = \{ 1, \cdots, p \}}.
#' @param verbose Logical.
#'   If `verbose = TRUE`, print message if error occurs.
#'
#' @returns A numeric vector.
#'
#' @export
#' @family Structure of Regression Functions
#' @keywords strRegression
beta <- function(sigmacapx,
                 sigmayx,
                 verbose = TRUE) {
  stopifnot(
    is.matrix(sigmacapx),
    sigmacapx == t(sigmacapx),
    is.vector(sigmayx)
  )
  tryCatch(
    {
      return(
        drop(
          solve(
            sigmacapx,
            sigmayx
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
          times = length(sigmayx)
        )
      )
    }
  )
}
