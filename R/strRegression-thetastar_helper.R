#' Helper Function to Convert a Vector of Standardized Parameters
#' to a List of Standardized Parameters
#'
#' @details
#' # Dependencies
#' * [rmvn_chol()] (test)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric vector.
#'   Vector of standardized parameters
#'   \eqn{\boldsymbol{theta}^{\ast}
#'   =
#'   \{
#'     \boldsymbol{\beta}^{\ast},
#'     \sigma_{y},
#'     \boldsymbol{\sigma}_{\mathbf{X}},
#'     \mathrm{\vechs}
#'     \left( \mathbf{P}_{\mathbf{X}, \mathbf{X}} \right)
#'   \}^{\prime}
#'   }
#'
#' @returns A list.
#'
#' @export
#' @family Structure of Regression Functions
#' @keywords strRegression
thetastar_helper <- function(x) {
  stopifnot(
    is.vector(x)
  )
  q <- length(x)
  stopifnot(
    q >= 3
  )
  p <- 0.5 * (sqrt(8 * q + 1) - 3)
  rhocapx <- matrix(
    1,
    nrow = p,
    ncol = p
  )
  if (nrow(rhocapx) != p) {
    stop("Length of \"x\" is not valid.")
  }
  k <- p + 1
  betastar <- x[seq_len(p)]
  sigmay <- x[k]
  x <- x[seq_len(p * (p + 1) / 2) + k]
  sigmax <- x[1:p]
  if (p > 1) {
    rhocapx <- sym_of_vechs(x[-(1:p)], diags = 1)
    return(
      list(
        betastar = betastar,
        sigmay = sigmay,
        sigmax = sigmax,
        rhocapx = rhocapx
      )
    )
  } else {
    return(
      list(
        betastar = betastar,
        sigmay = sigmay,
        sigmax = sigmax
      )
    )
  }
}
