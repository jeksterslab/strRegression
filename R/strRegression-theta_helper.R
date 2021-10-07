#' Helper Function to Convert a Vector of Parameters
#' to a List of Parameters
#'
#' @details
#' # Dependencies
#' * [rmvn_chol()] (test)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric vector.
#'   Vector of parameters
#'   \eqn{\boldsymbol{theta}
#'   =
#'   \{
#'     \boldsymbol{\beta},
#'     \sigma^2,
#'     \mathrm{\vech}
#'     \left( \boldsymbol{\Sigma}_{\mathbf{X}, \mathbf{X}} \right)
#'   \}^{\prime}
#'   } when `mean_structure = FALSE` or
#'   \eqn{\boldsymbol{theta}
#'   =
#'   \{
#'     \boldsymbol{\beta},
#'     \sigma^2,
#'     \mathrm{\vech}
#'     \left( \boldsymbol{\Sigma}_{\mathbf{X}, \mathbf{X}} \right),
#'     \alpha,
#'     \boldsymbol{\mu}_{\mathbf{X}}
#'   \}^{\prime}
#'   } when `mean_structure = TRUE`
#' @param mean_structure Logical.
#'   If `mean_structure = TRUE`,
#'   `x` includes mean structure parameters.
#'   If `mean_structure = FALSE`,
#'   `x` only includes covariance structure parameters.
#'
#' @returns A list.
#'
#' @export
#' @family Structure of Regression Functions
#' @keywords strRegression
theta_helper <- function(x,
                         mean_structure = FALSE) {
  stopifnot(
    is.vector(x)
  )
  q <- length(x)
  if (mean_structure) {
    stopifnot(
      q >= 5
    )
    p <- 0.5 * (sqrt(8 * q + 9) - 5)
  } else {
    stopifnot(
      q >= 3
    )
    p <- 0.5 * (sqrt(8 * q + 1) - 3)
  }
  sigmacapx <- matrix(
    0,
    nrow = p,
    ncol = p
  )
  if (nrow(sigmacapx) != p) {
    stop("Length of \"x\" is not valid.")
  }
  k <- p + 1
  beta <- x[seq_len(p)]
  sigmasq <- x[k]
  sigmacapx[lower.tri(sigmacapx, diag = TRUE)] <- x[seq_len(p * (p + 1) / 2) + k]
  sigmacapx[upper.tri(sigmacapx)] <- t(sigmacapx)[upper.tri(sigmacapx)]
  if (mean_structure) {
    alpha <- x[(p * (p + 1) / 2) + k + 1]
    mux <- x[((p * (p + 1) / 2) + k + 2):q]
    return(
      list(
        beta = beta,
        sigmasq = sigmasq,
        sigmacapx = sigmacapx,
        alpha = alpha,
        mux = mux
      )
    )
  } else {
    return(
      list(
        beta = beta,
        sigmasq = sigmasq,
        sigmacapx = sigmacapx
      )
    )
  }
}
