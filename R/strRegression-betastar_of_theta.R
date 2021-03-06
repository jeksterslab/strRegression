#' Standardized Regression Coefficients
#' as a Function of the Vector of Regression Parameters
#'
#' @details
#' # Dependencies
#' * [rmvn_chol()] (test)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param verbose Logical.
#'   If `verbose = TRUE`, print message if error occurs.
#' @inheritParams theta_helper
#'
#' @returns A numeric vector.
#'
#' @export
#' @family Structure of Regression Functions
#' @keywords strRegression
betastar_of_theta <- function(x,
                              mean_structure = FALSE,
                              verbose = TRUE) {
  theta <- theta_helper(
    x = x,
    mean_structure = mean_structure
  )
  beta <- theta$beta
  sigmasq <- theta$sigmasq
  sigmacapx <- theta$sigmacapx
  sigmaxsq <- diag(sigmacapx)
  p <- length(beta)
  if (any(sigmaxsq <= 0)) {
    sigmax <- NA
  } else {
    sigmax <- sqrt(sigmaxsq)
  }
  sigmaysq <- sigmaysq(
    sigmasq = sigmasq,
    beta = beta,
    sigmacapx = sigmacapx
  )
  if (sigmaysq <= 0) {
    sigmay <- NA
  } else {
    sigmay <- sqrt(sigmaysq)
  }
  if (
    any(
      is.na(sigmax),
      is.na(sigmay)
    )
  ) {
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
        times = p
      )
    )
  }
  return(
    beta * (sigmax / sigmay)
  )
}
