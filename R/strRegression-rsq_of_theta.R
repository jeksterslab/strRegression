#' R-Squared as a Function
#' the Vector of Regression Parameters
#'
#' @details
#' # Dependencies
#' * [rmvn_chol()] (test)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams theta_helper
#'
#' @returns A numeric vector of length 1.
#'
#' @export
#' @family Structure of Regression Functions
#' @keywords strRegression
rsq_of_theta <- function(x) {
  theta <- theta_helper(x)
  beta <- theta$beta
  sigmasq <- theta$sigmasq
  sigmacapx <- theta$sigmacapx
  sigmayx <- tcrossprod(beta, sigmacapx)
  sigmax <- sqrt(diag(sigmacapx))
  sigmay <- sqrt(
    drop(
      sigmasq + (tcrossprod(beta, sigmacapx) %*% beta)
    )
  )
  rhoyx <- sigmayx / (sigmax * sigmay)
  return(
    sum(beta * rhoyx)
  )
}
