#' Covariance Matrix as a Function
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
#' @returns A numeric matrix.
#'
#' @export
#' @family Structure of Regression Functions
#' @keywords strRegression
sigmacap_of_theta <- function(x,
                              mean_structure = FALSE) {
  theta <- theta_helper(
    x,
    mean_structure = mean_structure
  )
  beta <- theta$beta
  sigmasq <- theta$sigmasq
  sigmacapx <- theta$sigmacapx
  sigmaysq <- drop(
    sigmasq + (tcrossprod(beta, sigmacapx) %*% beta)
  )
  sigmayx <- tcrossprod(beta, sigmacapx)
  p <- length(beta)
  k <- p + 1
  sigmacap <- matrix(
    data = 0,
    nrow = k,
    ncol = k
  )
  sigmacap[1, 1] <- sigmaysq
  sigmacap[1, 2:k] <- sigmayx
  sigmacap[2:k, 1] <- sigmayx
  sigmacap[2:k, 2:k] <- sigmacapx
  return(sigmacap)
}
