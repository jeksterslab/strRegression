#' Covariance Matrix as a Function
#' the Vector of Standardized Regression Parameters
#'
#' @details
#' # Dependencies
#' * [rmvn_chol()] (test)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams thetastar_helper
#'
#' @returns A numeric matrix.
#'
#' @export
#' @family Structure of Regression Functions
#' @keywords strRegression
sigmacap_of_thetastar <- function(x) {
  theta <- thetastar_helper(x)
  betastar <- theta$betastar
  sigmay <- theta$sigmay
  sigmax <- theta$sigmax
  p <- length(betastar)
  k <- p + 1
  rhocap <- matrix(
    data = 1,
    nrow = k,
    ncol = k
  )
  d <- diag(c(sigmay, sigmax))
  if (p > 1) {
    rhocapx <- theta$rhocapx
    rhocap[2:k, 2:k] <- rhocapx
    rhoyx <- tcrossprod(betastar, rhocapx)
    rhocap[2:k, 1] <- rhoyx
    rhocap[1, 2:k] <- rhoyx
  } else {
    rhocap[1, 2] <- betastar
    rhocap[2, 1] <- betastar
  }
  return(
    d %*% rhocap %*% d
  )
}
