#' Jacobian Matrix of the Regression Parameters
#' with Respect to the Unique Elements of the Covariance Matrix
#'
#' Matrix of partial derivatives of unstandardized parameters
#' with respect to the unique elements of the covariance matrix.
#' Note that the parameter vector is ordered as
#' \deqn{
#'   \boldsymbol{\theta}
#'   =
#'   \left\{
#'   \boldsymbol{\beta},
#'   \sigma^{2},
#'   \mathrm{vech} \left( \boldsymbol{\Sigma}_{\mathbf{X}, \mathbf{X}} \right)
#'   \right\}
#' }
#' and the covariance matrix \eqn{\boldsymbol{\Sigma}} is ordered as
#' \deqn{
#'   \left\{
#'   y,
#'   x_1,
#'   \dots,
#'   x_p
#'   \right\}^{\prime}
#' }.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric vector.
#'   Half-vectorization of the covariance matrix
#'   \eqn{\boldsymbol{\Sigma}}
#'   of
#'   \eqn{\left\{y, x_1, \dots, x_p \right\}^{\prime}}.
#'
#' @returns A matrix.
#'
#' @export
#' @family Structure of Regression Functions
#' @keywords strRegression
theta_wrt_vechsigmacap <- function(x) {
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
  sigmacapx <- sigmacap[2:k, 2:k, drop = FALSE]
  sigmayx <- sigmacap[2:k, 1, drop = FALSE]
  beta <- drop(
    solve(
      sigmacapx,
      sigmayx
    )
  )
  p <- length(beta)
  k <- p + 1
  s <- p * (p + 1) / 2
  betasq <- tcrossprod(beta)
  qcap <- chol2inv(chol(sigmacapx))
  dcap <- dcap(dim(sigmacapx)[1])
  tdcap <- t(dcap)
  beta_wrt_sigmaysq <- rep(x = 0, times = p)
  beta_wrt_sigmayx <- qcap
  beta_wrt_sigmacapx <- t(
    tdcap %*% (
      kronecker(
        -1 * beta,
        qcap
      )
    )
  )
  partial_beta <- cbind(
    beta_wrt_sigmaysq,
    beta_wrt_sigmayx,
    beta_wrt_sigmacapx
  )
  sigmasq_wrt_sigmasqy <- 1
  sigmasq_wrt_sigmayx <- -2 * beta
  sigmasq_wrt_sigmacapx <- as.vector(
    tdcap %*% as.vector(betasq)
  )
  partial_sigmasq <- c(
    sigmasq_wrt_sigmasqy,
    sigmasq_wrt_sigmayx,
    sigmasq_wrt_sigmacapx
  )
  sigmacapx_wrt_sigmacapx <- diag(s)
  sigmacapx_wrt_sigmasqy_sigmayx <- matrix(
    data = 0,
    nrow = s,
    ncol = k
  )
  partial_sigmacapx <- cbind(
    sigmacapx_wrt_sigmasqy_sigmayx,
    sigmacapx_wrt_sigmacapx
  )
  output <- rbind(
    partial_beta,
    partial_sigmasq,
    partial_sigmacapx
  )
  colnames(output) <- NULL
  rownames(output) <- NULL
  return(output)
}
