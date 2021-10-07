#' Mean vector as a Function of Regression Parameters
#'
#' @param x Numeric vector.
#'   Vector of parameters
#'   \eqn{
#'     \boldsymbol{\theta}
#'     =
#'     \left\{
#'       \boldsymbol{\mu} ,
#'       \mathrm{vech} \left( \boldsymbol{\Sigma} \right)
#'     \right\}^{\prime}
#'   }.
#'
#' @returns A numeric vector.
#'
#' @export
#' @family Structure of Regression Functions
#' @keywords strRegression
mu_of_theta <- function(x) {
  theta <- theta_helper(
    x,
    mean_structure = TRUE
  )
  beta <- theta$beta
  alpha <- theta$alpha
  mux <- theta$mux
  if (length(beta) == 1) {
    muy <- drop(
      alpha + (
        beta * mux
      )
    )
  } else {
    muy <- drop(
      alpha + as.vector(
        crossprod(beta, mux)
      )
    )
  }
  return(
    c(
      muy,
      mux
    )
  )
}
