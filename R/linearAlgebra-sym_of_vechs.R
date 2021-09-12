#' Symmetric matrix A from vechs(A)
#'
#' Symmetric matrix from its strict half-vectorization.
#'
#' Generates an \eqn{m \times m} symmetric matrix
#' from an \eqn{(m(m + 1) / 2) - m} vector.
#' The \eqn{m \times 1} vector of diagonal values should be supplied.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Vector.
#' @param diags Vector.
#'   Diagonal elements.
#'
#' @references
#' [Wikipedia: Half-vectorization](https://en.wikipedia.org/wiki/Vectorization_(mathematics)#Half-vectorization)
#'
#' @returns A vector.
#'
#' @examples
#' A <- matrix(
#'   data = c(
#'     1.0, 0.5, 0.4,
#'     0.5, 1.0, 0.6,
#'     0.4, 0.6, 1.0
#'   ),
#'   ncol = 3
#' )
#' vechsA <- c(0.5, 0.4, 0.6)
#'
#' sym_of_vechs(vechsA, diags = 1)
#' @export
#' @family Linear Algebra Functions
#' @keywords linearAlgebra symmetric
sym_of_vechs <- function(x, diags) {
  # m = m by m dimensions of the symmetric matrix
  # length(x) = m(m + 1) / 2 solve for m
  # output = symmetric matrix output
  stopifnot(
    is.vector(x),
    is.vector(diags)
  )
  m <- 0.5 * (sqrt(1 + 8 * length(x)) + 1)
  output <- matrix(
    data = 0,
    nrow = m,
    ncol = m
  )
  if (nrow(output) != m) {
    stop("Length of \"x\" is not valid.")
  }
  diags_m <- length(diags)
  stopifnot(diags_m == 1 || diags_m == m)
  output[lower.tri(output, diag = FALSE)] <- x
  output[upper.tri(output)] <- t(output)[upper.tri(output)]
  diag(output) <- diags
  return(output)
}
