#' Strict Half-Vectorize
#'
#' Apply a strict half-vectorization,
#' that is, half-vectorize an \eqn{m \times m}
#' symmetric matrix without the diagonal elements.
#'
#' Generates an \eqn{(m(m + 1) / 2) - m} vector
#' from the unique elements
#' of an \eqn{m \times m} symmetric matrix,
#' excluding the diagonals,
#' by stacking the columns (column-major).
#'
#' @details
#' # Dependencies
#' * [vechsnames()]
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Matrix.
#' @inheritParams vech
#'
#' @references
#' [Wikipedia: Half-vectorization](https://en.wikipedia.org/wiki/Vectorization_(mathematics)#Half-vectorization)
#'
#' @returns A vector.
#'
#' @examples
#' x <- matrix(
#'   data = c(
#'     1.0, 0.5, 0.4,
#'     0.5, 1.0, 0.6,
#'     0.4, 0.6, 1.0
#'   ),
#'   ncol = 3
#' )
#'
#' vechs(x)
#' @export
#' @family Linear Algebra Functions
#' @keywords linearAlgebra symmetric vectorization
vechs <- function(x,
                  names = FALSE,
                  sep = ".") {
  stopifnot(
    is.matrix(x),
    dim(x)[1] == dim(2),
    dim(x)[1] > 1
  )
  output <- x[lower.tri(x, diag = FALSE)]
  if (names) {
    varnames <- colnames(x)
    if (is.null(varnames)) {
      varnames <- paste0(seq_len(dim(x)[2]))
    }
    varnames <- vechsnames(varnames, sep = sep)
    names(output) <- varnames
  }
  return(output)
}
