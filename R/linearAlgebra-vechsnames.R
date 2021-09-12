#' Vector Names for Strict Half-Vectorization
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Character vector of names.
#' @inheritParams vechnames
#'
#' @references
#' [Wikipedia: Half-vectorization](https://en.wikipedia.org/wiki/Vectorization_(mathematics)#Half-vectorization)
#'
#' @returns A vector.
#'
#' @examples
#' x <- diag(2)
#' colnames(x) <- rownames(x) <- c("x1", "x2")
#' vechsnames(colnames(x))
#'
#' x <- diag(3)
#' colnames(x) <- rownames(x) <- c("x1", "x2", "x3")
#' vechsnames(colnames(x))
#' @export
#' @family Linear Algebra Functions
#' @keywords linearAlgebra symmetric vectorization
vechsnames <- function(x,
                       sep = ".") {
  stopifnot(
    is.vector(x),
    length(x) > 1
  )
  ind <- utils::combn(length(x), 2)
  output <- vector(mode = "list", length = 2)
  for (i in seq_len(dim(ind)[1])) {
    output[[i]] <- x[ind[i, ]]
  }
  paste0(
    output[[1]],
    sep,
    output[[2]]
  )
}
