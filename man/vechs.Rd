% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linearAlgebra-vechs.R
\name{vechs}
\alias{vechs}
\title{Strict Half-Vectorize}
\usage{
vechs(x, names = FALSE, sep = ".")
}
\arguments{
\item{x}{Matrix.}

\item{names}{Logical.
Add names.}

\item{sep}{Character string.
Separator for variable names.}
}
\value{
A vector.
}
\description{
Apply a strict half-vectorization,
that is, half-vectorize an \eqn{m \times m}
symmetric matrix without the diagonal elements.
}
\details{
Generates an \eqn{(m(m + 1) / 2) - m} vector
from the unique elements
of an \eqn{m \times m} symmetric matrix,
excluding the diagonals,
by stacking the columns (column-major).
}
\section{Dependencies}{
\itemize{
\item \code{\link[=vechsnames]{vechsnames()}}
}
}

\examples{
x <- matrix(
  data = c(
    1.0, 0.5, 0.4,
    0.5, 1.0, 0.6,
    0.4, 0.6, 1.0
  ),
  ncol = 3
)

vechs(x)
}
\references{
\href{https://en.wikipedia.org/wiki/Vectorization_(mathematics)#Half-vectorization}{Wikipedia: Half-vectorization}
}
\seealso{
Other Linear Algebra Functions: 
\code{\link{dcap}()},
\code{\link{sym_of_vechs}()},
\code{\link{sym_of_vech}()},
\code{\link{vechnames}()},
\code{\link{vechsnames}()},
\code{\link{vech}()}
}
\author{
Ivan Jacob Agaloos Pesigan
}
\concept{Linear Algebra Functions}
\keyword{linearAlgebra}
\keyword{symmetric}
\keyword{vectorization}
