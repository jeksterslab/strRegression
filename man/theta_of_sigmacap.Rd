% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strRegression-theta_of_sigmacap.R
\name{theta_of_sigmacap}
\alias{theta_of_sigmacap}
\title{Parameters Assuming Stochastic Regressors
as a Function of the Covariance Matrix}
\usage{
theta_of_sigmacap(x)
}
\arguments{
\item{x}{Numeric matrix.
Covariance matrix
\eqn{\boldsymbol{\Sigma}}
of
\eqn{\{y, x_1, \cdots, x_p \}^{\prime}}.}
}
\value{
A numeric vector.
}
\description{
Parameters Assuming Stochastic Regressors
as a Function of the Covariance Matrix
}
\section{Dependencies}{
\itemize{
\item \code{\link[=rmvn_chol]{rmvn_chol()}} (test)
}
}

\seealso{
Other Structure of Regression Functions: 
\code{\link{beta_of_sigmacap}()},
\code{\link{beta_of_vechsigmacap}()},
\code{\link{beta_wrt_vechsigmacap}()},
\code{\link{betastar_of_rhocap}()},
\code{\link{betastar_of_sigmacap}()},
\code{\link{betastar_of_theta}()},
\code{\link{betastar_of_vechsigmacap}()},
\code{\link{beta}()},
\code{\link{moments_helper}()},
\code{\link{mu_of_theta}()},
\code{\link{rsq_of_sigmacap}()},
\code{\link{rsq_of_theta}()},
\code{\link{rsq}()},
\code{\link{sigmacap_of_thetastar}()},
\code{\link{sigmacap_of_theta}()},
\code{\link{sigmasq_of_vechsigmacap}()},
\code{\link{sigmasq}()},
\code{\link{sigmaysq}()},
\code{\link{theta_helper}()},
\code{\link{theta_of_moments}()},
\code{\link{theta_of_vechsigmacap}()},
\code{\link{theta_wrt_vechsigmacap}()},
\code{\link{thetastar_helper}()},
\code{\link{thetastar_of_sigmacap}()},
\code{\link{thetastar_of_vechsigmacap}()},
\code{\link{thetastar}()},
\code{\link{theta}()}
}
\author{
Ivan Jacob Agaloos Pesigan
}
\concept{Structure of Regression Functions}
\keyword{strRegression}
