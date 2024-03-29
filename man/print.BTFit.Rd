% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BT_Print.R
\name{print.BTFit}
\alias{print.BTFit}
\title{Printing function.}
\usage{
\method{print}{BTFit}(x, ...)
}
\arguments{
\item{x}{a \code{\link{BTFit}} object.}

\item{\dots}{arguments passed to \code{print.default}.}
}
\value{
No value returned.
}
\description{
Function to print the BT results.
}
\details{
Print the different input parameters as well as obtained results (best iteration/performance & relative influence) given the chosen approach.
}
\references{
M. Denuit, D. Hainaut and J. Trufin (2019). \strong{Effective Statistical Learning Methods for Actuaries |: GLMs and Extensions}, \emph{Springer Actuarial}.

M. Denuit, D. Hainaut and J. Trufin (2019). \strong{Effective Statistical Learning Methods for Actuaries ||: Tree-Based Methods and Extensions}, \emph{Springer Actuarial}.

M. Denuit, D. Hainaut and J. Trufin (2019). \strong{Effective Statistical Learning Methods for Actuaries |||: Neural Networks and Extensions}, \emph{Springer Actuarial}.

M. Denuit, D. Hainaut and J. Trufin (2022). \strong{Response versus gradient boosting trees, GLMs and neural networks under Tweedie loss and log-link}.
Accepted for publication in \emph{Scandinavian Actuarial Journal}.

M. Denuit, J. Huyghe and J. Trufin (2022). \strong{Boosting cost-complexity pruned trees on Tweedie responses: The ABT machine for insurance ratemaking}.
Paper submitted for publication.

M. Denuit, J. Trufin and T. Verdebout (2022). \strong{Boosting on the responses with Tweedie loss functions}. Paper submitted for publication.
}
\seealso{
\code{\link{BT}}, \code{\link{.BT_relative_influence}}, \code{\link{BT_perf}}.
}
\author{
Gireg Willame \email{gireg.willame@gmail.com}

\emph{This package is inspired by the \code{gbm3} package. For more details, see \url{https://github.com/gbm-developers/gbm3/}}.
}
