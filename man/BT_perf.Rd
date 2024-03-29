% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BT_Perf.R
\name{BT_perf}
\alias{BT_perf}
\title{Performance assessment.}
\usage{
BT_perf(
  BTFit_object,
  plot.it = TRUE,
  oobag.curve = FALSE,
  overlay = TRUE,
  method,
  main = ""
)
}
\arguments{
\item{BTFit_object}{a \code{\link{BTFit}} object resulting from an initial call to \code{\link{BT}}}

\item{plot.it}{a boolean indicating whether to plot the performance measure. Setting \code{plot.it = TRUE} creates two plots.
The first one plots the \code{object$BTErrors$training.error} (in black) as well as the \code{object$BTErrors$validation.error} (in red) and/or the \code{object$BTErrors$cv.error} (in green) depending on the \code{method} and
parametrization. These values are plotted as a function of the iteration number. The scale of the error measurement, shown on the left vertical axis, depends on the arguments used in the
initial call to \code{\link{BT}} and the chosen \code{method}.}

\item{oobag.curve}{indicates whether to plot the out-of-bag performance measures in a second plot. Note that this option makes sense if the \code{bag.fraction} was properly defined in the
initial call to \code{\link{BT}}.}

\item{overlay}{if set to \code{TRUE} and \code{oobag.curve=TRUE} then a right y-axis is added and the estimated cumulative improvement in the loss function is
plotted versus the iteration number.}

\item{method}{indicates the method used to estimate the optimal number of boosting iterations. Setting \code{method = "OOB"} computes the out-of-bag estimate and \code{method = "validation"}
uses the validation dataset to compute an out-of-sample estimate. Finally, setting \code{method = "cv"} extracts the optimal number of iterations using cross-validation, if
\code{\link{BT}} was called with \code{cv.folds > 1}. If missing, a guessing method is applied.}

\item{main}{optional parameter that allows the user to define specific plot title.}
}
\value{
Returns the estimated optimal number of iterations. The method of computation depends on the \code{method} argument.
}
\description{
Function to compute the performances of a fitted boosting tree.
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
\code{\link{BT}}, \code{\link{BT_call}}.
}
\author{
Gireg Willame \email{g.willame@detralytics.eu}

\emph{This package is inspired by the \code{gbm3} package. For more details, see \url{https://github.com/gbm-developers/gbm3/}}.
}
