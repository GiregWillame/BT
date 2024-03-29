% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BT_Predict.R
\name{predict.BTFit}
\alias{predict.BTFit}
\title{Predict method for BT Model fits.}
\usage{
\method{predict}{BTFit}(object, newdata, n.iter, type = "link", single.iter = FALSE, ...)
}
\arguments{
\item{object}{a \code{\link{BTFit}} object.}

\item{newdata}{data frame of observations for which to make predictions. If missing or not a data frame, if \code{keep.data=TRUE} in the initial fit then the original training set will be used.}

\item{n.iter}{number of boosting iterations used for the prediction. This parameter can be a vector in which case predictions are returned for each iteration specified.}

\item{type}{the scale on which the BT makes the predictions. Can either be "link" or "response". Note that, by construction, a log-link function is used during the fit.}

\item{single.iter}{if \code{single.iter=TRUE} then \code{predict.BTFit} returns the predictions from the single tree \code{n.iter}.}

\item{\dots}{not currently used.}
}
\value{
Returns a vector of predictions. By default, the predictions are on the score scale.
If \code{type = "response"}, then \code{BT} converts back to the same scale as the outcome. Note that, a log-link is supposed by construction.
}
\description{
Predicted values based on a boosting tree model object.
}
\details{
\code{predict.BTFit} produces a predicted values for each observation in \code{newdata} using the first \code{n.iter} boosting iterations.
If \code{n.iter} is a vector then the result is a matrix with each column corresponding to the \code{BT} predictions with \code{n.iter[1]} boosting iterations, \code{n.iter[2]} boosting
iterations, and so on.

As for the fit, the predictions do not include any offset term.
In the Poisson case, please remind that a weighted approach is initially favored.
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
\code{\link{BT}}, \code{\link{BTFit}}.
}
\author{
Gireg Willame \email{gireg.willame@gmail.com}

\emph{This package is inspired by the \code{gbm3} package. For more details, see \url{https://github.com/gbm-developers/gbm3/}}.
}
