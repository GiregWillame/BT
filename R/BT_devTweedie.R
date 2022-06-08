# Deviance functions for Tweedie family.

#' Deviance function for the Tweedie family.
#'
#' Compute the deviance for the Tweedie family case.
#'
#' @param y a vector containing the observed values.
#' @param mu a vector contraining the fitted values.
#' @param w a vector of weights. If no weights are needed, please
#' @param tweedieVal a numeric representing the Tweedie Power. It has to be a positive numeric outside of the interval ]0,1[.
#'
#' @return A vector of individual deviance contribution.
#'
#' @details
#' This function computes the Tweedie related deviance. The latter is defined as
#'
#' @author Gireg Willame \email{g.willame@@detralytics.eu}
#'
#' \emph{This package is inspired by the \code{gbm3} package, originally written by Greg Ridgeway \email{greg.ridgeway@@gmail.com}}.
#'
#' @seealso \code{\link{BT}}, \code{\link{BT_call}}.
#'
#' @references D. Hainaut, J. Trufin and M. Denuit (2019). \dQuote{Effective Statistical Learning Methods for Actuaries, volume 1, 2 & 3,} \emph{Springer Actuarial}.
#'
#' @export
#'
BT_devTweedie <- function(y, mu, tweedieVal, w = NULL){

  check_tweedie_power(tweedieVal)
  if(any(is.logical(y) | is.character(y) | (y != as.double(y)) | is.na(y))) {
    stop("Responses must be doubles")
  }
  if(any(is.logical(mu) | is.character(mu) | (mu != as.double(mu)) | is.na(mu))) {
    stop("Predictions must be doubles")
  }
  if (is.null(w)) {w <- rep(1, length(y))}

  if(any(is.logical(w) | is.character(w) | (w != as.double(w)) | is.na(w) | (w < 0))) {
    stop("Weights must be positive doubles")
  }
  if (any(length(y) != length(mu) | length(y) != length(w))){
    stop("Responses, predictions and weights should have the same length")
  }

  if (tweedieVal==0){ # Gaussian case.
    dev<-w*(y-mu)**2
  } else if (tweedieVal==1){ # Poisson case.
    r <- mu
    p <- which(y>0)
    r[p] <- (y*log(y/mu) - (y-mu))[p]
    dev<-2*r*w
  } else if (tweedieVal==2){ # Gamma case.
    dev<-2*w*(-log(ifelse(y==0, 1, y/mu)) + (y/mu) - 1) # Support Gamma : ]0; +Inf[
  } else{
    dev<-2*w*(((max(y,0)^(2-tweedieVal))/((1-tweedieVal)*(2-tweedieVal))) - (y*(mu^(1-tweedieVal))/(1-tweedieVal)) + ((mu^(2-tweedieVal))/(2-tweedieVal)))
  }
  return(dev)
}
