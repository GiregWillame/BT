#' Predict method for BT Model fits.
#'
#' Predicted values based on a boosting tree model object.
#'
#' @param BTFit_object a \code{\link{BTFit}} object.
#' @param newdata data frame of observations for which to make predictions. If missing or not a data frame, if \code{keep.data=TRUE} in the initial fit then the original training set will be used.
#' @param n.iter number of boosting iteration used in the prediction. This parameter can be a vector in which case predictions are returned for each iteration specified.
#' @param type the scale on which the BT makes the predictions. Can either be "link" or "response". Note that, by construction, a log-link function is used during the fit.
#' @param single.iter if \code{single.iter=TRUE} then \code{predict.BTFit} returns only the prediction from tree \code{n.iter}.
#' @param \dots not currently used.
#'
#' @return Returns a vector of predictions. By default, the predictions are on the score scale.
#' If \code{type = "response"}, then \code{BT} converts back to the same scale as the outcome. Note that, a log-link is supposed by construction.
#'
#' @details
#' \code{predict.BTFit} produces a predicted values for each observation in \code{newdata} using the first \code{n.iter} boosting iterations.
#' If \code{n.iter} is a vector then the result is a matrix with each column corresponding to the \code{BT} predictions with \code{n.iter[1]} boosting iterations, \code{n.iter[2]} boosting
#' iterations, and so on.
#'
#' As for the fit, the predictions do not include any offset term. The user may add the value of the offset to the predicted value, if desired.
#'
#' @author Gireg Willame \email{g.willame@@detralytics.eu}
#'
#' Julien Trufin \email{j.trufin@@detralytics.eu}
#'
#' Michel Denuit \email{m.denuit@@detralytics.eu}
#'
#' @seealso \code{\link{BT}}, \code{\link{BTFit}}.
#'
#' @references D. Hainaut, J. Trufin and M. Denuit (2019). \dQuote{Effective Statistical Learning Methods for Actuaries, volume 1, 2 & 3,} \emph{Springer Actuarial}.
#'
#' @export
#'
predict.BTFit <- function(BTFit_object, newdata, n.iter, type = "link", single.iter=FALSE, ...){

  # Check inputs
  if(!is.element(type, c("link","response" ))) {
    stop("type must be either 'link' or 'response'")
  }

  if(missing(newdata) || !is.data.frame(newdata)) {
    if (BTFit_object$keep.data){
      message("As newdata is missing or is not a data frame, the training set has been used thanks to the keep.data = TRUE parameter.")
      newdata <- BTFit_object$BTData$training.set
    } else{
      stop("newdata must be provided as a data frame.")
    }
  }

  if (!all(BTFit_object$var.names %in% colnames(newdata))){
    stop("newdata must contain the same explanatory variable as the original fitted BT object.")
  }

  if(missing(n.iter)) {
    stop("Number of iterations to be used in prediction must be provided.")
  }

  if (length(n.iter) == 0) {
    stop("n.iter cannot be NULL or a vector of zero length.")
  }

  if(any(n.iter != as.integer(n.iter)) || is.na(all(n.iter == as.integer(n.iter)))
     || any(n.iter <= 0)) { # at least one iteration - not only the init considered to avoid problem.
    stop("n.iter must be a vector of non-negative integers.")
  }

  if(any(n.iter > BTFit_object$BTParams$n.iter)) {
    n.iter[n.iter > BTFit_object$BTParams$n.iter] <- BTFit_object$BTParams$n.iter
    warning("Number of trees exceeded number fit so far. Using ", paste(n.iter,collapse=" "),".")
  }

  outMatrix <- matrix(NA, nrow=nrow(newdata), ncol=length(n.iter))

  if (single.iter){
    for (i in seq(1, length(n.iter))){
      iIter <- n.iter[i]
      # Link-scale output.
      outMatrix[,i] <- log(predict(BTFit_object$BTIndivFits[[iIter]], newdata))
    }
  } else{
    # Compute cumulative results for each iteration in the vector n.iter
    lastIter <- max(n.iter)
    shrinkage <- BTFit_object$BTParams$shrinkage

    currPred <- predict(BTFit_object$BTInit$initFit, newdata=newdata, type = "link") # GLM used as first prediction.


    for (iIter in seq(1, lastIter)){
      currPred <- currPred + shrinkage*log(predict(BTFit_object$BTIndivFits[[iIter]], newdata))
      if (iIter %in% n.iter){
        outMatrix[, which(n.iter == iIter)] <- currPred
      }
    }
  }

  if (type=="response") outMatrix <- exp(outMatrix) # Exponential link-function.
  if (length(n.iter)==1) outMatrix <- as.vector(outMatrix)
  return(outMatrix)
}


