#' Predictions for CV fitted BT models.
#'
#' Compute predictions from cross-validated Boosting Trees model.
#'
#' @param BTCVFit_object a \code{\link{BTCVFit}} object containing CV BT models.
#' @param data the database on which one wants to predict the different CV BT models.
#' @param cv.folds a positive integer specifying the number of folds to be used in cross-validation of the BT fit.
#' @param folds vector of integers specifying which row of data belongs to which cv.folds.
#' @param best.iter.cv the optimal number of trees with a CV approach.
#' @param \dots not currently used.
#'
#' @return Returns a vector of predictions for each cv folds.
#'
#' @details
#' This function has not been coded for public usage.
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
predict.BTCVFit <- function(BTCVFit_object, data, cv.folds, folds, best.iter.cv, ...){

  # We directly apply the predict on the dataset -> no database extraction.

  # Check match between data and folds.
  if (nrow(data) != length(folds)){
    stop('Error in predict.BTCVFit - folds and data should have the same number of records.')
  }

  # Flatten data for prediction.
  result <- rep(NA, length = nrow(data))

  for (index in seq_len(cv.folds)){
    currModel <- BTCVFit_object[[index]]
    flag <- which(folds==index)
    result[flag] <- predict(currModel,
                            newdata = data[flag, currModel$var.names, drop=FALSE],
                            n.trees=best.iter.cv)
  }
  return(result)
}
