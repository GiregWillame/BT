#' Cross-validation errors.
#'
#' Function to compute the cross-validation error.
#'
#' @param BT_cv_fit a \code{\link{BTCVFit}} object.
#' @param cv.folds a numeric corresponding to the number of folds.
#' @param folds a numerical vector containing the different \code{folds.id}. Note that if the latter was not defined by the user, those are randomly generated based on the \code{cv.folds} input.
#'
#' @return Vector containing the cross-validation errors w.r.t. the boosting iteration.
#'
#' @details
#' This function computes the the global cross-validation error as a function of the boosting iteration. Differently said, this measure is obtained by
#' computing the weighted average of out-of-fold errors.
#'
#' @author Gireg Willame \email{g.willame@@detralytics.eu}
#'
#' \emph{This package is inspired by the \code{gbm3} package. For more details, see \url{https://github.com/gbm-developers/gbm3/}}.
#'
#' @seealso \code{\link{BT}}.
#'
#' @references D. Hainaut, J. Trufin and M. Denuit (2019). \dQuote{Effective Statistical Learning Methods for Actuaries, volume 1, 2 & 3}, \emph{Springer Actuarial}.
#'
#' @rdname BT_cv_errors
#' @export
#'
BT_cv_errors <- function(BT_cv_fit, cv.folds, folds) {
  UseMethod("BT_cv_errors", BT_cv_fit)
}

#' @keywords internal
#'
BT_cv_errors.BTCVFit <- function(BT_cv_fit, cv.folds, folds){

  check_if_BTCV_fit(BT_cv_fit)

  in_group <- tabulate(folds, nbins=cv.folds)
  cv_error <- vapply(seq_len(cv.folds),
                     function(xx){
                       model <- BT_cv_fit[[xx]]
                       model$BTErrors$validation.error * in_group[xx]
                     }, double(BT_cv_fit[[1]]$BTParams$n.iter)) # Similar structure for each BT_cv_fit.
  ## this is now a (num_trees, cv.folds) matrix
  ## and now a n.trees vector
  return(rowSums(cv_error) / length(folds))
}
