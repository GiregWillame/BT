#' Printing functions.
#'
#' Functions to print the BT results.
#'
#' @param x a \code{\link{BTFit}} object.
#' @param \dots arguments passed to \code{print.default}.
#'
#' @details Print the different input parameters as well as obtained results (best iteration/performance & relative influence) given the chosen approach.
#'
#' @author Gireg Willame \email{g.willame@@detralytics.eu}
#'
#' \emph{This package is inspired by the \code{gbm3} package. For more details, see \url{https://github.com/gbm-developers/gbm3/}}.
#'
#' @seealso \code{\link{BT}}, \code{\link{BT_relative_influence}}, \code{\link{BT_perf}}.
#'
#' @references D. Hainaut, J. Trufin and M. Denuit (2019). \dQuote{Effective Statistical Learning Methods for Actuaries, volume 1, 2 & 3}, \emph{Springer Actuarial}.
#'
#' @rdname print.BTFit
#' @export
#'
print.BTFit <- function(x, ... ){
  # Print call
  if(!is.null(x$call)) print(x$call)

  #  Print out number of iterations and distribution used
  print_iters_and_dist(x)

  # Print out performance measures
  best_iter <- print_perf_measures(x)

  # Print out relative influence of variables
  ri <- BT_relative_influence(x, n.iter=best_iter)
  cat( "There were", length(x$var.names), "predictors of which",
       sum(ri > 0), "had non-zero influence.\n" )

  return(invisible(x))
}


#### Helper Functions ####
#' @rdname print.BTFit
#' @export
#'
print_iters_and_dist <- function(x) {
  check_if_BT_fit(x)
  if (x$BTParams$ABT){
    cat("An adaptive boosting tree model with Tweedie parameter :", x$distribution, " has been fitted.\n",
        length(iteration_error(x, 'train')), "iterations were performed.\n")
  }else{
    cat("A boosting tree model with Tweedie parameter :", x$distribution, " has been fitted.\n",
        length(iteration_error(x, 'train')), "iterations were performed.\n")
  }
}

#' @rdname print.BTFit
#' @export
#'
print_perf_measures <- function(x) {
  # Calculate the best number of iterations - returns test set if possible
  check_if_BT_fit(x)

  # Set default answer - final iteration
  best_iter <- length(iteration_error(x, 'train'))

  # OOB best iteration.
  if (has_bagging(x)){
    best_iter <- print(BT_callPerformance(x, method="OOB"))
  }
  # CV best iteration
  if (has_cross_validation(x)) {
    best_iter <- print(BT_callPerformance(x, method="cv"))
  }
  # Validation set best iteration
  if (has_train_validation_split(x)) {
    best_iter <- print(BT_callPerformance(x, method="validation"))
  }

  return(best_iter)
}
