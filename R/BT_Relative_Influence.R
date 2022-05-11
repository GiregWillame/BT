#' Method for estimating the relative influence.
#'
#' Helper function for computing the relative influence of each variable in the BT object.
#'
#' @param BTFit_object a \code{\link{BTFit}} object.
#' @param n.iter number of boosting iteration used for computations. If not provided, the function will perform a best guest approach to determine the optimal number of iterations. In fact,
#' if a test set was used during the fitting, the retained number of iterations is the one corresponding to the lowest test set error ; otherwise, if cross-validation was performed, the
#' number of iterations resulting in lowest cross-validation error will be used; otherwise, if the out-of-bag parameter was defined, the OOB error will be used to determine the optimal
#' number of iterations; otherwise, all iterations will be used.
#' @param rescale whether or not the results should be rescaled. Default set to \code{FALSE}.
#' @param sort.it whether or not the results should be (reverse) sorted. Default set to \code{FALSE}.
#'
#' @return Returns by default an unprocessed vector of estimated relative influences. If the \code{rescale} and \code{sort.it} arguments are used, it returns
#' a processed version of the same vector.
#'
#' @details
#' This function is not intended for end-user use. It performs the relative influence computation and is called during the summary function.
#' Note that a permutation approach is not yet implemented.
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
#' @rdname BT_relative_influence
#' @export
#'
BT_relative_influence <- function(BTFit_object, n.iter,
                               rescale = FALSE, sort.it = FALSE){
  # Initial checks
  check_if_BT_fit(BTFit_object)
  if(!is.logical(rescale) || (length(rescale) > 1))
    stop("rescale argument must be a logical")
  if(!is.logical(sort.it) || (length(sort.it) > 1))
    stop("sort.it must be a logical")

  # Fill in missing values
  if(missing(n.iter)){
    if (has_train_validation_split(BTFit_object)){
      n.iter <- BT_performance(BTFit_object, method="validation")
    }
    else if ( has_cross_validation(BTFit_object) ) {
      n.iter <- BT_performance(BTFit_object, method="cv")
    }
    else if (has_bagging(BTFit_object)){
      n.iter <- BT_performance(BTFit_object, method="OOB")
    }
    else{
      n.iter <- BTFit_object$BTParams$n.iter
    }
    message("n.iter not given. Using ", n.iter, " trees.")

  } else if (n.iter > length(BTFit_object$BTIndivFits)){
    stop("n.iter exceeds number in fit")
  }

  # Create relative influence for every variable
  rel_inf_verbose <- unlist(lapply(BTFit_object$BTIndivFits[seq(1, n.iter)],
                                   get_rel_inf_of_vars))

  # Sum across trees
  rel_inf_compact <- unlist(lapply(split(rel_inf_verbose, names(rel_inf_verbose)), sum))
  # Not the case with rpart : rel_inf_compact <- rel_inf_compact[names(rel_inf_compact) != "-1"] , directly 'dropped'.

  # rel_inf_compact excludes those variable that never entered the model
  # insert 0's for the excluded variables
  if (length(BTFit_object$var.names) != length(names(rel_inf_compact))){
    varToAdd <- BTFit_object$var.names[!(BTFit_object$var.names %in% names(rel_inf_compact))]
    rel_inf <- c(rel_inf_compact, rep(0, length(varToAdd)))
    names(rel_inf)[(length(rel_inf_compact)+1):length(BTFit_object$var.names)] <- varToAdd
  }else{
    rel_inf <- rel_inf_compact
  }

  # Rescale and sort
  if (rescale) rel_inf <- rel_inf / max(rel_inf)
  if (sort.it) rel_inf <- rev(sort(rel_inf))

  return(rel_inf)
}

#### Helper function ####
#' @keywords internal
get_rel_inf_of_vars <- function(rpart_object) {
  if (!is.null(rpart_object$splits)) return(lapply(split(rpart_object$splits[,3], names(rpart_object$splits[,3])), sum)) # 3 - Improvement
  else (return(list())) # With rpart : splits isn't returned if we've a single node (i.e. no splits).
}
