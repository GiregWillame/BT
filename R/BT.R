#' (Adaptive) Boosting Trees (ABT/BT) Algorithm.
#'
#' Perform the (Adaptive) Boosting Trees algorithm. This prepare the inputs and call the function \code{\link{BT_call}}.
#' Each tree in the process is built thanks to the \code{\link{rpart}} function.
#' In case of cross-validation, this function prepares the folds and performs multiple calls to the fitting function \code{\link{BT_call}}.
#'
#' @param formula a symbolic description of the model to be fit. Note that the offset isn't supported in this algorithm.
#' Instead, everything is performed with a log-link function and a direct relationship exist between response, offset and weights.
#'
#' @param data an optional data frame containing the variables in the model. By default the variables are taken from \code{environment(formula)}, typically the environment from which
#' \code{BT} is called. If \code{keep.data=TRUE} in the initial call to \code{BT} then \code{BT} stores a copy with the object (up to the variables used).
#'
#' @param tweedie.power the tweedie power related to the chosen distribution. This parameter should be a positive integer not defined in the interval (0,1). By default, it is set to 1.
#' Currently, the only Poisson approach is developed.
#'
#' @param ABT a boolean parameter. If \code{ABT=TRUE} an adaptive boosting tree algorithm is built whereas if \code{ABT=FALSE} an usual boosting tree algorithm is run.
#' By default, it is set to \code{TRUE}.
#'
#' @param n.iter the total number of iterations to fit. This is equivalent to the number of trees and the number of basis function in the additive expansion.
#' Please note that the initialization is not taken into account in the \code{n.iter}. More explicitly, a first GLM initializes the algorithm and then \code{n.iter} trees
#' are built. Moreover, note that the \code{bag.fraction}, \code{colsample.bytree}, ... are not used for this initializing phase.
#' By default, it is set to 100.
#'
#' @param train.fraction the first \code{train.fraction * nrows(data)} observations are used to fit the \code{BT} and the remainder are used for
#' computing out-of-sample estimates (also known as validation error) of the loss function. By default, it is set to 1 meaning no out-of-sample estimates.
#'
#' @param interaction.depth the maximum depth of variable interactions: 1 builds an additive model, 2 builds a model with up to two-way interactions, etc. By default, it is set to 4.
#' Please note that if this parameter is \code{NULL}, all the trees in the expansion are built based on the \code{tree.control} parameter only.
#' This option is devoted to advanced users only and allows them to benefit from the full flexibility of the implemented algorithm.
#'
#' @param shrinkage a shrinkage parameter (in the interval (0,1]) applied to each tree in the expansion. Also known as the learning rate or step-size reduction. By default, it is set to 1.
#'
#' @param bag.fraction the fraction of independent training observations randomly selected to propose the next tree in the expansion.
#' This introduces randomness into the model fit. If \code{bag.fraction}<1 then running the same model twice will result in similar but different fits.
#' \code{BT} uses the R random number generator, so \code{set.seed} ensures the same model can be reconstructed.
#' Please note that if this parameter is used the \code{BTErrors$training.error} corresponds to the normalized in-bag error and the out-of-bag improvements
#' are computed and stored in \code{BTErrors$oob.improvement}. See \code{\link{BTFit}} for more details.
#' By default, it is set to 1.
#'
#' @param colsample.bytree each tree will be trained on a random subset of \code{colsample.bytree} number of features. Each tree will consider a new
#' random subset of features from the formula, adding variability to the algorithm and reducing computation time. \code{colsample.bytree} will be bounded between
#' 1 and the number of features considered in the formula. By default, it is set to \code{NULL} meaning no effect.
#'
#' @param keep.data a boolean variable indicating whether to keep the data frames or not. This is particularly useful if one wants to keep track of the initial data frames
#' and is further used for predicting in case any data frame is specified.
#' Note that in case of cross-validation, if \code{keep.data=TRUE} the initial data frames is saved whereas the cross-validation samples are not.
#' By default, it is set to \code{FALSE}.
#'
#' @param is.verbose if \code{is.verbose=TRUE}, the \code{BT} will print out the algorithm progress. By default, it is set to \code{FALSE}.
#'
#' @param cv.folds a positive integer representing the number of cross-validation folds to perform. If \code{cv.folds}>1 then \code{BT}, in addition to the usual fit,
#' will perform a cross-validation and calculate an estimate of generalization error returned in \code{BTErrors$cv.error}. By default, it is set to 1 meaning no cross-validation.
#'
#' @param folds.id an optional vector of values identifying what fold each observation is in. If supplied, this parameter prevails over \code{cv.folds}.
#' By default, \code{folds.id = NULL} meaning that no folds are defined.
#'
#' @param n.cores the number of cores to use for parallelization. This parameter is used during the cross-validation.
#' By default, it is set to 1.
#'
#' @param tree.control for advanced user only. It allows to define additional tree parameters that will be used at each iteration.
#' See \code{\link{rpart.control}} for more information.
#'
#' @param weights optional vector of weights used in the fitting process. These weights must be positive but does not need to be normalized.
#' By default, it is set to \code{NULL} which corresponds to an uniform weight of 1 for each observation.
#'
#' @param \dots not currently used.
#'
#' @return a \code{BTFit} object.
#'
#' @author Gireg Willame \email{g.willame@@detralytics.eu}
#'
#' Julien Trufin \email{j.trufin@@detralytics.eu}
#'
#' Michel Denuit \email{m.denuit@@detralytics.eu}
#'
#' @seealso \code{\link{BTFit}}, \code{\link{BTCVFit}}, \code{\link{BT_call}}, \code{\link{BT.perf}}, \code{\link{predict.BTFit}},
#' \code{\link{summary.BTFit}}, \code{\link{print.BTFit}}, \code{\link{BT_cv_errors}}.
#'
#' @references D. Hainaut, J. Trufin and M. Denuit (2019). \dQuote{Effective Statistical Learning Methods for Actuaries, volume 1, 2 & 3,} \emph{Springer Actuarial}.
#'
#' @export
#'
BT <- function(formula = formula(data), data=list(), tweedie.power = 1, ABT = TRUE, n.iter = 100,
               train.fraction = 1, interaction.depth = 4, shrinkage = 1, bag.fraction = 1,
               colsample.bytree = NULL, keep.data = TRUE, is.verbose = FALSE,
               cv.folds = 1, folds.id = NULL, n.cores = 1,
               tree.control = rpart.control(xval = 0, maxdepth = (if(!is.null(interaction.depth)){interaction.depth} else{10}), cp = 0, minsplit = 2),
               weights = NULL, ...){

  the_call <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "weights"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  mf$na.action <- na.pass
  mf[[1]] <- as.name("model.frame")
  m <- mf
  mf <- eval(mf, parent.frame())
  Terms <- attr(mf, "terms")
  respVar <- as.character(attr(Terms, "variables"))[-1][attr(Terms, "response")] ; explVar <- attr(Terms, "term.labels")
  #mf$originalRespVar <- mf[,respVar] # Keep the original variable -> Do not need to modify the formula that way.
  #originalFormula <- formula # Keep a track of the original formula if there's any change with variable subsampling.

  if(!is.null(attr(Terms, "offset"))){
    stop("Offset are not supported. For Poisson model with log-link function, weights (=offset) and response variable (=Frequency rate, i.e. ClaimNb/Offset) can instead be used.")
  }

  if (is.null(model.weights(mf))){mf$w <- rep(1, nrow(mf))}
  else{colnames(mf)[names(mf)=="(weights)"] <- "w"}
  w <- "w"

  check_tweedie_power(tweedie.power)
  check_ABT(ABT)
  check_n_iter(n.iter)
  check_train_fraction(train.fraction)
  check_interaction_depth(interaction.depth)
  check_shrinkage(shrinkage)
  check_bag_fraction(bag.fraction)
  check_colsample_bytree(colsample.bytree)
  check_keep_data(keep.data)
  check_is_verbose(is.verbose)
  check_cv_folds(cv.folds)
  check_folds_id(folds.id)
  check_n_cores(n.cores)
  check_weights(mf$w)

  if (!is.null(interaction.depth) && tree.control$maxdepth != interaction.depth){
    stop("interaction.depth and maxdepth defined. If interaction.depth is not null it has to be set to maxdepth.")
  }

  setList <- create_validation_set(mf, train.fraction)
  training.set <- setList$training.set ; validation.set <- setList$validation.set
  rm(setList) ; rm(mf) ; gc()

  # Fit full model.
  if (is.verbose) message("Fit the model on the whole training set. \n")
  BT_full_results <- BT_call(training.set, validation.set, tweedie.power, respVar, w, explVar, ABT,
                             tree.control, train.fraction, interaction.depth, bag.fraction, shrinkage, n.iter, colsample.bytree,
                             keep.data, is.verbose)

  if (!is.null(folds.id)){
    numFolds <- length(unique(folds.id))
    if (cv.folds != numFolds) warning("CV folds changed from ", cv.folds, " to ", numFolds, " because of levels in folds.id")
    cv.folds <- numFolds
    # Transform folds.id index to a numeric vector of index, ascending from 1.
    folds.id <- as.numeric(as.factor(folds.id))
  }

  if (cv.folds==1){
    BT_full_results$cv.folds <- cv.folds ; BT_full_results$call <- the_call ; BT_full_results$Terms <- Terms
    return(BT_full_results)
  }

  # Else : cv.folds > 1 (or folds.id defined).
  if (is.verbose) message("Fit the model on the different CV folds. \n")
  folds <- create_cv_folds(training.set, cv.folds, folds.id)
  cl <- makeCluster(n.cores)
  clusterExport(cl, varlist=c("training.set", "tweedie.power", "respVar", "w", "explVar", "ABT",
                              "tree.control", "train.fraction", "interaction.depth", "bag.fraction", "shrinkage", "n.iter",
                              "colsample.bytree", "keep.data", "is.verbose"))
  BT_cv_results <- parLapply(cl, seq_len(cv.folds), function(xx){
    if (is.verbose) message('CV : ', xx, '\n')
    valIndex <- which(folds==xx) ; trainIndex <- setdiff(1:length(folds), valIndex)
    BT_call(training.set[trainIndex,], training.set[valIndex,], tweedie.power, respVar, w, explVar, ABT,
            tree.control, train.fraction, interaction.depth, bag.fraction, shrinkage, n.iter, colsample.bytree,
            FALSE, is.verbose) # We dont keep a copy of each dataset in case of cross-validation, keep.data=FALSE
  })
  stopCluster(cl)
  # Different folds -> result object is from a different class.
  class(BT_cv_results) <- "BTCVFit"

  cv_errors <- BT_cv_errors(BT_cv_results, cv.folds, folds)

  # Best number of iterations/trees.
  bestIterCV <- which.min(cv_errors)

  # Prediction on each OOF for the optimal number of iterations.
  predictions <- predict(BT_cv_results, training.set, cv.folds, folds, bestIterCV)

  # Extract relevant part - all data model.
  BT_full_results$cv.folds <- cv.folds ; BT_full_results$folds <- folds
  BT_full_results$call <- the_call ; BT_full_results$Terms <- Terms
  BT_full_results$BTErrors$cv.error <- cv_errors
  BT_full_results$cv.fitted <- predictions

  return(BT_full_results)
}


