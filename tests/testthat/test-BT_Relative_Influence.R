#########################
# Author : Gireg Willame
# June 2022.
#
# The goal is to check that the BT_Relative_Influence
#   computation is correct.
#
########################

testthat::test_that("Check the BT_Relative_Influence function - Inputs",{

  # Create datasets.
  set.seed(4)
  n <- 100000

  Gender <- factor(sample(c("male","female"),n,replace=TRUE))
  Age <- sample(c(18:65),n,replace=TRUE)
  Split <- factor(sample(c("yes","no"),n,replace=TRUE))
  Sport <- factor(sample(c("yes","no"),n,replace=TRUE))

  lambda <- 0.1*ifelse(Gender=="male",1.1,1)
  lambda <- lambda*(1+1/(Age-17)^0.5)
  lambda <- lambda*ifelse(Sport=="yes",1.15,1)

  ExpoR <- runif(n)

  Y <- rpois(n, ExpoR*lambda)
  Y_normalized <- Y/ExpoR
  datasetFull <- data.frame(Y,Gender,Age,Split,Sport,ExpoR, Y_normalized)

  # Run a BT algo.
  set.seed(4)
  paramsBT <- list(formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
                   data = datasetFull,
                   tweedie.power = 1,
                   ABT = T,
                   n.iter = 200,
                   train.fraction = 0.8,
                   interaction.depth = 4,
                   shrinkage = 0.01,
                   bag.fraction = 0.5,
                   colsample.bytree = NULL,
                   keep.data = T,
                   is.verbose = F,
                   cv.folds = 1,
                   folds.id = NULL,
                   n.cores = 1,
                   weights = datasetFull$ExpoR)

  BT_algo <- do.call(BT, paramsBT)

  # empty or wrong BTFit_object
  expect_error(BT_relative_influence(list()))
  expect_error(BT_relative_influence(BTFit_object = seq(1,10)))
  expect_error(BT_relative_influence())

  # Check the n.iter parameter.
  expect_message(tempRes <- BT_relative_influence(BT_algo))
  expect_equal(tempRes, BT_relative_influence(BT_algo, BT_performance(BT_algo, "validation")))

  n.iter <- 100
  # Check rescale parameter.
  rescale <- 1 ;  expect_error(BT_relative_influence(BT_algo, n.iter, rescale = rescale))
  rescale <- 0.4 ; expect_error(BT_relative_influence(BT_algo, n.iter, rescale = rescale))
  rescale <- 2.785 ; expect_error(BT_relative_influence(BT_algo, n.iter, rescale = rescale))
  rescale <- c(3, 4) ; expect_error(BT_relative_influence(BT_algo, n.iter, rescale = rescale))
  rescale <- NULL ; expect_error(BT_relative_influence(BT_algo, n.iter, rescale = rescale))
  rescale <- NA ; expect_error(BT_relative_influence(BT_algo, n.iter, rescale = rescale))
  rescale <- "Text" ; expect_error(BT_relative_influence(BT_algo, n.iter, rescale = rescale))

  # Check sort.it parameter.
  sort.it <- 1 ;  expect_error(BT_relative_influence(BT_algo, n.iter, sort.it = sort.it))
  sort.it <- 0.4 ; expect_error(BT_relative_influence(BT_algo, n.iter, sort.it = sort.it))
  sort.it <- 2.785 ; expect_error(BT_relative_influence(BT_algo, n.iter, sort.it = sort.it))
  sort.it <- c(3, 4) ; expect_error(BT_relative_influence(BT_algo, n.iter, sort.it = sort.it))
  sort.it <- NULL ; expect_error(BT_relative_influence(BT_algo, n.iter, sort.it = sort.it))
  sort.it <- NA ; expect_error(BT_relative_influence(BT_algo, n.iter, sort.it = sort.it))
  sort.it <- "Text" ; expect_error(BT_relative_influence(BT_algo, n.iter, sort.it = sort.it))

  # Check if n.iter is not well defined (or bigger than number of algo iters).
  n.iter <- 400 ; expect_error(BT_relative_influence(BT_algo, n.iter))
  n.iter <- 0 ; expect_error(BT_relative_influence(BT_algo, n.iter))
  n.iter <- c(3, 4) ; expect_error(BT_relative_influence(BT_algo, n.iter))
  n.iter <- NULL ; expect_error(BT_relative_influence(BT_algo, n.iter))
  n.iter <- NA ; expect_error(BT_relative_influence(BT_algo, n.iter))
  n.iter <- "Text" ; expect_error(BT_relative_influence(BT_algo, n.iter))
  n.iter <- F ; expect_error(BT_relative_influence(BT_algo, n.iter))

  # Change inputs - test OOB.
  paramsBT$train.fraction <- 1
  BT_algo <- do.call(BT, paramsBT)
  # Check the n.iter parameter.
  expect_message(tempRes <- BT_relative_influence(BT_algo))
  expect_message(tempResExpected_NbIter <- BT_performance(BT_algo, "OOB"))
  expect_equal(tempRes, BT_relative_influence(BT_algo, tempResExpected_NbIter))

  # Change inputs - test cv
  paramsBT$cv.folds <- 3
  BT_algo <- do.call(BT, paramsBT)
  # Check the n.iter parameter.
  expect_message(tempRes <- BT_relative_influence(BT_algo))
  expect_equal(tempRes, BT_relative_influence(BT_algo, BT_performance(BT_algo, "cv")))

})

testthat::test_that("Check the BT_Relative_Influence function - Results",{

  # Create datasets.
  set.seed(4)
  n <- 100000

  Gender <- factor(sample(c("male","female"),n,replace=TRUE))
  Age <- sample(c(18:65),n,replace=TRUE)
  Split <- factor(sample(c("yes","no"),n,replace=TRUE))
  Sport <- factor(sample(c("yes","no"),n,replace=TRUE))

  lambda <- 0.1*ifelse(Gender=="male",1.1,1)
  lambda <- lambda*(1+1/(Age-17)^0.5)
  lambda <- lambda*ifelse(Sport=="yes",1.15,1)

  ExpoR <- runif(n)

  Y <- rpois(n, ExpoR*lambda)
  Y_normalized <- Y/ExpoR
  datasetFull <- data.frame(Y,Gender,Age,Split,Sport,ExpoR, Y_normalized)

  # Run a BT algo.
  set.seed(4)
  paramsBT <- list(formula = as.formula("Y_normalized ~ Age + Sport + Split + Gender"),
                   data = datasetFull,
                   tweedie.power = 1,
                   ABT = T,
                   n.iter = 200,
                   train.fraction = 0.8,
                   interaction.depth = 4,
                   shrinkage = 0.01,
                   bag.fraction = 0.5,
                   colsample.bytree = NULL,
                   keep.data = T,
                   is.verbose = F,
                   cv.folds = 4,
                   folds.id = c(rep(1, train.fraction*nrow(data)/4), rep(2, train.fraction*nrow(data)/4),
                                rep(3, train.fraction*nrow(data)/4), rep(4, train.fraction*nrow(data)/4)),
                   n.cores = 1,
                   weights = datasetFull$ExpoR)

  BT_algo <- do.call(BT, paramsBT)

  #BTFit_object, n.iter, rescale = FALSE, sort.it = FALSE
  # Test n.iter coming from validation.set
  n.iter <- BT_performance(BT_algo, "validation")
  # Extract trees of interest.



})
