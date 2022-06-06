#########################
# Author : Gireg Willame
# June 2022.
#
# Series of tests to check the BT_More function.
#
########################

testthat::test_that("BT_More checks",{
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
                   n.iter = 100,
                   train.fraction = 0.8,
                   interaction.depth = 4,
                   shrinkage = 0.01,
                   bag.fraction = 0.5,
                   colsample.bytree = NULL,
                   keep.data = F,
                   is.verbose = T,
                   cv.folds = 1,
                   folds.id = NULL,
                   n.cores = 1,
                   weights = datasetFull$ExpoR)

  BT_algo <- do.call(BT, paramsBT)

  #BT_more <- function(BTFit_object, new.n.iter=100, is.verbose=FALSE)

  # Expect error if not BT_type object.
  expect_error(BT_more(BTFit_object = "Test"))
  expect_error(BT_more(BTFit_object = list("a"=1, "b"=2)))

  # Expect error if n.iter is not a natural number.
  n.iter <- 0 ; expect_error(BT_more(BT_algo, new.n.iter = n.iter))
  n.iter <- c(3, 4) ; expect_error(BT_more(BT_algo, new.n.iter = n.iter))
  n.iter <- NULL ; expect_error(BT_more(BT_algo, new.n.iter = n.iter))
  n.iter <- NA ; expect_error(BT_more(BT_algo, new.n.iter = n.iter))
  n.iter <- "Text" ; expect_error(BT_more(BT_algo, new.n.iter = n.iter))
  n.iter <- F ; expect_error(BT_more(BT_algo, new.n.iter = n.iter))

  # Expect error if is.verbose is not a boolean
  is.verbose <- 1 ;  expect_error(BT_more(BT_algo, is.verbose = is.verbose))
  is.verbose <- 0.4 ; expect_error(BT_more(BT_algo, is.verbose = is.verbose))
  is.verbose <- 2.785 ; expect_error(BT_more(BT_algo, is.verbose = is.verbose))
  is.verbose <- c(3, 4) ; expect_error(BT_more(BT_algo, is.verbose = is.verbose))
  is.verbose <- NULL ; expect_error(BT_more(BT_algo, is.verbose = is.verbose))
  is.verbose <- NA ; expect_error(BT_more(BT_algo, is.verbose = is.verbose))
  is.verbose <- "Text" ; expect_error(BT_more(BT_algo, is.verbose = is.verbose))

  # Expect error if keep.data = F
  expect_error(BT_more(BT_algo))

})
