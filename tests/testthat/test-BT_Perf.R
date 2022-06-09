#########################
# Author : Gireg Willame
# June 2022.
#
# Series of tests to check the BT_Perf function.
#
########################

# BT.perf <- function(BTFit_object,
#                     plot.it=TRUE,
#                     oobag.curve=FALSE,
#                     overlay=TRUE,
#                     method,
#                     main="")

testthat::test_that("Check the BT_Perf function - Inputs",{

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
                   bag.fraction = 1,
                   colsample.bytree = NULL,
                   keep.data = T,
                   is.verbose = F,
                   cv.folds = 1,
                   folds.id = NULL,
                   n.cores = 1,
                   weights = datasetFull$ExpoR)

  BT_algo <- do.call(BT, paramsBT)

  # Checks the inputs values.
  # Check BT Object.
  expect_error(BT.perf("Stupid Object"))
  expect_error(BT.perf(list(a=c(1,2,3))))

  # plot.it has to be boolean.
  plot.it <- 1 ;  expect_error(BT.perf(BT_algo, plot.it = plot.it))
  plot.it <- 0.4 ; expect_error(BT.perf(BT_algo, plot.it = plot.it))
  plot.it <- 2.785 ; expect_error(BT.perf(BT_algo, plot.it = plot.it))
  plot.it <- c(3, 4) ; expect_error(BT.perf(BT_algo, plot.it = plot.it))
  plot.it <- NULL ; expect_error(BT.perf(BT_algo, plot.it = plot.it))
  plot.it <- NA ; expect_error(BT.perf(BT_algo, plot.it = plot.it))
  plot.it <- "Text" ; expect_error(BT.perf(BT_algo, plot.it = plot.it))

  # Same goes for overlay and oobag.curve
  oobag.curve <- 1 ;  expect_error(BT.perf(BT_algo, oobag.curve = oobag.curve))
  oobag.curve <- 0.4 ; expect_error(BT.perf(BT_algo, oobag.curve = oobag.curve))
  oobag.curve <- 2.785 ; expect_error(BT.perf(BT_algo, oobag.curve = oobag.curve))
  oobag.curve <- c(3, 4) ; expect_error(BT.perf(BT_algo, oobag.curve = oobag.curve))
  oobag.curve <- NULL ; expect_error(BT.perf(BT_algo, oobag.curve = oobag.curve))
  oobag.curve <- NA ; expect_error(BT.perf(BT_algo, oobag.curve = oobag.curve))
  oobag.curve <- "Text" ; expect_error(BT.perf(BT_algo, oobag.curve = oobag.curve))

  overlay <- 1 ;  expect_error(BT.perf(BT_algo, overlay = overlay))
  overlay <- 0.4 ; expect_error(BT.perf(BT_algo, overlay = overlay))
  overlay <- 2.785 ; expect_error(BT.perf(BT_algo, overlay = overlay))
  overlay <- c(3, 4) ; expect_error(BT.perf(BT_algo, overlay = overlay))
  overlay <- NULL ; expect_error(BT.perf(BT_algo, overlay = overlay))
  overlay <- NA ; expect_error(BT.perf(BT_algo, overlay = overlay))
  overlay <- "Text" ; expect_error(BT.perf(BT_algo, overlay = overlay))

  # Method has to be either 'validation', 'cv' or 'OOB' (if defined)
  method <- 'Test' ; expect_error(BT.perf(BT_algo, method = method))
  method <- 1 ;  expect_error(BT.perf(BT_algo, method = method))
  method <- 0.4 ; expect_error(BT.perf(BT_algo, method = method))
  # method <- 2.785 ; expect_error(BT.perf(BT_algo, method = method))
  method <- c(3, 4) ; expect_error(BT.perf(BT_algo, method = method))
  method <- NULL ; expect_error(BT.perf(BT_algo, method = method))
  method <- NA ; expect_error(BT.perf(BT_algo, method = method))
  method <- T ; expect_error(BT.perf(BT_algo, method = method))
  method <- F ; expect_error(BT.perf(BT_algo, method = method))

  # Main has to be mainly text - managed by plot function.

})


testthat::test_that("Check the BT_Perf function - Validation only",{

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
                   bag.fraction = 1,
                   colsample.bytree = NULL,
                   keep.data = T,
                   is.verbose = F,
                   cv.folds = 1,
                   folds.id = NULL,
                   n.cores = 1,
                   weights = datasetFull$ExpoR)

  BT_algo <- do.call(BT, paramsBT)

  # Not all methods allowed.
  method <- 'cv' ; expect_error(BT.perf(BT_algo, method = method))
  method <- 'OOB' ;  expect_error(BT.perf(BT_algo, method = method))

  # Check results.
  expect_message(BT_perf_validation <- BT.perf(BT_algo, plot.it = F), "Using validation method...")
  # Plotted results.
  expect_message(BT_perf_validation <- BT.perf(BT_algo, plot.it = T), "Using validation method...")
  # Plotted results with overlay (no changes, only for OOB).
  expect_message(BT_perf_validation <- BT.perf(BT_algo, plot.it = T, overlay = T), "Using validation method...")
  # No OOB curve expected.
  expect_error(BT.perf(BT_algo, plot.it = T, oobag.curve = T))

  # No error expected if plot.it is set to False, whatever the other values.
  expect_message(BT_perf_validation <- BT.perf(BT_algo, plot.it = F, oobag.curve = T), "Using validation method...")

  # Should be similar to the one with the correct method called.
  BT_perf_validation_rerun <- BT.perf(BT_algo, plot.it = F, method = 'validation')

  expect_equal(class(BT_perf_validation), class(BT_perf_validation))
  expect_equal(BT_perf_validation, BT_perf_validation_rerun)

  expect_equal(BT_perf_validation, which.min(BT_algo$BTErrors$validation.error))
  # Visual checks on the produce plot.
  plot(seq(1, 100), BT_algo$BTErrors$training.error, type = 'l')
  lines(seq(1,100), BT_algo$BTErrors$validation.error, col = 'red')

})

testthat::test_that("Check the BT_Perf function - OOB only",{

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
                   train.fraction = 1,
                   interaction.depth = 4,
                   shrinkage = 0.01,
                   bag.fraction = 0.8,
                   colsample.bytree = NULL,
                   keep.data = T,
                   is.verbose = F,
                   cv.folds = 1,
                   folds.id = NULL,
                   n.cores = 1,
                   weights = datasetFull$ExpoR)

  BT_algo <- do.call(BT, paramsBT)


  # Not all methods allowed.
  method <- 'validation' ; expect_error(BT.perf(BT_algo, method = method))
  method <- 'cv' ;  expect_error(BT.perf(BT_algo, method = method))

  # Check results.
  expect_message(BT_perf_OOB <- BT.perf(BT_algo, plot.it = F), "Using OOB method...")
  # Plotted results.
  expect_message(BT_perf_OOB <- BT.perf(BT_algo, plot.it = T), "Using OOB method...")
  # Plotted results with overlay.
  expect_message(BT_perf_OOB <- BT.perf(BT_algo, plot.it = T, overlay = T), "Using OOB method...")
  # Plot results with OOB curve.
  expect_message(BT_perf_OOB <- BT.perf(BT_algo, plot.it = T, oobag.curve = T), "Using OOB method...")

  # Should be similar to the one with the correct method called.
  BT_perf_OOB_rerun <- BT.perf(BT_algo, plot.it = F, method = 'OOB')

  expect_equal(class(BT_perf_OOB), class(BT_perf_OOB_rerun))
  expect_equal(BT_perf_OOB, BT_perf_OOB_rerun)

  # OOB errors are smoothed before taking the best iterations.
  # Use similar info to reproduce the smoother...
  x <- seq_len(BT_algo$BTParams$n.iter)
  smoother <- loess(BT_algo$BTErrors$oob.improvement~x,
                    enp.target=min(max(4,length(x)/10),50))
  smoother$y <- smoother$fitted
  smoother$x <- x
  expect_equal(smoother$x[which.min(-cumsum(smoother$y))], BT_perf_OOB)

})
