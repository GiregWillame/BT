#########################
# Author : Gireg Willame
# June 2022.
#
# Series of tests to check the BT_Predict function.
#
########################

testthat::test_that("BT_Predict function checks",{

  # Create datasets.
  set.seed(4)
  # training set.
  n <- 500000 # size of training set (number of observations)

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

  training.set <- data.frame(Y,Gender,Age,Split,Sport,ExpoR, Y_normalized)

  # validation set
  n.val <- 1500000 # size of validation set (number of observations)

  Gender <- factor(sample(c("male","female"),n.val,replace=TRUE))
  Age <- sample(c(18:65),n.val,replace=TRUE)
  Split <- factor(sample(c("yes","no"),n.val,replace=TRUE))
  Sport <- factor(sample(c("yes","no"),n.val,replace=TRUE))

  lambda <- 0.1*ifelse(Gender=="male",1.1,1)
  lambda <- lambda*(1+1/(Age-17)^0.5)
  lambda <- lambda*ifelse(Sport=="yes",1.15,1)

  ExpoR <- runif(n.val)

  Y <- rpois(n.val, ExpoR*lambda)
  Y_normalized <- Y/ExpoR

  test.set <- data.frame(Y,Gender,Age,Split,Sport,ExpoR,lambda, Y_normalized)

  # Additional parameters.
  tweedie.power <- 1
  respVar <- "Y_normalized"
  w <- "ExpoR"

  paramsList <- list(formula = as.formula("Y_normalized ~ Gender+Age+Split+Sport"),
                     data = training.set,
                     n.iter = 200,
                     train.fraction = 0.5,
                     interaction.depth = 2,
                     shrinkage = 0.01,
                     bag.fraction = 0.5,
                     colsample.bytree = 2,
                     keep.data = F,
                     is.verbose = F,
                     cv.folds = 1,
                     folds.id = NULL)

  BT_algo <- do.call(BT, paramsList)

  trainSet <- training.set[seq(1, paramsList$train.fraction*nrow(training.set)),]
  valSet <- training.set[setdiff(seq(1, nrow(training.set)), seq(1, paramsList$train.fraction*nrow(training.set))),]

  # prediction with 20 iters.
  pred_trainBT20 <- predict(BT_algo, trainSet, n.iter = 20)
  pred_valBT20 <- predict(BT_algo, valSet, n.iter = 20)
  # prediction with 100 iters.
  pred_trainBT100 <- predict(BT_algo, trainSet, n.iter = 100)
  pred_valBT100 <- predict(BT_algo, valSet, n.iter = 100)

  glmTrain <- log(BT_algo$BTInit$initFit$fitted.values)
  glmVal <- predict(BT_algo$BTInit$initFit, newdata = valSet, type = 'link')
  predTrain <- list() ; predVal <- list()
  for (iTree in seq(1, 100)){
    predTrain_current <- log(predict(BT_algo$BTIndivFits[[iTree]], newdata = trainSet, type = 'vector'))
    predVal_current <- log(predict(BT_algo$BTIndivFits[[iTree]], newdata = valSet, type = 'vector'))
    if (iTree == 1){
      predTrain[[iTree]] <- glmTrain + (BT_algo$BTParams$shrinkage * predTrain_current)
      predVal[[iTree]] <- glmVal + (BT_algo$BTParams$shrinkage * predVal_current)
    }else{
      predTrain[[iTree]] <- predTrain[[iTree-1]] + (BT_algo$BTParams$shrinkage * predTrain_current)
      predVal[[iTree]] <- predVal[[iTree-1]] + (BT_algo$BTParams$shrinkage * predVal_current)
    }
  }

  # Comparison with 20 iterations.
  expect_equal(pred_trainBT20, unname(unlist(predTrain[[20]])))
  expect_equal(pred_valBT20, unname(unlist(predVal[[20]])))

  # Comparison with 100 iterations.
  expect_equal(pred_trainBT100, unname(unlist(predTrain[[100]])))
  expect_equal(pred_valBT100, unname(unlist(predVal[[100]])))

})
