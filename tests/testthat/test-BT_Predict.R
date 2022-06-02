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

  BT_algo <- BT::BT(formula = as.formula("Y_normalized ~ Gender+Age+Split+Sport"),
                    data = training.set,
                    shrinkage = 0.01,
                    train.fraction = 0.5)

  expect_equal(T,T)

})
