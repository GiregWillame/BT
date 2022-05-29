#########################
# Author : Gireg Willame
# May 2022.
#
# Series of tests to check the outputs of the BT algorithm.
# Allows us to evaluate if the outputs are expected.
#
########################

# Create datasets.
set.seed(4)

n <- 500000 # size of dataset (number of observations)

Gender <- factor(sample(c("male","female"),n,replace=TRUE))
Age <- sample(c(18:65),n,replace=TRUE)
Split <- factor(sample(c("yes","no"),n,replace=TRUE))
Sport <- factor(sample(c("yes","no"),n,replace=TRUE))

lambda <- 0.1*ifelse(Gender=="male",1.1,1)
lambda <- lambda*(1+1/(Age-17)^0.5)
lambda <- lambda*ifelse(Sport=="yes",1.15,1)

ExpoR <- runif(n) # rep(1,n) # ExpoR = 1

Y <- rpois(n, ExpoR*lambda)
Y_normalized <- Y/ExpoR

dataset <- data.frame(Y,Gender,Age,Split,Sport,ExpoR, Y_normalized)

# Parameters list.
# initial_parametersList = list(formula = as.formula("Y_normalized ~ Gender+Age+Split+Sport"),
#                          data = dataset,
#                          tweedie.power = 1,
#                          ABT = TRUE,
#                          n.iter = 100,
#                          train.fraction = 1,
#                          interaction.depth = 4,
#                          shrinkage = 1,
#                          bag.fraction = 1,
#                          colsample.bytree = NULL,
#                          keep.data = TRUE,
#                          is.verbose = FALSE,
#                          cv.folds = 1,
#                          folds.id = NULL,
#                          n.cores = 1,
#                          tree.control = rpart::rpart.control(xval = 0, maxdepth = 4, cp = 0, minsplit = 2),
#                          weights = NULL)
#
#
# # Start tests of the algorithm.
# testthat::test_that("Simple run as such",{
#   expect_success(do.call(BT, initial_parametersList))
# })
