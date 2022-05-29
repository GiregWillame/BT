#########################
# Author : Gireg Willame
# May 2022.
#
# Series of tests to check the initial inputs of the BT algorithm.
# Allows us to evaluate the check functions' performance.
#
########################

# Start tests input parameters.
testthat::test_that("Error thrown if no inputs defined at all",{
  expect_error(BT())
})

testthat::test_that("Invalid tweedie power checks",{

  tweedie.power <- 0.5 ; expect_error(check_tweedie_power(tweedie.power))
  tweedie.power <- c(1,2,3) ; expect_error(check_tweedie_power(tweedie.power))
  tweedie.power <- NULL ; expect_error(check_tweedie_power(tweedie.power))
  tweedie.power <- NA ; expect_error(check_tweedie_power(tweedie.power))
  tweedie.power <- "Text" ; expect_error(check_tweedie_power(tweedie.power))

  tweedie.power <- 1 ; expect_silent(check_tweedie_power(tweedie.power))
  tweedie.power <- 0 ; expect_silent(check_tweedie_power(tweedie.power))
  tweedie.power <- 1.5 ; expect_silent(check_tweedie_power(tweedie.power))
  tweedie.power <- 2 ; expect_silent(check_tweedie_power(tweedie.power))

})

testthat::test_that("Invalid ABT checks",{

  ABT <- 1 ; expect_error(check_ABT(ABT))
  ABT <- c(3,4) ; expect_error(check_ABT(ABT))
  ABT <- NULL ; expect_error(check_ABT(ABT))
  ABT <- NA ; expect_error(check_ABT(ABT))
  ABT <- "Text" ; expect_error(check_ABT(ABT))

  ABT <- T ; expect_silent(check_ABT(ABT))
  ABT <- F; expect_silent(check_ABT(ABT))

})

testthat::test_that("Invalid n.iter checks",{

  n.iter <- 0 ; expect_error(check_n_iter(n.iter))
  n.iter <- c(3,4) ; expect_error(check_n_iter(n.iter))
  n.iter <- NULL ; expect_error(check_n_iter(n.iter))
  n.iter <- NA ; expect_error(check_n_iter(n.iter))
  n.iter <- "Text" ; expect_error(check_n_iter(n.iter))
  n.iter <- F ; expect_error(check_n_iter(n.iter))

  n.iter <- T ; expect_silent(check_n_iter(n.iter)) # TRUE == 1 -> Check if this is working through the entire program.
  n.iter <- 20 ; expect_silent(check_n_iter(n.iter))
  n.iter <- 1000 ; expect_silent(check_n_iter(n.iter))

})

testthat::test_that("Invalid train.fraction check",{

  train.fraction <- 0 ; expect_error(check_train_fraction(train.fraction))
  train.fraction <- 1.5 ; expect_error(check_train_fraction(train.fraction))
  train.fraction <- 2 ; expect_error(check_train_fraction(train.fraction))
  train.fraction <- c(4,5) ; expect_error(check_train_fraction(train.fraction))
  train.fraction <- c(0.5, 0.8) ; expect_error(check_train_fraction(train.fraction))
  train.fraction <- NULL ; expect_error(check_train_fraction(train.fraction))
  train.fraction <- NA ; expect_error(check_train_fraction(train.fraction))
  train.fraction <- "Text" ; expect_error(check_train_fraction(train.fraction))
  train.fraction <- F ; expect_error(check_train_fraction(train.fraction))

  train.fraction <- T ; expect_silent(check_train_fraction(train.fraction)) # TRUE == 1 -> Check if this is working through the entire program.
  train.fraction <- 1 ; expect_silent(check_train_fraction(train.fraction))
  train.fraction <- 0.4 ; expect_silent(check_train_fraction(train.fraction))
  train.fraction <- 0.7598 ; expect_silent(check_train_fraction(train.fraction))

})



