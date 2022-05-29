#########################
# Author : Gireg Willame
# May 2022.
#
# Series of tests to check the initial inputs of the BT algorithm.
# Allows us to evaluate the check functions' performance.
#
########################

temp = T

testthat::test_that("Error thrown if no inputs defined at all",{
  expect_error(BT())
})

testthat::test_that("Same value",{
  expect_true(temp)
})

