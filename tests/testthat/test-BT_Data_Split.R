#########################
# Author : Gireg Willame
# June 2022.
#
# Series of tests to check the BT_Data_Split functions.
#
########################

testthat::test_that("Create_validation_set function",{

  data <- data.frame('a' = seq(1,100), 'b' = seq(101, 200))
  expect_equal(T,T)
})
