library(anode)
context("util functions")

#f1 score function
test_that("f1 score is calculated correctly", {
  expect_equal(f1_score(c(1,1,1),c(1,1,1)),1)
  expect_equal(f1_score(c(1,1,1),c(0,0,1)),0.5)
  expect_equal(f1_score(c(1,1,1,1,0),c(0,0,1,1,0)),2/3)
})

#epsilon optimization function
test_that("asdf", {
  expect_equal(0,0)
})
