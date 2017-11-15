context("util functions")

test_that("f1 score is calculated correctly", {
  expect_equal(.f1_score(c(1,1,1),c(1,1,1)),1)
  expect_equal(.f1_score(c(1,1,1),c(0,0,1)),0.5)
  expect_equal(.f1_score(c(1,1,1,1,0),c(0,0,1,1,0)),2/3)
})


test_that("univariate gaussian is calculated correctly", {
  dmat <- matrix(c(1,2,1,4,3,5,2,1,3,2,1.1,2.2),nrow=4)
  dmclass <- matrix(c(0,0,1,1),nrow=4)


  expect_equal(.univariate_gaussian(1,2,3),0)

  expect_equal(0,0)
})

#dummy tests


test_that("data split works correctly", {
  expect_equal(0,0)
})

test_that("epsilon optimization works correctly", {
  expect_equal(0,0)
})
