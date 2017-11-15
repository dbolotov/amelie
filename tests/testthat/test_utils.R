context("util functions")

test_that("f1 score is calculated correctly", {
  expect_equal(.f1_score(c(1,1,1),c(1,1,1)),1)
  expect_equal(.f1_score(c(1,1,1),c(0,0,1)),0.5)
  expect_equal(.f1_score(c(1,1,1,1,0),c(0,0,1,1,0)),2/3)
})


test_that(".mean2 is calculated correctly", {
  dmat <- matrix(c(1,2,1,4,3,5,2,1,3,2,1.1,2.2),nrow=4)
  expect_equal(.mean2(dmat),c(2.000, 2.750, 2.075))
  expect_equal(.mean2(dmat[1:2,1:2]),c(1.5, 4))
  expect_null(names(.mean2(dmat)))
})


test_that(".sd2 is calculated correctly", {
  dmat <- matrix(c(1,2,1,4,3,5,2,1,3,2,1.1,2.2),nrow=4)
  expect_equal(.sd2(dmat), c(1.4142136, 1.7078251, 0.7804913), tolerance = 0.00002)
  expect_equal(.sd2(dmat[1:2,1:2]), c(0.7071068, 1.4142136), tolerance = 0.00002)
  expect_null(names(.sd2(dmat)))
})


# test_that("univariate gaussian is calculated correctly", {
#   dmat <- matrix(c(1,2,1,4,3,5,2,1,3,2,1.1,2.2),nrow=4)
#   dmclass <- matrix(c(0,0,1,1),nrow=4)
#
#
#   expect_equal(.univariate_gaussian(1,2,3),0)
#
#   expect_equal(0,0)
# })

#dummy tests


test_that("data split works correctly", {
  expect_equal(0,0)
})

test_that("epsilon optimization works correctly", {
  expect_equal(0,0)
})
