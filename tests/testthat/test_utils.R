context("util functions")

test_that("f1 score is calculated correctly", {
  expect_equal(.f1_score(c(1,1,1),c(1,1,1)),1)
  expect_equal(.f1_score(c(1,1,1),c(0,0,1)),0.5)
  expect_equal(.f1_score(c(1,1,1,1,0),c(0,0,1,1,0)),2/3)
})

test_that("mcc is calculated correctly", {
  expect_equal(.mcc(c(1,1,1),c(1,1,1)),0)
  expect_equal(.mcc(c(1,1,1),c(0,0,1)),0)
  expect_equal(.mcc(c(1,1,1,1,0),c(0,0,1,1,0)),0.4082483)
  expect_equal(.mcc(c(0,0,1),c(0,0,1)),1)
  expect_equal(.mcc(c(0,0,0),c(0,0,0)),0)
  expect_equal(.mcc(c(1,1,1,1,0),c(0,0,0,0,1)),-1)
})

test_that(".score is calculated correctly", {
  expect_equal(.score(c(1,1,1,1,0),c(0,0,1,1,0),"f1_score"),
               .f1_score(c(1,1,1,1,0),c(0,0,1,1,0)))
  expect_equal(.score(c(1,1,1,1,0),c(0,0,1,1,0),"mcc"),
               .mcc(c(1,1,1,1,0),c(0,0,1,1,0)))
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


test_that("univariate pdf is calculated correctly 1", {
  dmat <- matrix(c(3,1,3,1,2,3,-1,0),nrow=2)
  expect_equal(.univariate_pdf(dmat,c(0,0,0,0),c(1,1,1,1)),
               c(2.565983e-07, 1.035191e-04), tolerance = 0.00002)

})

test_that("univariate pdf is calculated correctly 2", {
  dmat <- matrix(c(3,1,3,1,2,3,-1,0),nrow=2)
  x_mean <- .mean2(dmat)
  x_sd <- .sd2(dmat)
  expect_equal(.univariate_pdf(dmat,x_mean,x_sd),
               c(0.009318495, 0.009318495), tolerance = 0.00002)

})

test_that("multivariate pdf is calculated correctly 1 ", {
  dmat <- matrix(c(3,1,3,1,2,3,-1,0),nrow=2)
  x_mean <- c(0,0,0,0)
  x_sd <- c(1,1,1,1)
  expect_equal(.multivariate_pdf(dmat,x_mean,x_sd),
               c(2.565983e-07, 1.035191e-04), tolerance = 0.00002)

})


test_that("multivariate pdf is calculated correctly 2", {
  dmat <- matrix(c(3,1,3,1,2,3,-1,0),nrow=2)
  x_mean <- .mean2(dmat)
  x_sd <- .sd2(dmat)
  expect_equal(.multivariate_pdf(dmat,x_mean,x_sd),
               c(0.009318495, 0.009318495), tolerance = 0.00002)

})



x1 <- c(2,3,4,5,6,7,8,9,10)
x2 <- c(22,33,44,55,66,77,88,99,110)
x <- do.call(cbind,list(x1,x2))
y <- c(0,0,0,0,0,0,0,0,1)

test_that("random data split works correctly 1", {
  set.seed(101)
  dat <- .split_data(x, y)
  expect_equal(dat$val_y,c(0,0,1))
  expect_equal(dat$train_x[,1],c(5,2,6,3,8,7))
  expect_equal(dat$val_x[,1],c(9,4,10))
})

test_that("random data split works correctly 2", {
  set.seed(232)
  dat <- .split_data(x, y, p = 0.5)
  expect_equal(dat$val_y,c(0,0,0,0,1))
  expect_equal(dat$train_x[,1],c(9,2,4,5))
  expect_equal(dat$val_x[,1],c(6,3,8,7,10))
})

test_that("non-random data split works correctly", {
  x1 <- c(2,3,4,5,6,7,8,9,10,11)
  x2 <- c(22,33,44,55,66,77,88,99,111,222)
  x <- do.call(cbind,list(x1,x2))
  y <- c(0,0,0,0,0,0,0,1,0,1)
  dat <- .split_data(x, y, random = FALSE, p = 0.66)
  expect_equal(dat$val_y,c(0,1,0,1))
  expect_equal(dat$train_x[,1],c(2,3,4,5,6,7))
  expect_equal(dat$val_x[,1],c(8,9,10,11))
})


test_that("fail when x and y are of different lengths", {
  y <- c(0,0,0)
  expect_error(ad(x = x, y = y),
               'x and y must have the same number of observations',
               fixed = TRUE)
})



# test_that("epsilon optimization works correctly", {
#   expect_equal(0,0)
# })
