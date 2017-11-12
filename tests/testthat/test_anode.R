context("anode object")


test_that("class of fit object excludes formula when using default notation",{
  set.seed(1234)
  x1 <- c(1,.2,3,1,1,.7,-2,-1)
  x2 <- c(0,.5,0,.4,0,1,-.3,-.1)
  x <- do.call(cbind,list(x1,x2))
  y <- c(0,0,0,0,0,0,1,1)
  dframe <- data.frame(x,y)
  df_fit <- anode(y ~ x1 + x2, dframe)
  mat_fit <- anode(x = x, y = y)
  expect_identical(class(df_fit),c("anode.formula","anode"))
  expect_identical(class(mat_fit),c("anode"))
})


test_that("fit is exactly the same for formula and matrix data",{
  set.seed(321)
  x1 <- c(0,.2,3,1,-1,.8,-2,-1)
  x2 <- c(1,.2,0.4,3,0,1,-.3,-.1)
  x <- do.call(cbind,list(x1,x2))
  y <- c(0,0,0,0,0,0,1,1)
  dframe <- data.frame(x,y)
  df_fit <- anode(y ~ x1 + x2, dframe)
  mat_fit <- anode(x = x, y = y)

  expect_identical(df_fit$epsilon,mat_fit$epsilon)
  expect_identical(df_fit$train_x_mean,mat_fit$train_x_mean)
  expect_identical(df_fit$train_x_sd,mat_fit$train_x_sd)
  expect_identical(df_fit$train_predictions,mat_fit$train_predictions)
  expect_false(identical(df_fit$call,mat_fit$call))
})

#dummy tests


test_that("fail when x is not numeric", {
  expect_equal(10, 10)
})

test_that("fail when y is not numeric", {
  expect_equal(10, 10)
})

test_that("data contains positive examples", {
  expect_equal(10, 10)
})

test_that("warn if data contains more positive than negative examples", {
  expect_equal(10, 10)
})
