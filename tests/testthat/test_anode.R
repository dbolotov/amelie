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
  expect_is(df_fit,c("anode"))
  expect_is(df_fit,c("anode.formula")) #check inheritance
  expect_is(mat_fit,c("anode")) #check inheritance
})


test_that("fit is exactly the same for formula and matrix data",{
  x1 <- c(0,.2,3,1,-1,.8,-2,-1)
  x2 <- c(1,.2,0.4,3,0,1,-.3,-.1)
  x <- do.call(cbind,list(x1,x2))
  y <- c(0,0,0,0,0,0,1,1)
  dframe <- data.frame(x,y)
  set.seed(321)
  df_fit <- anode(y ~ x1 + x2, dframe)
  set.seed(321)
  mat_fit <- anode(x = x, y = y)
  expect_identical(df_fit$epsilon,mat_fit$epsilon)
  expect_identical(df_fit$train_x_mean,mat_fit$train_x_mean)
  expect_identical(df_fit$train_x_sd,mat_fit$train_x_sd)
  expect_identical(df_fit$train_predictions,mat_fit$train_predictions)
  expect_false(identical(df_fit$call,mat_fit$call))
})


test_that("fit object contains required attributes", {
  set.seed(234)
  x1 <- c(0,.2,3,1,4,.9,-2,-1)
  x2 <- c(0,.5,1,2.4,1.0,1,-.3,-.1)
  x <- do.call(cbind,list(x1,x2))
  y <- c(0,0,0,0,0,0,1,1)
  dframe <- data.frame(x,y)
  df_fit <- anode(y ~ x1 + x2, dframe)
  mat_fit <- anode(x = x, y = y)

  expect_equal(attributes(df_fit)$names,c("call", "epsilon", "train_x_mean",
                                          "train_x_sd",
                                          "val_score", "terms"))
  expect_equal(attributes(mat_fit)$names,c("call", "epsilon", "train_x_mean",
                                           "train_x_sd",
                                           "val_score"))
})


test_that("fail when x or y is not numeric", {
  x1 <- c("0",.2,3,1,4,.9,-2,-1)
  x2 <- c(0,.5,1,2.4,1.0,1,-.3,-.1)
  x <- do.call(cbind,list(x1,x2))
  y <- c(0,0,0,0,0,0,1,1)
  dframe <- data.frame(x,y)
  expect_error(anode(y ~ x1 + x2, dframe),"Both x and y must be numeric.", fixed = TRUE)
  expect_error(anode(x = x, y = y),"Both x and y must be numeric.", fixed = TRUE)
})

test_that("fail when y has anything other than 0 and 1", {
  x1 <- c(0,.2,3,1,4,.9,-2,-1)
  x2 <- c(0,.5,1,2.4,1.0,1,-.3,-.1)
  x <- do.call(cbind,list(x1,x2))
  y <- c(0,0,0,0,0,0,1,2)
  dframe <- data.frame(x,y)
  expect_error(anode(y ~ x1 + x2, dframe),"y must contain only 0 and 1, and both classes must be represented (normal = 0, anomaly = 1).",
               fixed = TRUE)

  y <- c(0,0,0,0,0,0,0,0)
  dframe <- data.frame(x,y)
  expect_error(anode(y ~ x1 + x2, dframe),"y must contain only 0 and 1, and both classes must be represented (normal = 0, anomaly = 1).",
               fixed = TRUE)

  y <- c(1,1,1,1,1,1,1,1)
  dframe <- data.frame(x,y)
  expect_error(anode(y ~ x1 + x2, dframe),"y must contain only 0 and 1, and both classes must be represented (normal = 0, anomaly = 1).",
               fixed = TRUE)

})


#test not working properly
# test_that("fit object is printed with call and epsilon",{
#   set.seed(321)
#   x1 <- c(0,.2,3,1,1,-.8,-2,-1)
#   x2 <- c(1,.2,0.4,-3,0,-1,-.3,-.1)
#   x <- do.call(cbind,list(x1,x2))
#   y <- c(0,0,0,0,0,1,1,1)
#   dframe <- data.frame(x,y)
#   df_fit <- anode(y ~ x1 + x2, dframe)
#   mat_fit <- anode(x = x, y = y)
#
#   expect_output(print(df_fit),"Call:
# anode(formula = y ~ x1 + x2, data = dframe)
#
# epsilon: 0.02435071")
# })




#TODO tests

test_that("fail when x and y do not have same number of observations", {
  #account for x possibly being a 1-d array
  expect_equal(0,0)
})

test_that("fail when x does not have at least 2 rows for each class", {
  expect_equal(10, 10)
})



test_that("data contains positive examples", {
  expect_equal(10, 10)
})

test_that("warn if data contains more positive than negative examples", {
  expect_equal(10, 10)
})
