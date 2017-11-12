context("predict.anode function")


test_that("fail when object does not inherit from anode", {
  a <- "a test string"
  expect_error(predict.anode(a,iris),"object not of class anode")
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

  expect_equal(attributes(df_fit)$names,c("call","epsilon","train_x_mean",
                                          "train_x_sd","train_predictions",
                                          "terms"))
  expect_equal(attributes(mat_fit)$names,c("call","epsilon","train_x_mean",
                                           "train_x_sd","train_predictions"))
})

#dummy tests

test_that("anode.formula", {
  expect_equal(0,0)
})

test_that("anode.default", {
  expect_equal(0,0)
})
