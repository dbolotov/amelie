context("ad object")

x1 <- c(1,.2,3,1,1,.7,-2,-1)
x2 <- c(0,.5,0,.4,0,1,-.3,-.1)
x <- do.call(cbind,list(x1,x2))
y <- c(0,0,0,0,0,0,1,1)
dframe <- data.frame(x,y)
set.seed(1234)
df_fit <- ad(y ~ x1 + x2, dframe)
set.seed(1234)
mat_fit <- ad(x = x, y = y)

test_that("class of fit object excludes formula when using default notation",{
  expect_identical(class(df_fit),c("ad.formula","ad"))
  expect_identical(class(mat_fit),c("ad"))
  expect_is(df_fit,c("ad"))
  expect_is(df_fit,c("ad.formula")) #check inheritance
  expect_is(mat_fit,c("ad")) #check inheritance
})


test_that("fit is the same for formula and matrix data",{
  expect_identical(df_fit$epsilon,mat_fit$epsilon)
  expect_identical(df_fit$train_x_mean,mat_fit$train_x_mean)
  expect_identical(df_fit$train_x_sd,mat_fit$train_x_sd)
  expect_identical(df_fit$train_predictions,mat_fit$train_predictions)
  expect_false(identical(df_fit$call,mat_fit$call))
})


test_that("fit object contains required attributes", {
  expect_equal(attributes(df_fit)$names,c("call", "univariate", "score",
                                          "epsilon", "train_x_mean",
                                          "train_x_sd", "val_score", "terms"))
  expect_equal(attributes(mat_fit)$names,c("call", "univariate", "score",
                                           "epsilon", "train_x_mean",
                                           "train_x_sd", "val_score"))
})


test_that("fail when x or y is not numeric", {
  x1 <- c("0",.2,3,1,4,.9,-2,-1)
  x2 <- c(0,.5,1,2.4,1.0,1,-.3,-.1)
  x <- do.call(cbind,list(x1,x2))
  y <- c(0,0,0,0,0,0,1,1)
  dframe <- data.frame(x,y)
  expect_error(ad(y ~ x1 + x2, dframe),"Both x and y must be numeric.", fixed = TRUE)
  expect_error(ad(x = x, y = y),"Both x and y must be numeric.", fixed = TRUE)
})

test_that("fail when y has anything other than 0 and 1", {
  x1 <- c(0,.2,3,1,4,.9,-2,-1)
  x2 <- c(0,.5,1,2.4,1.0,1,-.3,-.1)
  x <- do.call(cbind,list(x1,x2))
  y <- c(0,0,0,0,0,0,1,2)
  dframe <- data.frame(x,y)
  expect_error(ad(y ~ x1 + x2, dframe),"y must contain only 0 and 1, and both classes must be represented (normal = 0, anomaly = 1).",
               fixed = TRUE)

  y <- c(0,0,0,0,0,0,0,0)
  dframe <- data.frame(x,y)
  expect_error(ad(y ~ x1 + x2, dframe),"y must contain only 0 and 1, and both classes must be represented (normal = 0, anomaly = 1).",
               fixed = TRUE)

  y <- c(1,1,1,1,1,1,1,1)
  dframe <- data.frame(x,y)
  expect_error(ad(y ~ x1 + x2, dframe),"y must contain only 0 and 1, and both classes must be represented (normal = 0, anomaly = 1).",
               fixed = TRUE)

})


test_that("NAs are treated correctly", {
  x1 <- c(1,NA,3,1,4,.9,-2,-1)
  x2 <- c(0,.5,1,2.4,1.0,1,-.3,-.1)
  x <- do.call(cbind,list(x1,x2))
  y <- c(0,0,0,0,0,0,1,1)
  dframe <- data.frame(x,y)
  set.seed(142)
  df_fit <- ad(y ~ x1 + x2, dframe)
  expect_equal(df_fit$train_x_mean,c(1.966667,1.466667), tolerance = 0.00001)
  expect_equal(df_fit$epsilon,0.004755208)
})


test_that("fit object is printed with call and epsilon", {
  set.seed(321)
  x1 <- c(0,.2,3,1,1,-.8,-2,-1)
  x2 <- c(1,.2,0.4,-3,0,-1,-.3,-.1)
  x <- do.call(cbind,list(x1,x2))
  y <- c(0,0,0,0,0,1,1,1)
  dframe <- data.frame(x,y)
  df_fit <- ad(y ~ x1 + x2, dframe)
  mat_fit <- ad(x = x, y = y)

  expect_output(print(df_fit),"Call:\nad(formula = y ~ x1 + x2, data = dframe)\n\nepsilon: 0.0009140219", fixed = TRUE)
})

test_that("no errors when calling with univariate argument", {
  expect_silent(ad(y ~ x1 + x2, data = dframe, univariate = TRUE))
  expect_silent(ad(x = x, y = y, univariate = TRUE))
  expect_silent(ad(y ~ x1 + x2, data = dframe, univariate = FALSE))
  expect_silent(ad(x = x, y = y, univariate = FALSE))
})

test_that("ad matches expected values", {
  skip("skip for now")
  expect_equal(df_fit$train_x_mean,0)
  expect_equal(df_fit$train_x_sd,0)
  expect_equal(df_fit$epsilon,0)
  expect_equal(df_fit$val_score,0)
})


test_that("fail when score is not one of expected strings", {
  expect_error(ad(y ~ x1+x2, dframe, score = "foo"),
               "score must be one of 'f1' or 'mcc'.", fixed = TRUE)
  expect_error(ad(x, y, score = "bar"),
               "score must be one of 'f1' or 'mcc'.", fixed = TRUE)
})

test_that("fail when univariate is not logical", {
  expect_error(ad(y ~ x1+x2, dframe, univariate = 'foo'),
               "univariate must be logical.", fixed = TRUE)
  expect_error(ad(y ~ x1+x2, dframe, univariate = 3),
               "univariate must be logical.", fixed = TRUE)
})
