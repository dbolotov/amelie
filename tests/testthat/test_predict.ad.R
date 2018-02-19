context("predict.anode function")

x1 <- c(1,.2,3,1,1,.7,-2,-1)
x2 <- c(0,.5,0,.4,0,1,-.3,-.1)
x <- do.call(cbind,list(x1,x2))
y <- c(0,0,0,0,0,0,1,1)
dframe <- data.frame(x,y)
names(dframe) <- c("x1","x2","y")
set.seed(1234)
df_fit <- anode(y ~ x1 + x2, dframe)
set.seed(1234)
mat_fit <- anode(x = x, y = y)

x1 <- c(NA,2,3,4)
x2 <- c(0.4,0.3,0.2,0.1)
x <- do.call(cbind,list(x1,x2))
y <- c(0,0,0,1)
dframe_test <- data.frame(x,y)
names(dframe_test) <- c("x1","x2","y")


test_that("fail when object does not inherit from anode", {
  a <- "a test string"
  expect_error(predict.anode(a,iris),"Object not of class anode.")
})

test_that("fail when type is not class or prob", {
  expect_error(predict(df_fit, newdata=dframe, type = 'foo'),
               "type must be either 'class' or 'prob'.",fixed = TRUE)
})

test_that("return class when type = class and probs when type = prob", {
  expect_equal(predict(df_fit, newdata=dframe, type = 'class'), c(0,0,1,0,0,0,1,1))
  expect_equal(predict(df_fit, newdata=dframe, type = 'prob'),
               c(1.506116e+00,1.805404e-05,4.770983e-42,1.970310e+00,
                 1.506116e+00,2.830568e-01,2.345398e-83,2.462731e-36),
               tolerance=0.0000001)
})

test_that("prediction on formula object behaves according to na.action", {
  expect_equal(predict(df_fit, newdata = dframe_test, type = 'class',
                       na.action = na.omit),c(1,1,1))
  expect_error(predict(df_fit, newdata = dframe_test, type = 'class',
                       na.action = na.fail),"missing values in object", fixed = TRUE)
  expect_equal(predict(df_fit, newdata = dframe_test, type = 'class'),c(NA,1,1,1))

})

test_that("prediction on default object errors when data has missing values", {
  expect_error(predict(mat_fit, newdata = dframe_test, type = 'class'),
               'newdata contains missing values.', fixed = TRUE)
})
