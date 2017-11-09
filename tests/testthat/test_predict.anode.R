context("predict.anode function")


test_that("fail when object does not inherit from anode", {
  a <- "a test string"
  expect_error(predict.anode(a,iris),"object not of class anode")
})

#dummy tests

test_that("anode.formula", {
  expect_equal(0,0)
})

test_that("anode.default", {
  expect_equal(0,0)
})
