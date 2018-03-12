context("pdfunc")

test_that("univ pdfunc result matches .univariate_pdf", {
  dmat <- matrix(c(3,1,3,1,2,3,-1,0),nrow=2)
  x_mean <- .mean2(dmat)
  x_sd <- .sd2(dmat)
  expect_equal(.univariate_pdf(dmat,x_mean,x_sd),
               pdfunc(dmat))
})

test_that("multiv pdfunc result matches .multivariate_pdf", {
  dmat <- matrix(c(3,1,3,1,2,3,-1,0),nrow=2)
  x_mean <- .mean2(dmat)
  x_sd <- .sd2(dmat)
  expect_equal(.multivariate_pdf(dmat,x_mean,x_sd),
               pdfunc(dmat,FALSE))
})

test_that("pdfunc result is different for multv and univ", {
  dmat <- matrix(c(3,1,3,1,2,3,-1,0),nrow=2)
  expect_false(isTRUE(all.equal(pdfunc(dmat,TRUE),pdfunc(dmat,FALSE),tolerance=1.0e-20)))
})
