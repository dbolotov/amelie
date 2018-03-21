#' Compute the probability density function of a matrix of features.
#'
#' @param x A matrix of numeric features.
#' @param univariate Logical indicating whether the univariate pdf should be computed.
#'
#' @return A vector with values of the density function.
#'
#' @details
#'
#' \code{pdfunc} computes univariate or multivariate probabilities for a set of observations.
#'
#' All columns of a row are used in computing the pdf.
#'
#' Standard deviations are computed using \code{sd}, where denominator
#' \code{n-1} is used (sample standard deviation).
#'
#' @examples
#' dmat <- matrix(c(3,1,3,1,2,3,-1,0),nrow=2)
#' pdfunc(dmat,TRUE)
#'
#' @rdname pdfunc
#' @export
pdfunc <- function(x, univariate = TRUE) {

  #check univariate
  if (!is.logical(univariate)) stop("univariate must be logical.")

  #compute mean and sd
  x_mean <- .mean2(x)
  x_sd <- .sd2(x)

  if (univariate == TRUE) {
    .univariate_pdf(x,x_mean,x_sd)
  } else {
    .multivariate_pdf(x,x_mean,x_sd)
  }
}
