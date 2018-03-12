#' pdfunc: compute the probability density function.
#'
#' @param x A matrix of numeric features.
#' assumed to be anomalous.
#' @param univariate Logical indicating whether the univariate pdf should be computed.
#'
#' @return A list containing the following:
#'   \item{univariate}{TODO}
#'   \item{x_mean}{TODO}
#'   \item{x_sd}{TODO}
#'   \item{probs}{TODO}
#'
#' @details
#'
#' \code{pdfunc} computes univariate or multivariate probabilities for a set of observations.

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
