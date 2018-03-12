#' pdfunc: compute probability density function.
#'
#' @param data A data frame containing the features (predictors).
#' @param x A matrix of numeric features.
#' assumed to be anomalous.
#' @param univariate Logical indicating whether the univariate pdf should be computed.
#' @param na.action A function specifying the action to be taken if NAs are
#' found.
#' @param ... Currently not used.
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
#' @references
#' \url{https://www.coursera.org/learn/machine-learning}

#' @rdname pdfunction
#' @export
pdfunc <- function() {
  .univariate_pdf(x,x_mean,x_sd)
  .multivariate_pdf(x,x_mean,x_sd)
}
