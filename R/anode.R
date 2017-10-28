#' Anomaly Detection
#'
#' @param formula An object of class "formula": a symbolic description of the
#' model to be fitted.
#' @param data a data frame containing the features (predictors) and target.
#' Features are assumed to be continuous, and target is assumed to be either
#' 0 or 1.
#'
#' @return An object of class \code{anode}:
#'   \item{call}{The original call to \code{anode}.}
#'   \item{epsilon}{The threshold value.}
#'
#' @details Details go here.
#' @examples
#' # Examples go here.
#' @export
anode <- function(formula, data) {

  feats <- data[,1:(dim(data)[2]-1)] #TEMPORARY IMPLEMENTATION; assumes target is last column

  #mean and variance
  feats_mean <- apply(feats,2,mean)
  feats_sd <- apply(feats,2,sd) #using sample standard deviation

  #product of probabilities
  feats_probs_prod <- rep(NA,nrow(feats))
  for (r in 1:nrow(feats)) {
    feats_probs_prod[r] <- prod(dnorm(as.numeric(feats[r,]), mean = feats_mean, sd = feats_sd, log = FALSE))
  }

  #optimize epsilon
  epsilon = 0.0001


  # create the return object
  call <- match.call()
  return_obj <- list(call = call,
                     epsilon = epsilon)
  class(return_obj) <- "anode"
  return(return_obj)
}







#' @export
print.anode <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
}

