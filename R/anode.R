#' Anomaly Detection
#'
#' @param formula An object of class "formula": a symbolic description of the
#' model to be fitted.
#' @param data a data frame containing the features (predictors) and target.
#'
#' @return An object of class \code{anode}:
#'   \item{call}{The original call to \code{anode}.}
#'   \item{epsilon}{The threshold value.}
#'
#' @details Details go here.
#' Uses F1 score.
#' Features are assumed to be continuous, and target is assumed to be either
#' 0 or 1. NAs not supported.
#' @examples
#' # Examples go here.
#'
#'
#' @export
anode <- function(formula, data, val_data) {

  #val_data is validation set TEMPORARILY. should be created from data.

  x <- data[,1:(dim(data)[2]-1)] #TEMPORARY IMPLEMENTATION; assumes y (target) is last column
  y <- data[,dim(data)[2]]

  x_val <- data[,1:(dim(val_data)[2]-1)] #TEMPORARY IMPLEMENTATION; assumes y (target) is last column
  y_val <- data[,dim(val_data)[2]]


  #mean and variance
  x_mean <- apply(x,2,mean)
  x_sd <- apply(x,2,sd) #using sample standard deviation

  #product of probabilities
  x_probs_prod <- univariate_gaussian(x,x_mean,x_sd)

  x_val_probs_prod <- univariate_gaussian(x_val,x_mean,x_sd)



  #optimize epsilon
  epsilon <- get_epsilon(x_val_probs_prod,y_val) #should be on a cross-validation set, not on train set


  #compute predictions on training set
  train_preds <- x_probs_prod < epsilon

  #compute train error rate


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

