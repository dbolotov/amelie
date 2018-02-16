#' Predict Method for anode Objects
#'
#' @param object An object of class \code{anode}, created by the function \code{anode}.
#' @param newdata A data frame or matrix containing new data.
#' @param type One of 'class' (for class prediction) or 'prob' (for probabilities).
#' @param na.action TODO
#' @param ... Currently not used.
#'
#' @return A vector of predicted values.
#'
#' @details Specifying 'class' for \code{type} returns the class of each
#' observation as anomalous or non-anomalous. Specifying 'prob' returns the
#' probability of each observation.
#'
#' @examples
#' # Examples go here.
#'
#'@importFrom stats delete.response model.frame
#'
#'
#'@export
predict.anode <- function(object, newdata, type = 'class', na.action = na.pass, ...) {
  if (!inherits(object, "anode"))
    stop("Object not of class anode.")

  if (!(type %in% c('class','prob'))){
    stop("type must be either 'class' or 'prob'.")
  }

  if (inherits(object, "anode.formula")) {
    newdata <- as.data.frame(newdata)
    rn <- row.names(newdata)
    Terms <- delete.response(object$terms)
    x <- model.frame(Terms, newdata, na.action = na.action)
  } else { #must inherit from anode.default
    if (is.null(dim(newdata)))
      dim(newdata) <- c(1, length(newdata))
    x <- newdata
    if (nrow(x) == 0)
      stop("newdata has 0 rows.")
    if (any(is.na(x)))
      stop("newdata contains missing values.")
  }

  epsilon <- object$epsilon
  train_x_mean <- object$train_x_mean
  train_x_sd <- object$train_x_sd


  #prediction
  newdata_probs_prod <- .univariate_gaussian(x,train_x_mean,train_x_sd)
  if (type == 'class') {
    predictions <- as.numeric(newdata_probs_prod < epsilon)
    return(predictions)
  } else if (type == 'prob') {
    return(newdata_probs_prod)
  }
}
