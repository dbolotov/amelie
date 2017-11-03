#' Predict Method for anode Objects
#'
#' @param object An object of class \code{anode}, created by the function \code{anode}.
#' @param newdata A data frame or matrix containing new data.
#' @param na.action TODO
#' @param ... Currently not used.
#'
#' @return A vector of predicted values.
#'
#' @details Details go here.
#'
#' @examples
#' # Examples go here.
#'
#'@importFrom stats delete.response model.frame
#'
#'
#'@export
predict.anode <- function(object, newdata, na.action, ...) {
  if (!inherits(object, "anode"))
    stop("object not of class anode")

  if (inherits(object, "anode.formula")) {
    newdata <- as.data.frame(newdata)
    rn <- row.names(newdata)
    Terms <- delete.response(object$terms)
    x <- model.frame(Terms, newdata, na.action = na.omit)
  } else { #must inherit from anode.default
    if (is.null(dim(newdata)))
      dim(newdata) <- c(1, length(newdata))
    x <- newdata
    if (nrow(x) == 0)
      stop("newdata has 0 rows")
    if (any(is.na(x)))
      stop("missing values in newdata")
  }

  epsilon <- object$epsilon
  train_x_mean <- object$train_x_mean
  train_x_sd <- object$train_x_sd


  #prediction
  newdata_probs_prod <- .univariate_gaussian(x,train_x_mean,train_x_sd)

  predictions <- (newdata_probs_prod < epsilon)

  return(predictions)
}
