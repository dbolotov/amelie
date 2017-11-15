#' anode: supervised anomaly detection with normal probability
#' functions.
#'
#' @param formula An object of class "formula": a symbolic description of the
#' model to be fitted.
#' @param data A data frame containing the features (predictors) and target.
#' @param x A matrix of numeric features.
#' @param y A matrix of numeric predicted values, either 0 or 1, with 1
#' assumed to be anomalous.
#' @param na.action A function specifying the action to be taken if NAs are
#' found.
#' @param ... Currently not used.
#'
#' @return An object of class \code{anode}:
#'   \item{call}{The original call to \code{anode}.}
#'   \item{epsilon}{The threshold value.}
#'
#' @details Details go here.
#'
#' \code{anode} implements anomaly detection as described in the Coursera Machine
#' Learning course.
#'
#' The approach is set up as a binary classification problem. Features are
#' assumed to be continuous, and the target is assumed to take on values of 0
#' (negative case, no anomaly) or 1 (positive case, anomaly).
#'
#' The threshold \code{epsilon} is optimized using the F1 score.
#'
#' NAs are supported in the following way: TODO EXPLANATION.
#'
#' @examples
#' # Examples go here.
#'
#'@importFrom stats sd


#'@export
anode <- function(x, ...){
  UseMethod("anode")
}

#'@rdname anode
#'@export
anode.formula <- function(formula, data, na.action = na.omit, ...) {
  call <- match.call()
  if (!inherits(formula, "formula"))
    stop("method is only for formula objects")
  m <- match.call(expand.dots = FALSE)
  if (identical(class(eval.parent(m$data)), "matrix"))
    m$data <- as.data.frame(eval.parent(m$data))

  m[[1L]] <- quote(stats::model.frame)
  m$na.action <- na.action
  m <- eval(m, parent.frame())
  Terms <- attr(m, "terms")
  attr(Terms, "intercept") <- 0

  x <- model.matrix(Terms, m)
  y <- model.extract(m, "response")
  attr(x, "na.action") <- attr(y, "na.action") <- attr(m, "na.action")

  return_object <- anode.default(x, y, na.action = na.action)
  return_object$call <- call
  return_object$call[[1]] <- as.name("anode")
  return_object$terms <- Terms
  if (!is.null(attr(m, "na.action")))
    return_object$na.action <- attr(m, "na.action")
  class(return_object) <- c("anode.formula", class(return_object))
  return (return_object)
}



#' @rdname anode
#' @export
anode.default <- function(x, y, na.action = na.omit, ...) {

  #split data into train and validation sets
  split_obj <- .split_data(x,y)
  train_x <- split_obj$train_x
  train_y <- split_obj$train_y
  val_x <- split_obj$val_x
  val_y <- split_obj$val_y


  #mean and variance
  train_x_mean <- unname(apply(train_x,2,mean))
  train_x_sd <- unname(apply(train_x,2,sd)) #using sample standard deviation

  train_x_mean <- .mean2(train_x)
  train_x_sd <- .sd2(train_x) #using sample standard deviation

  #product of probabilities
  train_x_probs_prod <- .univariate_gaussian(train_x,train_x_mean,train_x_sd)

  val_x_probs_prod <- .univariate_gaussian(val_x,train_x_mean,train_x_sd)



  #optimize epsilon
  epsilon <- .op_epsilon(val_x_probs_prod,val_y)


  #compute predictions on training set?
  train_predictions <- as.numeric(train_x_probs_prod < epsilon)

  #compute train error rate?


  # create the return object
  call <- match.call()
  return_obj <- list(call = call,
                     epsilon = epsilon,
                     train_x_mean = train_x_mean,
                     train_x_sd = train_x_sd,
                     train_predictions = train_predictions)
  class(return_obj) <- "anode"
  return(return_obj)
}


#' @rdname anode
#' @export
print.anode <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\n")
  cat("epsilon: ",x$epsilon,sep="")
}

