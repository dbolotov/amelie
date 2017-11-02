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
#'@importFrom stats sd


#'@export
anode <- function(x, ...){
  UseMethod("anode")
}

#'@rdname anode
#'@export
anode.formula <- function(formula, data, na.action = na.omit) {
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

  ret <- anode.default(x, y, na.action = na.action)
  ret$call <- call
  ret$call[[1]] <- as.name("anode")
  ret$terms <- Terms
  if (!is.null(attr(m, "na.action")))
    ret$na.action <- attr(m, "na.action")
  class(ret) <- c("anode.formula", class(ret))
  return (ret)
}



#' @rdname anode
#' @export
anode.default <- function(x, y, na.action = na.omit) {

  #split data into train and validation sets
  split_obj <- data_split(x,y)
  train_x <- split_obj$train_x
  train_y <- split_obj$train_y
  val_x <- split_obj$val_x
  val_y <- split_obj$val_y


  #mean and variance
  train_x_mean <- apply(train_x,2,mean)
  train_x_sd <- apply(train_x,2,sd) #using sample standard deviation

  #product of probabilities
  train_x_probs_prod <- .univariate_gaussian(train_x,train_x_mean,train_x_sd)

  val_x_probs_prod <- .univariate_gaussian(val_x,train_x_mean,train_x_sd)



  #optimize epsilon
  epsilon <- .op_epsilon(val_x_probs_prod,val_y)


  #compute predictions on training set
  train_preds <- train_x_probs_prod < epsilon

  #compute train error rate?


  # create the return object
  call <- match.call()
  return_obj <- list(call = call,
                     epsilon = epsilon,
                     train_preds = train_preds)
  class(return_obj) <- "anode"
  return(return_obj)
}


#' @rdname anode
#' @export
print.anode <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
}

