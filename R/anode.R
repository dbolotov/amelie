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
#'
#' @export
anode.formula <- function(formula, full_data) {
  call <- match.call()
  if (!inherits(formula, "formula"))
    stop("method is only for formula objects")
  m <- match.call(expand.dots = FALSE)
  if (identical(class(eval.parent(m$data)), "matrix"))
    m$data <- as.data.frame(eval.parent(m$data))
  # m$... <- NULL
  m[[1L]] <- quote(stats::model.frame)
  m <- eval(m, parent.frame())
  Terms <- attr(m, "terms")
  attr(Terms, "intercept") <- 0
}



#' @export
anode <- function(formula, full_data) {

  # #assume formula & data frame
  # m <- match.call(expand.dots = FALSE)
  # target <- as.character(formula[[2]])
  # print(m)

  #TEMPORARY: split full_data into data and val_data equally
  split_point <- floor(nrow(full_data)/2)
  data <- full_data[1:split_point,]
  val_data <- full_data[(split_point+1):nrow(full_data),]




  x <- data[,1:(dim(data)[2]-1)] #TEMPORARY IMPLEMENTATION; assumes y (target) is last column
  y <- data[,dim(data)[2]]

  x_val <- val_data[,1:(dim(val_data)[2]-1)] #TEMPORARY IMPLEMENTATION; assumes y (target) is last column
  y_val <- val_data[,dim(val_data)[2]]


  #mean and variance
  x_mean <- apply(x,2,mean)
  x_sd <- apply(x,2,sd) #using sample standard deviation

  #product of probabilities
  x_probs_prod <- .univariate_gaussian(x,x_mean,x_sd)

  x_val_probs_prod <- .univariate_gaussian(x_val,x_mean,x_sd)



  #optimize epsilon
  epsilon <- .op_epsilon(x_val_probs_prod,y_val)


  #compute predictions on training set
  train_preds <- x_probs_prod < epsilon

  #compute train error rate?


  # create the return object
  call <- match.call()
  return_obj <- list(call = call,
                     epsilon = epsilon,
                     train_preds = train_preds)
  class(return_obj) <- "anode"
  return(return_obj)
}







#' @export
print.anode <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
}

