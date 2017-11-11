#utility functions

#'@importFrom stats dnorm na.omit model.extract model.matrix
#'
.univariate_gaussian <- function(x,x_mean,x_sd) {
  probs <- rep(NA,nrow(x))
  for (r in 1:nrow(x)) {
    probs[r] <- prod(dnorm(as.numeric(x[r,]), mean = x_mean, sd = x_sd, log = FALSE))
  }
  return(probs)
}


.f1_score <- function(y_hat,y) {
  #assumes that "1" is the positive result
  tp <- sum((y_hat==1) & (y==1))
  fp <- sum((y_hat==1) & (y==0))
  fn <- sum((y_hat==0) & (y==1))
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  return(2*precision*recall/(precision+recall))
}

.op_epsilon <- function(p_val,y_val) {
  #optimization as implemented in mlclass.org

  #p_val: probability values from validation set
  #y_val: target values from validation set

  best_epsilon <- 0
  best_f1 <- 0
  f1 <- 0

  stepsize <- (max(p_val) - min(p_val)) / 1000

  for (epsilon in seq(min(p_val),max(p_val),stepsize)) {
    predictions <- (p_val < epsilon) #? this makes it so predictions will be all 0 for 1st round of for loop
    f1 <- .f1_score(predictions,y_val)

    if(is.nan(f1)){f1<-0} #matlab/octave implementation will return 0 if comparing Nan with a number. R will not.

    if (f1 > best_f1) {
      best_f1 <- f1
      best_epsilon <- epsilon
    }
  }
  return(best_epsilon)
}

.split_data <- function(x,y){


  #randomly split data into train and val sets; ensure val set contains positive cases
  split_index <- 0

  #randomly split data into training and validation sets
  # split_index <- sample.int(n = nrow(x), size = floor(.5*nrow(x)), replace = FALSE)

  #non-randomly split data into training and validation sets
  split_index <- c(1:(floor(nrow(x)/2)))

  train_x <- x[split_index,]
  train_y <- y[split_index]
  val_x <- x[-split_index,]
  val_y <- y[-split_index]

  ret <- list(train_x,train_y,val_x,val_y)
  names(ret) <- c("train_x","train_y","val_x","val_y")

  return(ret)
}









#
# #product of probabilities
# x_probs_prod <- rep(NA,nrow(x))
# for (r in 1:nrow(x)) {
#   x_probs_prod[r] <- prod(dnorm(as.numeric(x[r,]), mean = x_mean, sd = x_sd, log = FALSE))
# }
#
