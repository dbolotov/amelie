#utility functions


univariate_gaussian <- function(x,x_mean,x_sd) {
  probs <- rep(NA,nrow(x))
  for (r in 1:nrow(x)) {
    probs[r] <- prod(dnorm(as.numeric(x[r,]), mean = x_mean, sd = x_sd, log = FALSE))
  }
  return(probs)
}


f1_score <- function(x,y) {
  #assumes that "1" is the positive result
  tp <- sum((x==1) & (y==1))
  fp <- sum((x==1) & (y==0))
  fn <- sum((x==0) & (y==1))
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  return(2*precision*recall/(precision+recall))
}

op_epsilon <- function(p_val,y_val) {
  #optimization as implemented in mlclass.org

  #p_val: probability values from validation set
  #y_val: target values from validation set

  best_epsilon = 0
  best_f1 = 0
  f1 = 0

  stepsize <- (max(probs) - min(probs)) / 1000

  for (epsilon in seq(min(probs),max(probs),stepsize)) {
    predictions <- (p_val < epsilon)
    f1 <- f1_score(predictions,yval)

    if (f1 > best_f1) {
      best_f1 <- f1
      best_epsilon <- epsilon
    }
  }
  return(epsilon)
}

#
# #product of probabilities
# x_probs_prod <- rep(NA,nrow(x))
# for (r in 1:nrow(x)) {
#   x_probs_prod[r] <- prod(dnorm(as.numeric(x[r,]), mean = x_mean, sd = x_sd, log = FALSE))
# }
#
