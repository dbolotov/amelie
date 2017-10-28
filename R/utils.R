#utility functions


univariate_gaussian <- function(x,x_mean,x_sd) {
  probs <- rep(NA,nrow(x))
  for (r in 1:nrow(x)) {
    probs[r] <- prod(dnorm(as.numeric(x[r,]), mean = x_mean, sd = x_sd, log = FALSE))
  }
  return(probs)
}

get_epsilon <- function(probs,y_val) {
  ##TODO: implement function

  return(0.0001)
}

#
# #product of probabilities
# x_probs_prod <- rep(NA,nrow(x))
# for (r in 1:nrow(x)) {
#   x_probs_prod[r] <- prod(dnorm(as.numeric(x[r,]), mean = x_mean, sd = x_sd, log = FALSE))
# }
#
