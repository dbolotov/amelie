#utility functions

#'@importFrom stats dnorm na.omit model.extract model.matrix

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

.split_data <- function(x,y,random=FALSE){
  #split data into training and validation sets
  #validation must have posive examples, training none
  #should training set be allowed to have positive examples?

  #steps
  #shufle rows in full data set
  #separate into two sets, negatives and posives
  #move all postives to val, 1/3 of negatives to val, and 2/3 of the negatives to train
  #shuffle val
  print(random)
  if (random==TRUE) {
    full_shuffle_index <- sample(1:length(y))
    print(full_shuffle_index)
    x <- x[full_shuffle_index,]
    y <- y[full_shuffle_index]

    pos_index <- which(y==1)

    pos_x <- x[pos_index,]
    #pos_y is implicitly all 1
    neg_x <- x[-pos_index,]

    split_point <- floor((length(y)-length(pos_index))*.75)
    train_x <- neg_x[1:split_point,]
    train_y <- rep(0,length(1:split_point))

    val_x <- neg_x[(split_point+1):(length(y)-length(pos_index)),]
    val_x <- rbind(val_x,pos_x)
    val_y <- c(rep(0,length((split_point+1):(length(y)-length(pos_index)))),
               rep(1,length(pos_index))) #combine negative and positive observations
  }

  else if (random==FALSE) {
    #non-randomly split data into training and validation sets

    split_index <- c(1:(floor(nrow(x)/2)))

    train_x <- x[split_index,]
    train_y <- y[split_index]
    val_x <- x[-split_index,]
    val_y <- y[-split_index]
  }

  else {
    stop("random must be logical TRUE or FALSE")
  }

  ret <- list(train_x,train_y,val_x,val_y)
  names(ret) <- c("train_x","train_y","val_x","val_y")

  return(ret)
}

.mean2 <- function(x) {
  return(unname(apply(x,2,mean)))
}

.sd2 <- function(x) {
  #using sample standard deviation
  return(unname(apply(x,2,sd)))
}







#
# #product of probabilities
# x_probs_prod <- rep(NA,nrow(x))
# for (r in 1:nrow(x)) {
#   x_probs_prod[r] <- prod(dnorm(as.numeric(x[r,]), mean = x_mean, sd = x_sd, log = FALSE))
# }
#
#randomly split data into training and validation sets
# split_index <- sample.int(n = nrow(x), size = floor(.5*nrow(x)), replace = FALSE)

