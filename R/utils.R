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

  step_size <- (max(p_val) - min(p_val)) / 1000

  for (epsilon in seq(min(p_val),max(p_val),step_size)) {
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

.split_data <- function(x,y,random=TRUE,p=0.75){
  #y: vector
  #x: matrix
  #p: ratio of the negative cases to use for training set

  #split data into training and validation sets
  #validation must have posive examples, training set must have none


  #steps
  #shufle rows in full data set
  #separate into two sets, negatives and posives
  #move all postives to val,
  #split negatives between val and train sets
  #shuffle val


  # print(random)
  if (random==TRUE) {
    shuf_idx <- sample(1:length(y))
    x <- x[shuf_idx,]
    y <- y[shuf_idx]

    pos_idx <- which(y==1)

    pos_x <- x[pos_idx,]
    neg_x <- x[-pos_idx,]

    split_point <- floor((dim(neg_x)[1])*p) #split point for negative observations
    train_x <- neg_x[1:split_point,]
    # train_y <- rep(0,length(1:split_point))

    val_x <- neg_x[(split_point+1):(dim(neg_x)[1]),]
    val_y <- c(rep(0,dim(val_x)[1]),
               rep(1,length(pos_idx))) #combine negative and positive observations
    val_x <- unname(rbind(val_x,pos_x))

    val_shuf_idx <- sample(1:length(val_y))
    val_x <- val_x[val_shuf_idx,]
    val_y <- val_y[val_shuf_idx]
  }

  else if (random==FALSE) {
    #non-randomly split data into training and validation sets

    split_index <- c(1:(floor(nrow(x)/2)))

    train_x <- x[split_index,]
    # train_y <- y[split_index]
    val_x <- x[-split_index,]
    val_y <- y[-split_index]
  }

  else {
    stop("random must be logical TRUE or FALSE")
  }

  # ret <- list(train_x,train_y,val_x,val_y)
  ret <- list(train_x,val_x,val_y)
  names(ret) <- c("train_x","val_x","val_y")

  return(ret)
}

.mean2 <- function(x) {
  return(unname(apply(x,2,mean)))
}

.sd2 <- function(x) {
  #using sample standard deviation
  return(unname(apply(x,2,sd)))
}

.check_data <- function(x, y) {
  #check x and y for problems

  #check that x and y are numeric
  if ((!is.numeric(x)) | (!is.numeric(y))) {
    stop("Both x and y must be numeric.")
  }

  #check that y only contains 0 and 1, and that y contains both kinds of examples
  if (!identical(sort(unique(y)),c(0,1))) {
    stop("y must contain only 0 and 1, and both classes must be represented (normal = 0, anomaly = 1).")
  }

  #check that x and y don't contain NA
  if ((any(is.na(x))) | (any(is.na(y)))) {
    stop("NAs currently not supported.")
  }
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

