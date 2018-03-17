#utility functions

#'@importFrom stats dnorm na.omit model.extract model.matrix

.univariate_pdf <- function(x,x_mean,x_sd) {
  probs <- rep(NA,NROW(x))
  for (r in 1:NROW(x)) {
    probs[r] <- prod(dnorm(as.numeric(x[r,]), mean = x_mean, sd = x_sd, log = FALSE))
  }
  return(probs)
}

.multivariate_pdf <- function(x,x_mean,x_sd) {
  probs <- rep(NA,NROW(x))
  n <- length(x_mean)
  x_sd_2 <- diag(x_sd^2)

  x_no_m <- as.matrix(sweep(x,2,x_mean))

  probs <- ((2*pi)^(-n/2)) * ((det(x_sd_2))^(-0.5)) * exp(-0.5 * rowSums((x_no_m %*% solve(x_sd_2)) * x_no_m))
  return(probs)
}


.f1_score <- function(y_hat, y) {
  #assumes that "1" is the positive result
  tp <- sum((y_hat==1) & (y==1))
  fp <- sum((y_hat==1) & (y==0))
  fn <- sum((y_hat==0) & (y==1))
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  return(2*precision*recall/(precision+recall))
}

.mcc <- function(y_hat, y) {
  #assumes that "1" is the positive result
  tp <- sum((y_hat==1) & (y==1))
  fp <- sum((y_hat==1) & (y==0))
  fn <- sum((y_hat==0) & (y==1))
  tn <- sum((y_hat==0) & (y==0))

  return((tp*tn - fp*fn)/sqrt((tp+fp)*(tp+fn)(tn+fp)*(tn+fn)))
}

.op_epsilon <- function(p_val,y_val) {
  #p_val: probability values from validation set
  #y_val: target values from validation set

  best_epsilon <- 0
  best_f1 <- 0
  f1 <- 0

  step_size <- (max(p_val) - min(p_val)) / 100

  # print(c("best_f1","f1","epsilon"))
  for (epsilon in seq(min(p_val),max(p_val),step_size)) {

    predictions <- (p_val < epsilon) #? this makes it so predictions will be all 0 for 1st round of for loop
    f1 <- .f1_score(predictions,y_val)

    # print(c(best_f1, f1, best_epsilon, epsilon))



    if(is.nan(f1)){f1<-0} #matlab/octave implementation will return 0 if comparing Nan with a number. R will not.

    if (f1 > best_f1) {
      # print('best')
      best_f1 <- f1
      best_epsilon <- epsilon
    }
  }
  return(best_epsilon)
}

.split_data <- function(x,y,random=TRUE,p=0.75){
  #split data into training and validation sets
  #validation must have posive examples, training set must have none

  #y: vector
  #x: matrix
  #p: ratio of the negative cases to use for training set

  #steps for random split:
  #shufle rows in full data set
  #separate into two sets, negatives and posives
  #move all postives to val,
  #split negatives between val and train sets
  #shuffle val that now has positive and negative examples

  #steps for non-random split:
  #first part of p goes to train, second part goes to val


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

    # val_shuf_idx <- sample(1:length(val_y))
    # val_x <- val_x[val_shuf_idx,]
    # val_y <- val_y[val_shuf_idx]
  }

  else if (random==FALSE) {
    #non-randomly split data into training and validation sets, regardless of class
    split_point <- floor((length(y))*p)

    train_x <- x[1:split_point,]
    val_x <- x[-c(1:split_point),]
    val_y <- y[-c(1:split_point)]
  }

  else {
    stop("random must be logical TRUE or FALSE")
  }

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

  #check that x and y are same length
  if (NROW(x) != length(y)) {
    stop("x and y must have the same number of observations.")
  }

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
