# function that create dummies based on month
month_num <- function(date){
  mth <- month(date)
  vec <- rep(0, 12)
  vec[mth] <- 1
  return(matrix(vec[1:11], 1))
}


# This function should give me the right matrices that will be used when forecasting with BVAR.

#### INPUT
# matrix = matrix with time series (T x m)
# p = number of lags you want
# date = sequence of dates for when the data was sampled (only if seasonal = TRUE)

#### OUTPUT
# Z (big) = intercept, lags and dummies for t = 1, ..., T-p
# Y = m time series for t = p+1, ..., T
# T = length of time series
# k = number of coefficients
# p = number of lags
# d = number of deterministics variables
# m = number of time series
# z (small) = explanatory variables for time point T+1, will be the values for the first forecast in BVAR

setup_data <- function(matrix, p=1, date=NULL){
  m <- ncol(matrix) # Tells how many variables there are
  T <- nrow(matrix) # How many time points are observed
  
  lags_var <- list()
  Z <- cbind(rep(1, T-p), matrix[p:(T-(1)),]) # Z with intercept and lag 1
  if(p > 1){ # add other lags to Z
    for(i in 1:(p-1)){ 
      lags_var[[i]] <- matrix[(p-i):(T-(i+1)),]
      Z <- cbind(Z, lags_var[[i]]) # lag > 1
    }
  }
  
  if(trend){
    Z <- cbind(Z, 1:nrow(Z))
  }
  
  d <- 0  
  if(seasonal){ # add seasonal dummies to Z
    dummies <- matrix(0, ncol=11, nrow=(T-p))
    wd <- date[(p+1):length(date)]
    for(i in (p):(length(date)-p)){
      dummies[i,] <- month_num(wd[i])
    }
    d <- 11
    Z <- cbind(Z, dummies)
  }
  
  Y <- matrix[(p+1):nrow(matrix),] # Fix Y depending on how many lags there are
  colnames(Y) <- rownames(Y) <- colnames(Z) <- rownames(Z) <- NULL # remove ugly rownames
  
  d <- 1+d # intercept + dummies 
  k <- m*p+d # number of coefficients
  p <- p # number of lags
  
  # Fix z (small) that will be the values for the first forecast in BVAR
  o <- matrix(Y[nrow(Y):(nrow(Y)-p+1),], ncol=m)
  r <- o[1,]
  if(p > 1){
    for(i in 2:p){
      r <- cbind(r, o[i,])
    }
  }
  
  # add trend-feature
  r <- c(r, as.vector(Z[nrow(Y), 5]+1))
  
  # add month-dummies
  z <- matrix(c(1, as.vector(r), month_num(wd[i]+1)), nrow=1) 
  
  return(list(Z = Z, Y = Y, T = T, k = k, p = p, d = d, m = m, z=z))
}

setup_result <- setup_data(matrix = as.matrix(data_train[,2:4]), 
                            p = 1,
                            date = data_train[,1] %>% pull(),
                            seasonal = TRUE)
