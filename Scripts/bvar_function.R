#### INPUT
# H = Number of time points you want to forecast
# R = number of iterations for Gibbs sampler
# Y = time series (output from function 'setup')
# Z (big) (output from function 'setup')
# z (small) = (output from function 'setup')
# p = number of lags (output from function 'setup')

#### OUTPUT
# predY = A list, each folder in the list is a (R x H) matrix


BVAR_forecast <- function(H, R, Y, Z, z, p, date = as.Date("2017-01-01")){
  m <- ncol(Y)
  k <- ncol(Z)
  t <- nrow(Z)
  T <- t+p
  # Parameters
  Gamma_hat <- solve(t(Z) %*% Z) %*% t(Z) %*% Y  
  S <- t(Y - Z %*% Gamma_hat) %*% (Y - Z %*% Gamma_hat)
  P <- solve(t(Z) %*% Z)
  
  # Set up
  Gamma <- matrix(0, ncol=R, nrow=(ncol(Gamma_hat)*nrow(Gamma_hat)))
  psi <- matrix(0, ncol=R, nrow=m*m)
  mat <- rbind(Z, matrix(0, ncol=k, nrow=H))
  pred <- matrix(0, ncol=m, nrow=H)
  predY <- list()
  for(o in 1:m){
    predY[[o]] <- matrix(0, ncol=H, nrow=R)
  }
  
  ## GIBBS
  IW <- matrix(0, m, m)
  for(j in 1:R){
    # Sample Psi
    IW <- riwish(T-k, S) 
    psi[,j] <- matrix(IW, ncol=1) 
    
    Q <- IW
    # Sample Gamma
    Gamma[,j] <- matrix(mvrnorm(1, as.vector(Gamma_hat), kronecker(Q, P)), ncol=1) 
    
    # Predict Y_T+1 : Y_T+H
    for(h in 1:H){
      # Sample u for every m:th time series at time point t
      u <- as.vector(mvrnorm(1, rep(0, m), IW))
      if(h == 1){
        # predict Y_T+1
        pred[h,] <- z %*% matrix(Gamma[,j], ncol=m) + u
      }else{
        # predict Y_T+h
        pred[h,] <- matrix(mat[t+h-1,],1) %*% matrix(Gamma[,j], ncol=m) + u
      }
      for(i in 1:m){
        # put the predicted values in the right list
        # (one folder in the list for each time series)
        predY[[i]][j,h] <- pred[h,i]
      }
      
      # add predicted values to Z so it's possible to predict next value.
      # these values will be replaced for every iteration in Gibbs sampler.
      if(p > 1){
        mat[t+h,] <- cbind(1, matrix(pred[h,], 1), Z[nrow(Z), ncol(Y)+1]+h, matrix(mat[t+h-1, 1:(m*(p-1))+1],1), month_num(date+h))
      }else if(p == 1){
        mat[t+h,] <- cbind(1, matrix(pred[h,], 1), Z[nrow(Z), ncol(Y)+1]+h, month_num(date+h))
      }
    }
  }
  return(predY)
}