# function that create dummies based on month
month_num <- function(date){
  mth <- month(date)
  vec <- rep(0, 12)
  vec[mth] <- 1
  return(matrix(vec[1:11], 1))
}
