### Processing Normolite column to plot ----------------------------------------

norm_deriv <- function(col){
  index <- which(diff(col) < -1)
  new <- col[1:index[1]]
  
  for (i in 2:(length(index) + 1)){
    value <- sum(col[index[1:i - 1]])
    if (i == (length(index) + 1)){
      aux <- col[(index[i - 1] + 1):length(col)] + value
    } else {
      aux <- col[(index[i - 1] + 1):index[i]] + value
    }
    new <- c(new, aux)
  }
  c(diff(new, lag = 5)/5, rep(NA, 5))
}