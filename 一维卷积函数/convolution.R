convolve <- function(input1,input2) {
  x <- input1
  y <- input2
  if (length(input1) != length(input2)) {
    if (length(input2) > length(input1)) {
      x <- input2
      y <- input1
    } else{
      x <- input1
      y <- input2
    }
  }

  len <- length(x)+length(y)-1
  zero_repeat <- rep(x = 0,times=len-length(x))
  x_transform <- as.numeric(matrix(data = c(zero_repeat,x),nrow = 1,byrow = TRUE))
  zero_repeat <- rep(x = 0,times=len-length(y))
  y_transform <- as.numeric(matrix(data = c(zero_repeat,y),nrow = 1,byrow = TRUE))

  result <- numeric(length = len)

#  for(i in 1:len){
#    for(j in len:i){
#      result[i] <- result[i]+x_transform[j]*y_transform[len+i-j]
#    }
#  }

  for(i in 1:len){
    temp <- 0
    for(j in len:i){
       temp<- temp+x_transform[j]*y_transform[len+i-j]
    }
    result[i] <- temp
  }
  list(x=x,y=y,x_transform=x_transform,y_transform=y_transform,result=result)
}



