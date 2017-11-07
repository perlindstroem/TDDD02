knearest=function(data,k,newdata) {
  
  n1=dim(data)[1]
  n2=dim(newdata)[1]
  p=dim(data)[2]
  Prob=numeric(n2)
  X=as.matrix(data[,-p])
  Xn=as.matrix(newdata[-p])
  X=X/matrix(sqrt(rowSums(X^2)), nrow=n1, ncol=p-1)
  
  Xs <- sqrt(rowSums(X*X))
  Xns <- sqrt(rowSums(Xn*Xn))
  
  #MISSING: implement steps ii)-iv)
  
  for (i in 1:n2 ){
    #MISSING: use the computed distance matrix to find 
    #which observations are the nearest neighbors to case #i
    #MISSING: derive probability value 'Prob[i]' by using the
    #target values of the nearest neighbors
  }
  return(Prob)
}

data <- matrix(c(1,2,3,4,5,6), nrow = 3)

print(data)

result <- sqrt(rowSums(data*data))
print(result)

newdata <- matrix(c(2,2), nrow = 1)

knearest(data, 1, newdata)