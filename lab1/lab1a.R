#imports the excel-file
library(readxl)
data <- read_excel("TDDE01/lab1/spambase.xlsx")
#View(spambase)

#divide data into training and test
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

spambase <- data.matrix(spambase)

train <- data.matrix(train)
test <- data.matrix(test)

knearest=function(data,k,newdata) {
  
  n1=dim(data)[1]
  n2=dim(newdata)[1]
  p=dim(data)[2]
  Prob=numeric(n2)
  
  X=as.matrix(data[,-p])
  Xclass=as.matrix(data[,p])
  X=X/matrix(sqrt(rowSums(X^2)), nrow=n1, ncol=p-1)
  
  Y=as.matrix(newdata[,-p])
  Yclass=as.matrix(newdata[,p])
  Y=Y/matrix(sqrt(rowSums(Y^2)), nrow=n1, ncol=p-1)
  
  PredClass = rep(0,n2)
  
  C <- X %*% t(Y)
  D <- 1-C
  
  CM <- matrix(0L, nrow = 2, ncol = 2)
  
  for (i in 1:n2 ){
    Drow <- D[i,, drop=F]
    Drow <- t(Drow)
    Drow <- cbind(Drow,Xclass)
    Drow <- Drow[order(Drow[,1]),]
    Drow <- head(Drow,k)
    
    PredClass[i] <- if(sum(Drow[,2] > 0)/k > 0.5) 1 else 0; 
    
    if(PredClass[i] == 1 && Yclass[i] == 1) CM[1,1] = CM[1,1] + 1
    if(PredClass[i] == 1 && Yclass[i] == 0) CM[1,2] = CM[1,2] + 1
    if(PredClass[i] == 0 && Yclass[i] == 1) CM[2,1] = CM[2,1] + 1
    if(PredClass[i] == 0 && Yclass[i] == 0) CM[2,2] = CM[2,2] + 1
  }
  
  print(1-(CM[1,1]+CM[2,2])/sum(CM))
  
  print(CM)
  return(null)
}

knearest(train, 1, train)