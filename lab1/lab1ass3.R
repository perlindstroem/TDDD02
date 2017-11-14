library(readxl)
data <- read_excel("TDDE01/lab1/tecator.xlsx")

#divides the samples
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))

train=data[id,]
test=data[-id,]

#lengths were not equal
test=test[1:107,]

#par(mfrow=c(4,2))
#plot(data$Protein, data$Moisture)

n=6
MSE=numeric(n)
MSEtest=numeric(n)

for(i in 1:n) {
  model <- lm(train$Moisture ~ poly(train$Protein, i))
  pred <- predict(model, x=train$Protein)
  
  #plot(train$Protein, pred)

  MSE[i] <- 0
  for(ii in 1:length(predict)){
    MSE[i] <- MSE[i] + ((pred[ii] - train$Moisture[ii])**2)
  }
  MSE[i] <- MSE[i]/length(predict)
  
  MSEtest[i] <- 0
  for(ii in 1:length(predict)){
    MSEtest[i] <- MSEtest[i] + ((pred[ii] - test$Moisture[ii])**2)
  }
  MSEtest[i] <- MSEtest[i]/length(predict)
}

plot(MSE, ylab="MSE", xlab="Degree", xlim=c(1,6), ylim=c(6,22), col="blue", type="l")
par(new=T)
plot(MSEtest, ylab="MSE", xlab="Degree", xlim=c(1,6), ylim=c(6,22), type="l")
par(new=F)
par(mfrow=c(1,1))