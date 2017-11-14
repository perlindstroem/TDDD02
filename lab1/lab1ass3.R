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

par(mfrow=c(4,2))
plot(data$Protein, data$Moisture)

for(i in 1:6) {
  model <- lm(train$Moisture ~ poly(train$Protein, i))
  pred <- predict(model, x=train$Protein)
  
  plot(train$Protein, pred)
  
  MSE <- sum(1)
}

par(mfrow=c(1,1))