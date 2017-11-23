library(tree)
library(boot)

data = read.csv(file = "TDDE01/lab2/assignment3/State.csv", header=T, sep=";")
data$EX = as.numeric(data$EX)
data$MET = as.numeric(data$MET)
data = data[order(data$EX),]

set.seed(12345)

f = function(data2, ind) {
  data1 = data2[ind, ]
  fit = tree(EX ~ MET, data=data1)
  pred = predict(fit, newdata = data)
  return(pred)
}

f1 = function(data1) {
  fit = tree(EX ~ MET, data1, control = tree.control(48, minsize = 8))
  pred = predict(fit, newdata = data1)
  return(pred)
}

rng = function(data, mle) {
  data1 = data.frame(EX = data$EX, MET = data$MET)
  n = length(data$EX)
  data1$EX = rnorm(n, predict(mle, newdata=data1), sd(resid(mle)))
  return(data1)
}

q2 = function() {
  plot(data$MET, data$EX, ylab="EX", xlab="MET")
  # model ???
  
  fit = tree(EX ~ MET, data, control = tree.control(48, minsize = 8))
  fitted = predict(fit, newdata=data)
  #plot(fit)
  text(fit, pretty=0)
  
  points(data$MET, fitted, col="BLUE")
  
  residual = resid(fit)
  #hist(residual)
}

q4 = function() {
  mle = tree(EX ~ MET, data, control = tree.control(48, minsize = 8))
  #fitted = predict(fit, newdata=data)
  res = boot(data, statistic = f1, R = 1000, mle=mle, ran.gen = rng, sim="parametric")
  e = envelope(res)
  
  plot(data$MET, data$EX, ylab="EX", xlab="MET")
  points(data$MET, e$point[2,], col="blue")
  points(data$MET, e$point[1,], col="blue")
  
}

q3 = function() {
  res = boot(data, f, R=1000)
  e = envelope(res)
  
  plot(data$MET, data$EX, ylab="EX", xlab="MET")
  points(data$MET, e$point[2,], col="blue")
  points(data$MET, e$point[1,], col="blue")
}

q3()