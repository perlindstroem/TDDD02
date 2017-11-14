library(readxl)
data <- read_excel("TDDE01/lab1/machines.xlsx")

theta <- seq(from=0.00, to=3, by=0.01)

loglik <- function(x, theta) {
  ll <- numeric(length(theta))
  
  for(i in 1:(length(theta))){
    p <- theta[i]*exp((-theta[i]*x))
    ll[i] <- (log(prod(p))/dim(x)[1])
  }
  
  return(list(LogLik=ll, theta=theta))
}

loglik_b <- function(x, theta) {
  ll <- numeric(length(theta))
  
  for(i in 1:(length(theta))){
    p <- theta[i]*exp((-theta[i]*x))
    pre <- 10*exp(1)**(-theta[i]*10)
    ll[i] <- (log(prod(p)*pre)/dim(x)[1])
  }
  
  return(list(LogLik=ll, theta=theta))
}

result <- loglik(data, theta)
result2 <- loglik(head(data,6), theta)
result3 <- loglik_b(data, theta)

rand <- rexp(50,rate=1.13)
#View(rand)

#set.seed(12345)
#hist(rand)
#datam <- as.matrix(data)
#hist(datam)

#par(mfrow=c(2,1))
plot(result$theta, result$LogLik, xlab="Theta", ylab="Log Likelihood", xlim=c(0,3), ylim=c(-5,0), col="blue")
par(new=T)
plot(result2$theta, result2$LogLik, xlab="Theta", ylab="Log Likelihood", xlim=c(0,3), ylim=c(-5,0), col="red")
par(new=F)
par(new=T)
plot(result3$theta, result3$LogLik, xlab="Theta", ylab="Log Likelihood", xlim=c(0,3), ylim=c(-5,0), col="green")
par(new=F)
#par(mfrow=c(1,1))

#View(result)
#View(result2)
#View(cbind(result3$LogLik, result3$theta))