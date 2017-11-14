
ROC=function(Y, Yfit, p) {
  m=length(p)
  TPR=numeric(m)
  FPR=numeric(m)
  
  for(i in 1:m) {
    t=table(Yfit>p[i], Y)
    
    TP = t[2,2]
    FP = t[2,1]
    TN = t[1,1]
    FN = t[1,2]
    
    TPR[i]=TP/(TP+FN)
    FPR[i]=FP/(FP+TN)
  }
  plot(FPR, TPR, xlim=c(0,1), ylim=c(0,1))
  abline(a=0,b=1)
  return (list(TPR=TPR,FPR=FPR))
}

library(kknn)

p <- seq(from=0.05, to=0.95, by=0.05)

result <- kknn(Spam ~ ., as.data.frame(train), as.data.frame(test), k = 5)

fit <- fitted(result)

fitt <- floor(fit + 0.5)
#print(fitt)

roc = ROC(as.data.frame(test)$Spam, fit, p)
print(roc)


#cm <- table(
#  factor(as.data.frame(test)$Spam, labels=c("Actual not spam", "Actual spam")),
#  factor(fit, labels=c("Guessed not spam", "Guessed spam"))
#);

#print(cm)

