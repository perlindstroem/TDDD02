library(mboost)
library(kernlab)

setwd("~/TDDE01/exam_2016-01-09")
bf <- read.csv2("bodyfatregression.csv")
set.seed(1234567890)
m <- blackboost(Bodyfat_percent ~ Waist_cm+Weight_kg, data=bf) 
mstop(m)
cvf <- cv(model.weights(m), type="kfold")
cvm <- cvrisk(m, folds=cvf, grid=1:100)
plot(cvm)
mstop(cvm)

# SVM
data(spam)

index <- sample(1:dim(spam)[1])
spamtrain <- spam[index[1:floor(dim(spam)[1]/2)], ]
spamtest <- spam[index[((ceiling(dim(spam)[1]/2)) + 1):dim(spam)[1]], ]

m1 <- ksvm(type~.,data=spamtrain,kernel="vanilladot",C=1,cross=2)
m2 <- ksvm(type~.,data=spamtrain,kernel="rbfdot",kpar=list(sigma=0.01),C=1,cross=2)
m3 <- ksvm(type~.,data=spamtrain,kernel="rbfdot",kpar=list(sigma=0.05),C=1,cross=2)
m4 <- ksvm(type~.,data=spamtrain,kernel="vanilladot",C=5,cross=2)
m5 <- ksvm(type~.,data=spamtrain,kernel="rbfdot",kpar=list(sigma=0.01),C=5,cross=2)
m6 <- ksvm(type~.,data=spamtrain,kernel="rbfdot",kpar=list(sigma=0.05),C=5,cross=2)

mailtype <- predict(filter,spamtest[,-58])

## Check results
table(mailtype,spamtest[,58])