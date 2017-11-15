library(readxl)
library(glmnet)
data <- read_excel("TDDE01/lab1/tecator.xlsx", header=F)

covariates=scale(data[,2:101])
response=scale(data[,102])


lasso.model=cv.glmnet(as.matrix(covariates),response, alpha=1)

#par(mfrow=c(1,1))

plot(lasso.model)