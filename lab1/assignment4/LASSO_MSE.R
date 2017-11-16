library(readxl)
library(glmnet)
data <- read_excel("TDDE01/lab1/tecator.xlsx", header=F)

covariates=scale(data[,2:101])
response=scale(data[,102])

lasso.model=cv.glmnet(as.matrix(covariates),response, alpha=1)
print(lasso.model$lambda.min)
coef(lasso.model, s="lambda.min")

plot(lasso.model)