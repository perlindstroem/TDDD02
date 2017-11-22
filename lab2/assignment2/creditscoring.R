library(readxl)
library(tree)
data = read_excel("TDDE01/lab2/assignment2/creditscoring.xls")
data$good_bad = as.factor(data$good_bad)
n = dim(data)[1]

training = data[1:floor((1/2)*n),]
validation = data[(floor((1/2)*n)+1):(floor((3/4)*n)),]
test = data[(floor((3/4)*n)+1):n,]

fit.gini = tree(good_bad ~ ., data = training, split = c("gini"))
fit.dev = tree(good_bad ~ ., data = training, split = c("deviance"))

#plot(fit.gini)
#text(fit.gini, pretty=0)
#fit.gini
#summary(fit.gini)

pred.gini = predict(fit.gini, newdata=test, type="class")
pred.dev = predict(fit.dev, newdata=test, type="class")

table(test$good_bad, pred.gini)
table(test$good_bad, pred.dev)