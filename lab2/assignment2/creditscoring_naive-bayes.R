library(readxl)
library(e1071)

data = read_excel("TDDE01/lab2/assignment2/creditscoring.xls")
data$good_bad = as.factor(data$good_bad)
n = dim(data)[1]

# splitting
training = data[1:floor((1/2)*n),]
validation = data[(floor((1/2)*n)+1):(floor((3/4)*n)),]
test = data[(floor((3/4)*n)+1):n,]

# fitting
fit = naiveBayes(good_bad ~ ., data = training)

# predictions
pred.train = predict(fit, newdata = training)
pred.test = predict(fit, newdata = test)

# confusion matrices
table(training$good_bad, pred.train)
table(test$good_bad, pred.test)

# continue with loss matrix