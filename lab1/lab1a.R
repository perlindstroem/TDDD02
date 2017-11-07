#imports the excel-file
library(readxl)
data <- read_excel("TDDE01/lab1/spambase.xlsx")
#View(spambase)

#divide data into training and test
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

train <- data.matrix(train)

rowSums(train)
