library(readxl)
data <- read_excel("TDDE01/lab1/spambase.xlsx")
#View(spambase)

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]