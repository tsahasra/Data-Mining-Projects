library(caret)
library(RWeka)
library(party)
library(xlsx)

dataset <- read.xlsx("Dataset.xlsx",sheetIndex=1)
df <- dataset[,c(3:6)]
inputset <- data.frame(df)

set.seed(1)
trainIndex <- sample(2,nrow(inputset),replace = TRUE,prob=c(0.8,0.2))
trainInput1 <- inputset[trainIndex==1,]
testInput1 <- inputset[trainIndex==2,]
m1 <- JRip(Continent~.,control = Weka_control(O = 0 , F = 5 , N = 1),data = trainInput1)

set.seed(234)
trainIndex <- sample(2,nrow(inputset),replace = TRUE,prob=c(0.8,0.2))
trainInput2 <- inputset[trainIndex==1,]
testInput2 <- inputset[trainIndex==2,]
m2 <- JRip(Continent~.,control = Weka_control(O = 0 , F = 7 , N = 2), data = trainInput2)

set.seed(321)
trainIndex <- sample(2,nrow(inputset),replace = TRUE,prob=c(0.8,0.2))
trainInput3 <- inputset[trainIndex==1,]
testInput3 <- inputset[trainIndex==2,]
m3 <- JRip(Continent~.,control = Weka_control(O = 0 , F = 5 , N = 2), data = trainInput3)

set.seed(432)
trainIndex <- sample(2,nrow(inputset),replace = TRUE,prob=c(0.8,0.2))
trainInput4 <- inputset[trainIndex==1,]
testInput4 <- inputset[trainIndex==2,]
m4 <- JRip(Continent~.,control = Weka_control(O = 0 , F = 8 , N = 2), data = trainInput4)

set.seed(54)
trainIndex <- sample(2,nrow(inputset),replace = TRUE,prob=c(0.8,0.2))
trainInput5 <- inputset[trainIndex==1,]
testInput5 <- inputset[trainIndex==2,]
m5 <- JRip(Continent~.,control = Weka_control(O = 0 , F = 5 , N = 1), data = trainInput5)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)

p1 <- predict(m1, newdata = testInput1,type = c("class", "probability"))
p2 <- predict(m2, newdata = testInput2,type = c("class", "probability"))
p3 <- predict(m3, newdata = testInput3,type = c("class", "probability"))
p4 <- predict(m4, newdata = testInput4,type = c("class", "probability"))
p5 <- predict(m5, newdata = testInput5,type = c("class", "probability"))

summary(p1)
summary(p2)
summary(p3)
summary(p4)
summary(p5)