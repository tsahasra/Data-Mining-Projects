library(caret)
library(RWeka)
library(party)
library(xlsx)

dataset <- read.xlsx("Dataset.xlsx",sheetIndex=1)
df <- dataset[,c(3:6)]
inputset <- data.frame(df)

set.seed(1234)
trainIndex <- sample(2,nrow(inputset),replace = TRUE,prob=c(0.8,0.2))
trainInput1 <- inputset[trainIndex==1,]
testInput1 <- inputset[trainIndex==2,]
m1 <- J48(Continent~.,control = Weka_control(R = FALSE , C = 0.5 , M = 18),data = trainInput1)

set.seed(2909)
trainIndex <- sample(2,nrow(inputset),replace = TRUE,prob=c(0.8,0.2))
trainInput2 <- inputset[trainIndex==1,]
testInput2 <- inputset[trainIndex==2,]
m2 <- J48(Continent~.,control = Weka_control(R = FALSE , C = 0.5 , M = 9), data = trainInput2)

set.seed(386)
trainIndex <- sample(2,nrow(inputset),replace = TRUE,prob=c(0.8,0.2))
trainInput3 <- inputset[trainIndex==1,]
testInput3 <- inputset[trainIndex==2,]
m3 <- J48(Continent~.,control = Weka_control(R = FALSE , C = 0.5 , M = 10), data = trainInput3)

set.seed(7900)
trainIndex <- sample(2,nrow(inputset),replace = TRUE,prob=c(0.8,0.2))
trainInput4 <- inputset[trainIndex==1,]
testInput4 <- inputset[trainIndex==2,]
m4 <- J48(Continent~.,control = Weka_control(R = FALSE , C = 0.5, M = 9), data = trainInput4)

set.seed(9642)
trainIndex <- sample(2,nrow(inputset),replace = TRUE,prob=c(0.8,0.2))
trainInput5 <- inputset[trainIndex==1,]
testInput5 <- inputset[trainIndex==2,]
m5 <- J48(Continent~.,control = Weka_control(R = FALSE , C = 0.5 , M = 3), data = trainInput5)

p1 <- predict(m1, testInput1[,1:3])
p2 <- predict(m2, testInput2[,1:3])
p3 <- predict(m3, testInput3[,1:3])
p4 <- predict(m4, testInput4[,1:3])
p5 <- predict(m5, testInput5[,1:3])

c1 <- confusionMatrix(p1, testInput1[,4])
c2 <- confusionMatrix(p2, testInput2[,4])
c3 <- confusionMatrix(p3, testInput3[,4])
c4 <- confusionMatrix(p4, testInput4[,4])
c5 <- confusionMatrix(p5, testInput5[,4])

c1$overall
# c2$overall
# c3$overall
# c4$overall
# c5$overall