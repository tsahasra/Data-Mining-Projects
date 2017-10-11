library(caret)
library(RWeka)
library(party)
library(xlsx)

dataset <- read.xlsx("Dataset.xlsx",sheetIndex=1)
df <- dataset[,c(3:6)]
inputset <- data.frame(df)

set.seed(1150)
ds1 <- createDataPartition(inputset$Continent, p = .8,list = FALSE,times = 1)
trainInput1 <- inputset[ds1,]
testInput1 <- inputset[-ds1,]
inputlabels1 <- trainInput1[,4]
m1 <- J48(Continent~.,control = Weka_control(R = FALSE , C = 0.5 , M = 10),data = trainInput1)

set.seed(3240)
ds2 <- createDataPartition(inputset$Continent, p = .8,list = FALSE,times = 1)
trainInput2 <- inputset[ds2,]
testInput2 <- inputset[-ds2,]
inputlabels2 <- trainInput2[,4]
m2 <- J48(Continent~.,control = Weka_control(R = FALSE , C = 0.5 , M = 11), data = trainInput2)

set.seed(7200)
ds3 <- createDataPartition(inputset$Continent, p = .8,list = FALSE,times = 1)
trainInput3 <- inputset[ds3,]
testInput3 <- inputset[-ds3,]
inputlabels3 <- trainInput3[,4]
m3 <- J48(Continent~.,control = Weka_control(R = FALSE , C = 0.5 , M = 15), data = trainInput3)

set.seed(7900)
ds4 <- createDataPartition(inputset$Continent, p = .8,list = FALSE,times = 1)
trainInput4 <- inputset[ds4,]
testInput4 <- inputset[-ds4,]
inputlabels4 <- trainInput4[,4]
m4 <- J48(Continent~.,control = Weka_control(R = FALSE , C = 0.5, M = 12), data = trainInput4)

set.seed(9100)
ds5 <- createDataPartition(inputset$Continent, p = .8,list = FALSE,times = 1)
trainInput5 <- inputset[ds5,]
testInput5 <- inputset[-ds5,]
inputlabels5 <- trainInput5[,4]
m5 <- J48(Continent~.,control = Weka_control(R = FALSE , C = 0.5 , M = 12), data = trainInput5)

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
c2$overall
c3$overall
c4$overall
c5$overall