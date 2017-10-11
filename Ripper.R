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

m1 <- JRip(Continent~.,control = Weka_control(O = 0 , F =  5 , N = 2),data = trainInput1)

set.seed(3240)
ds2 <- createDataPartition(inputset$Continent, p = .8,list = FALSE,times = 1)
trainInput2 <- inputset[ds2,]
testInput2 <- inputset[-ds2,]
inputlabels2 <- trainInput2[,4]

m2 <- JRip(Continent~.,control = Weka_control(O = 0 , F = 15 , N = 1), data = trainInput2)

set.seed(7200)
ds3 <- createDataPartition(inputset$Continent, p = .8,list = FALSE,times = 1)
trainInput3 <- inputset[ds3,]
testInput3 <- inputset[-ds3,]
inputlabels3 <- trainInput3[,4]

m3 <- JRip(Continent~.,control = Weka_control(O = 0 , F = 8 , N = 1), data = trainInput3)

set.seed(7900)
ds4 <- createDataPartition(inputset$Continent, p = .8,list = FALSE,times = 1)
trainInput4 <- inputset[ds4,]
testInput4 <- inputset[-ds4,]
inputlabels4 <- trainInput4[,4]

m4 <- JRip(Continent~.,control = Weka_control(O = 0 , F = 5 , N = 2), data = trainInput4)

set.seed(9100)
ds5 <- createDataPartition(inputset$Continent, p = .8,list = FALSE,times = 1)
trainInput5 <- inputset[ds5,]
testInput5 <- inputset[-ds5,]
inputlabels5 <- trainInput5[,4]

m5 <- JRip(Continent~.,control = Weka_control(O = 0 , F = 5 , N = 2), data = trainInput5)










p1 <- predict(m1, newdata = testInput1,type = c("class", "probability"))
p2 <- predict(m2, newdata = testInput2,type = c("class", "probability"))
p3 <- predict(m3, newdata = testInput3,type = c("class", "probability"))
p4 <- predict(m4, newdata = testInput4,type = c("class", "probability"))
p5 <- predict(m5, newdata = testInput5,type = c("class", "probability"))

c1 <- confusionMatrix(p1,testInput1[,4])
c2 <- confusionMatrix(p2,testInput2[,4])
c3 <- confusionMatrix(p3,testInput3[,4])
c4 <- confusionMatrix(p4,testInput4[,4])
c5 <- confusionMatrix(p5,testInput5[,4])

c1$overall
c2$overall
c3$overall
c4$overall
c5$overall