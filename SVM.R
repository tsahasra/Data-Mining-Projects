library(e1071)
library(caret)
library(xlsx)
library(ggplot2)
library(PRROC)
library(PerfMeas)

dataset <- read.xlsx("Dataset.xlsx",sheetIndex=1)
df <- dataset[,c(3:6)]
inputset <- data.frame(df)

set.seed(1150)
ds1 <- createDataPartition(inputset$Continent, p = .8,list = FALSE,times = 1)
trainInput1 <- inputset[ds1,]
testInput1 <- inputset[-ds1,]
inputlabels1 <- trainInput1[,4]

svm_model1 <- svm(trainInput1$Continent~., data=trainInput1 , kernel = "radial" , cost=10 , gamma=5)
pred1 <- predict(svm_model1,testInput1)

set.seed(3240)
ds2 <- createDataPartition(inputset$Continent, p = .8,list = FALSE,times = 1)
trainInput2 <- inputset[ds2,]
testInput2 <- inputset[-ds2,]
inputlabels2 <- trainInput2[,4]

svm_model2 <- svm(trainInput2$Continent~., data=trainInput2 , kernel = "radial" , cost=10 , gamma=8)
pred2 <- predict(svm_model2,testInput2)

set.seed(7200)
ds3 <- createDataPartition(inputset$Continent, p = .8,list = FALSE,times = 1)
trainInput3 <- inputset[ds3,]
testInput3 <- inputset[-ds3,]
inputlabels3 <- trainInput3[,4]

svm_model3 <- svm(trainInput3$Continent~., data=trainInput3 , kernel = "radial" , cost=10 , gamma=7)
pred3 <- predict(svm_model3,testInput3)

set.seed(7900)
ds4 <- createDataPartition(inputset$Continent, p = .8,list = FALSE,times = 1)
trainInput4 <- inputset[ds4,]
testInput4 <- inputset[-ds4,]
inputlabels4 <- trainInput4[,4]

svm_model4 <- svm(trainInput4$Continent~., data=trainInput4, kernel = "radial" , cost=10 , gamma=8)
pred4 <- predict(svm_model4,testInput4)

set.seed(9100)
ds5 <- createDataPartition(inputset$Continent, p = .8,list = FALSE,times = 1)
trainInput5 <- inputset[ds5,]
testInput5 <- inputset[-ds5,]
inputlabels5 <- trainInput5[,4]

svm_model5 <- svm(trainInput5$Continent~., data=trainInput5 , kernel = "radial" , cost=10 , gamma=1)
pred5 <- predict(svm_model5,testInput5)

c1 <- confusionMatrix(pred1,testInput1[,4])
c2 <- confusionMatrix(pred2,testInput2[,4])
c3 <- confusionMatrix(pred3,testInput3[,4])
c4 <- confusionMatrix(pred4,testInput4[,4])
c5 <- confusionMatrix(pred5,testInput5[,4])

c1$overall
c2$overall
c3$overall
c4$overall
c5$overall



# print(svmfit)
# svm_tune <- tune(svm, train.x=trainInput1, train.y=trainInput1$Continent, kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
# summary(tuned)






