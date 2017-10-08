library(e1071)
library(caret)
library(xlsx)
library(ggplot2)

dataset <- read.xlsx("Dataset.xlsx",sheetIndex=1)
df <- dataset[,c(3:6)]
inputset <- data.frame(df)

set.seed(1234)
trainIndex1 <- sample(2,nrow(inputset),replace = TRUE,prob=c(0.8,0.2))
trainInput1 <- inputset[trainIndex1==1,1:4]
testInput1 <- inputset[trainIndex1==2,1:4]
inputlabels1 <- inputset[trainIndex1==1,4]

svm_model1 <- svm(trainInput1$Continent~., data=trainInput1 , kernel = "linear" , cost=1 , gamma=0.333333)
pred1 <- predict(svm_model1,testInput1)

set.seed(2909)
trainIndex2 <- sample(2,nrow(inputset),replace = TRUE,prob=c(0.8,0.2))
trainInput2 <- inputset[trainIndex2==1,1:4]
testInput2 <- inputset[trainIndex2==2,1:4]
inputlabels2 <- inputset[trainIndex2==1,4]

svm_model2 <- svm(trainInput2$Continent~., data=trainInput2 , kernel = "linear" , cost=1 , gamma=0.35)
pred2 <- predict(svm_model2,testInput2)

set.seed(386)
trainIndex3 <- sample(2,nrow(inputset),replace = TRUE,prob=c(0.8,0.2))
trainInput3 <- inputset[trainIndex3==1,1:4]
testInput3 <- inputset[trainIndex3==2,1:4]
inputlabels3 <- inputset[trainIndex3==1,4]

svm_model3 <- svm(trainInput3$Continent~., data=trainInput3 , kernel = "radial" , cost=1 , gamma=4)
pred3 <- predict(svm_model3,testInput3)

set.seed(7900)
trainIndex4 <- sample(2,nrow(inputset),replace = TRUE,prob=c(0.8,0.2))
trainInput4 <- inputset[trainIndex4==1,1:4]
testInput4 <- inputset[trainIndex4==2,1:4]
inputlabels4 <- inputset[trainIndex4==1,4]

svm_model4 <- svm(trainInput4$Continent~., data=trainInput4 , kernel = "linear" , cost=1 , gamma=0.35)
pred4 <- predict(svm_model4,testInput4)

set.seed(9310)
trainIndex5 <- sample(2,nrow(inputset),replace = TRUE,prob=c(0.8,0.2))
trainInput5 <- inputset[trainIndex5==1,1:4]
testInput5 <- inputset[trainIndex5==2,1:4]
inputlabels5 <- inputset[trainIndex5==1,4]

svm_model5 <- svm(trainInput5$Continent~., data=trainInput5 , kernel = "linear" , cost=1 , gamma=0.35)
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






