library(caret)
library(RWeka)
library(class)
library(e1071)
library(party)
library(xlsx)

dataset <- read.xlsx("Dataset.xlsx",sheetIndex=1)
df <- dataset[,c(3:6)]
inputset <- data.frame(df)

set.seed(1234)
trainIndex1 <- sample(2,nrow(inputset),replace = TRUE,prob=c(0.8,0.2))
trainInput1 <- inputset[trainIndex1==1,]
testInput1 <- inputset[trainIndex1==2,]
inputlabels1 <- inputset[trainIndex1==1,4]

set.seed(2909)
trainIndex2 <- sample(2,nrow(inputset),replace = TRUE,prob=c(0.8,0.2))
trainInput2 <- inputset[trainIndex2==1,]
testInput2 <- inputset[trainIndex2==2,]
inputlabels2 <- inputset[trainIndex2==1,4]

set.seed(386)
trainIndex3 <- sample(2,nrow(inputset),replace = TRUE,prob=c(0.8,0.2))
trainInput3 <- inputset[trainIndex3==1,]
testInput3 <- inputset[trainIndex3==2,]
inputlabels3 <- inputset[trainIndex3==1,4]

set.seed(7900)
trainIndex4 <- sample(2,nrow(inputset),replace = TRUE,prob=c(0.8,0.2))
trainInput4 <- inputset[trainIndex4==1,]
testInput4 <- inputset[trainIndex4==2,]
inputlabels4 <- inputset[trainIndex4==1,4]

set.seed(9642)
trainIndex5 <- sample(2,nrow(inputset),replace = TRUE,prob=c(0.8,0.2))
trainInput5 <- inputset[trainIndex5==1,]
testInput5 <- inputset[trainIndex5==2,]
inputlabels5 <- inputset[trainIndex5==1,4]

C45_model1 <- J48(Continent~.,control = Weka_control(R = FALSE , C = 0.5 , M = 18), data = trainInput1)
C45_model2 <- J48(Continent~.,control = Weka_control(R = FALSE , C = 0.5 , M = 9), data = trainInput2)
C45_model3 <- J48(Continent~.,control = Weka_control(R = FALSE , C = 0.5 , M = 10), data = trainInput3)
C45_model4 <- J48(Continent~.,control = Weka_control(R = FALSE , C = 0.5 , M = 9), data = trainInput4)
C45_model5 <- J48(Continent~.,control = Weka_control(R = FALSE , C = 0.5 , M = 3), data = trainInput5)

c45_p1 <- predict(C45_model1 , testInput1[,1:3])
c45_p2 <- predict(C45_model2 , testInput2[,1:3])
c45_p3 <- predict(C45_model3 , testInput3[,1:3])
c45_p4 <- predict(C45_model4 , testInput4[,1:3])
c45_p5 <- predict(C45_model5 , testInput5[,1:3])

knn_p1 <- knn(train = trainInput1[,1:3], test = testInput1[,1:3],cl = inputlabels1, k=26 ,prob=TRUE)
knn_p2 <- knn(train = trainInput2[,1:3], test = testInput2[,1:3],cl = inputlabels2, k=10 ,prob=TRUE)
knn_p3 <- knn(train = trainInput3[,1:3], test = testInput3[,1:3],cl = inputlabels3, k=6  ,prob=TRUE)
knn_p4 <- knn(train = trainInput4[,1:3], test = testInput4[,1:3],cl = inputlabels4, k=17 ,prob=TRUE)
knn_p5 <- knn(train = trainInput5[,1:3], test = testInput5[,1:3],cl = inputlabels5, k=32 ,prob=TRUE)

rip_m1 <- JRip(Continent~.,control = Weka_control(O = 0 , F = 7 , N = 2), data = trainInput1)
rip_m2 <- JRip(Continent~.,control = Weka_control(O = 0 , F = 7 , N = 2), data = trainInput2)
rip_m3 <- JRip(Continent~.,control = Weka_control(O = 0 , F = 6 , N = 2), data = trainInput3)
rip_m4 <- JRip(Continent~.,control = Weka_control(O = 0 , F = 5 , N = 1), data = trainInput4)
rip_m5 <- JRip(Continent~.,control = Weka_control(O = 0 , F = 6 , N = 2), data = trainInput5)

rip_p1 <- predict(rip_m1, newdata = testInput1,type = c("class", "probability"))
rip_p2 <- predict(rip_m2, newdata = testInput2,type = c("class", "probability"))
rip_p3 <- predict(rip_m3, newdata = testInput3,type = c("class", "probability"))
rip_p4 <- predict(rip_m4, newdata = testInput4,type = c("class", "probability"))
rip_p5 <- predict(rip_m5, newdata = testInput5,type = c("class", "probability"))

svm_model1 <- svm(trainInput1$Continent~., data=trainInput1 , kernel = "linear" , cost=1 , gamma=0.333333)
svm_model2 <- svm(trainInput2$Continent~., data=trainInput2 , kernel = "linear" , cost=1 , gamma=0.35)
svm_model3 <- svm(trainInput3$Continent~., data=trainInput3 , kernel = "radial" , cost=1 , gamma=4)
svm_model4 <- svm(trainInput4$Continent~., data=trainInput4 , kernel = "linear" , cost=1 , gamma=0.35)
svm_model5 <- svm(trainInput5$Continent~., data=trainInput5 , kernel = "linear" , cost=1 , gamma=0.35)

svm_pred1 <- predict(svm_model1,testInput1)
svm_pred2 <- predict(svm_model2,testInput2)
svm_pred3 <- predict(svm_model3,testInput3)
svm_pred4 <- predict(svm_model4,testInput4)
svm_pred5 <- predict(svm_model5,testInput5)

c45_c1 <- confusionMatrix(c45_p1, testInput1[,4])
c45_c2 <- confusionMatrix(c45_p2, testInput2[,4])
c45_c3 <- confusionMatrix(c45_p3, testInput3[,4])
c45_c4 <- confusionMatrix(c45_p4, testInput4[,4])
c45_c5 <- confusionMatrix(c45_p5, testInput5[,4])

knn_c1 <- confusionMatrix(knn_p1, testInput1[,4])
knn_c2 <- confusionMatrix(knn_p2, testInput2[,4])
knn_c3 <- confusionMatrix(knn_p3, testInput3[,4])
knn_c4 <- confusionMatrix(knn_p4, testInput4[,4])
knn_c5 <- confusionMatrix(knn_p5, testInput5[,4])

rip_c1 <- confusionMatrix(rip_p1, testInput1[,4])
rip_c2 <- confusionMatrix(rip_p2, testInput2[,4])
rip_c3 <- confusionMatrix(rip_p3, testInput3[,4])
rip_c4 <- confusionMatrix(rip_p4, testInput4[,4])
rip_c5 <- confusionMatrix(rip_p5, testInput5[,4])

svm_c1 <- confusionMatrix(svm_pred1,testInput1[,4])
svm_c2 <- confusionMatrix(svm_pred2,testInput2[,4])
svm_c3 <- confusionMatrix(svm_pred3,testInput3[,4])
svm_c4 <- confusionMatrix(svm_pred4,testInput4[,4])
svm_c5 <- confusionMatrix(svm_pred5,testInput5[,4])

######  c45 - confusion Matrix  ######
c45_c1$overall
c45_c2$overall
c45_c3$overall
c45_c4$overall
c45_c5$overall

######  knn - confusion Matrix  ######
knn_c1$overall
knn_c2$overall
knn_c3$overall
knn_c4$overall
knn_c5$overall

######  ripper - confusion Matrix  ######
rip_c1$overall
rip_c2$overall
rip_c3$overall
rip_c4$overall
rip_c5$overall

######  svm - confusion Matrix  ######
svm_c1$overall
svm_c2$overall
svm_c3$overall
svm_c4$overall
svm_c5$overall
