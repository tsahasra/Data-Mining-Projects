library(caret)
library(RWeka)
library(class)
library(e1071)
library(party)
library(xlsx)
library(ggplot2)



######      
###### Read the dataset in xlsx format and return a data frame excluding columns rank and entity     
######

readFromXlsx <- function(){
  dataset <- read.xlsx("Dataset.xlsx",sheetIndex=1)
  df <- dataset[,c(3:6)]
  inputset <- data.frame(df)
}


######      
###### Get a list of 5 random samples of the input dataset split into 80:20 ratio     
######

SplitDatasetSample <- function(inputset,num){
  
  set.seed(num)
  trainIndex <- createDataPartition(inputset$Continent, p = .8,list = FALSE,times = 1)
  
  return(trainIndex)
}


######      
###### K Nearest Neighbour Classification
###### Uses knn function of the RWeka library to classify vectors in the test set of each sample using the k nearest neighbours in its corresponding train set 
###### Plots the confusion matrix for each sample and prints the accuracy of classification for each model
######

KNN <- function(inputset){
  
  ds1 <- SplitDatasetSample(inputset,1150)
  trainInput1 <- inputset[ds1,]
  testInput1 <- inputset[-ds1,]
  inputlabels1 <- trainInput1[,4]
  
  knn_p1 <- knn(train = trainInput1[,1:3] , test = testInput1[,1:3] ,cl = inputlabels1, k=7 , prob=TRUE)
  knn_c1 <- confusionMatrix(testInput1[,4],knn_p1)
  
  
  ds2 <- SplitDatasetSample(inputset,3240)
  trainInput2 <- inputset[ds2,]
  testInput2 <- inputset[-ds2,]
  inputlabels2 <- trainInput2[,4]
  
  knn_p2 <- knn(train = trainInput2[,1:3] , test = testInput2[,1:3] ,cl = inputlabels2, k=12 , prob=TRUE)
  knn_c2 <- confusionMatrix(testInput2[,4],knn_p2)
  
  
  ds3 <- SplitDatasetSample(inputset,7200)
  trainInput3 <- inputset[ds3,]
  testInput3 <- inputset[-ds3,]
  inputlabels3 <- trainInput3[,4]
  
  knn_p3 <- knn(train = trainInput3[,1:3] , test = testInput3[,1:3] ,cl = inputlabels3, k=9 , prob=TRUE)
  knn_c3 <- confusionMatrix(testInput3[,4],knn_p3)
  
  
  ds4 <- SplitDatasetSample(inputset,7900)
  trainInput4 <- inputset[ds4,]
  testInput4 <- inputset[-ds4,]
  inputlabels4 <- trainInput4[,4]
  
  knn_p4 <- knn(train = trainInput4[,1:3], test = testInput4[,1:3] ,cl = inputlabels4, k=15, prob=TRUE)
  knn_c4 <- confusionMatrix(testInput4[,4],knn_p4)
  
  
  ds5 <- SplitDatasetSample(inputset,9100)
  trainInput5 <- inputset[ds5,]
  testInput5 <- inputset[-ds5,]
  inputlabels5 <- trainInput5[,4]
  
  knn_p5 <- knn(train = trainInput5[,1:3], test = testInput5[,1:3],cl = inputlabels5, k=13 , prob=TRUE)
  knn_c5 <- confusionMatrix(testInput5[,4],knn_p5)
  
  
  cat("\n  Accuracy metrics for algorithm 'K-Nearest-Neighbour' \n")
  
  cat("\n Average accuracy : ")
  cat(mean(c(knn_c1$overall['Accuracy'], knn_c2$overall['Accuracy'] , knn_c3$overall['Accuracy'] , knn_c4$overall['Accuracy'] , knn_c5$overall['Accuracy']))*100 ,'%')
  
  cat("\n Standard deviation : ")
  cat(sd(c(knn_c1$overall['Accuracy'], knn_c2$overall['Accuracy'] , knn_c3$overall['Accuracy'] , knn_c4$overall['Accuracy'] , knn_c5$overall['Accuracy'])))
  cat("\n")
  
  cat("\n  Sample 1:\n")
  
  print(knn_c1$overall)
  
  print(knn_c1$table)
  

  cat("\n  Sample 2:\n")
  print(knn_c2$overall)
  
  print(knn_c2$table)
  
  cat("\n  Sample 3:\n")
  print(knn_c3$overall)
  
  print(knn_c3$table)
  
  cat("\n  Sample 4:\n")
  print(knn_c4$overall)
  
  print(knn_c4$table)
  
  cat("\n  Sample 5:\n")
  print(knn_c5$overall)
  
  print(knn_c5$table)
  cat("\n")
  
}


######      
###### C4.5 Decision Tree Classification
###### Creates a classification model using the J48 function of the RWeka library
###### Predicts the continent label in the test set using model and determines its accuracy
###### Plots the confusion matrix for each sample and prints the accuracy of classification for each model
######

C45 <- function(inputset){
  
  ds1 <- SplitDatasetSample(inputset,1150)
  trainInput1 <- inputset[ds1,]
  testInput1 <- inputset[-ds1,]
  inputlabels1 <- trainInput1[,4]
  
  C45_model1 <- J48(Continent~.,control = Weka_control(R = FALSE , C = 0.5 , M = 10),data = trainInput1)
  c45_p1 <- predict(C45_model1 , testInput1[,1:3])
  c45_c1 <- confusionMatrix(testInput1[,4],c45_p1)
  
  
  ds2 <- SplitDatasetSample(inputset,3240)
  trainInput2 <- inputset[ds2,]
  testInput2 <- inputset[-ds2,]
  inputlabels2 <- trainInput2[,4]
  
  C45_model2 <- J48(Continent~.,control = Weka_control(R = FALSE , C = 0.5 , M = 11), data = trainInput2)
  c45_p2 <- predict(C45_model2 , testInput2[,1:3])
  c45_c2 <- confusionMatrix(testInput2[,4],c45_p2)
  
  
  ds3 <- SplitDatasetSample(inputset,7200)
  trainInput3 <- inputset[ds3,]
  testInput3 <- inputset[-ds3,]
  inputlabels3 <- trainInput3[,4]
  
  C45_model3 <- J48(Continent~.,control = Weka_control(R = FALSE , C = 0.5 , M = 15), data = trainInput3)
  c45_p3 <- predict(C45_model3 , testInput3[,1:3])
  c45_c3 <- confusionMatrix(testInput3[,4],c45_p3)
  
  
  ds4 <- SplitDatasetSample(inputset,7900)
  trainInput4 <- inputset[ds4,]
  testInput4 <- inputset[-ds4,]
  inputlabels4 <- trainInput4[,4]
  
  C45_model4 <- J48(Continent~.,control = Weka_control(R = FALSE , C = 0.5, M = 12), data = trainInput4)
  c45_p4 <- predict(C45_model4 , testInput4[,1:3])
  c45_c4 <- confusionMatrix(testInput4[,4],c45_p4)
  
  
  ds5 <- SplitDatasetSample(inputset,9100)
  trainInput5 <- inputset[ds5,]
  testInput5 <- inputset[-ds5,]
  inputlabels5 <- trainInput5[,4]
  
  C45_model5 <- J48(Continent~.,control = Weka_control(R = FALSE , C = 0.5 , M = 12), data = trainInput5)
  c45_p5 <- predict(C45_model5 , testInput5[,1:3])
  c45_c5 <- confusionMatrix(testInput5[,4],c45_p5)
  
  
  cat("\n Accuracy metrics for algorithm 'C4.5' \n")
  
  cat("\n Average accuracy : ")
  cat(mean(c(c45_c1$overall['Accuracy'], c45_c2$overall['Accuracy'] , c45_c3$overall['Accuracy'] , c45_c4$overall['Accuracy'] , c45_c5$overall['Accuracy']))*100 ,'%')
  
  cat("\n Standard deviation : ")
  cat(sd(c(c45_c1$overall['Accuracy'], c45_c2$overall['Accuracy'] , c45_c3$overall['Accuracy'] , c45_c4$overall['Accuracy'] , c45_c5$overall['Accuracy'])))
  cat("\n")
  
  cat("\n  Sample 1:\n")
  print(c45_c1$overall)
  
  print(c45_c1$table)
  
  cat("\n  Sample 2:\n")
  print(c45_c2$overall)
  
  print(c45_c2$table)
  
  cat("\n  Sample 3:\n")
  print(c45_c3$overall)
  
  print(c45_c3$table)
  
  cat("\n  Sample 4:\n")
  print(c45_c4$overall)
  
  print(c45_c4$table)
  
  cat("\n  Sample 5:\n")
  print(c45_c5$overall)
  
  print(c45_c5$table)
  cat("\n")
}


######      
###### ' Ripper ' Rule based Classification
###### Creates a classification model using the JRip function of the RWeka library
###### Predicts the continent label in the test set using model and determines its accuracy
###### Plots the confusion matrix for each sample and prints the accuracy of classification for each model
######

Ripper <- function(inputset){
  
  ds1 <- SplitDatasetSample(inputset,1150)
  trainInput1 <- inputset[ds1,]
  testInput1 <- inputset[-ds1,]
  inputlabels1 <- trainInput1[,4]
  
  rip_m1 <- JRip(Continent~.,control = Weka_control(O = 0 , F =  5 , N = 2),data = trainInput1)
  rip_p1 <- predict(rip_m1, newdata = testInput1,type = c("class", "probability"))
  rip_c1 <- confusionMatrix(testInput1[,4],rip_p1)
  
  
  ds2 <- SplitDatasetSample(inputset,3240)
  trainInput2 <- inputset[ds2,]
  testInput2 <- inputset[-ds2,]
  inputlabels2 <- trainInput2[,4]
  
  rip_m2 <- JRip(Continent~.,control = Weka_control(O = 0 , F = 15 , N = 1), data = trainInput2)
  rip_p2 <- predict(rip_m2, newdata = testInput2,type = c("class", "probability"))
  rip_c2 <- confusionMatrix(testInput2[,4],rip_p2)
  
  
  ds3 <- SplitDatasetSample(inputset,7200)
  trainInput3 <- inputset[ds3,]
  testInput3 <- inputset[-ds3,]
  inputlabels3 <- trainInput3[,4]
  
  rip_m3 <- JRip(Continent~.,control = Weka_control(O = 0 , F = 8 , N = 1), data = trainInput3)
  rip_p3 <- predict(rip_m3, newdata = testInput3,type = c("class", "probability"))
  rip_c3 <- confusionMatrix(testInput3[,4],rip_p3)
  
  
  ds4 <- SplitDatasetSample(inputset,7900)
  trainInput4 <- inputset[ds4,]
  testInput4 <- inputset[-ds4,]
  inputlabels4 <- trainInput4[,4]
  
  rip_m4 <- JRip(Continent~.,control = Weka_control(O = 0 , F = 5 , N = 2), data = trainInput4)
  rip_p4 <- predict(rip_m4, newdata = testInput4,type = c("class", "probability"))
  rip_c4 <- confusionMatrix(testInput4[,4],rip_p4)
  
  
  ds5 <- SplitDatasetSample(inputset,9100)
  trainInput5 <- inputset[ds5,]
  testInput5 <- inputset[-ds5,]
  inputlabels5 <- trainInput5[,4]
  
  rip_m5 <- JRip(Continent~.,control = Weka_control(O = 0 , F = 5 , N = 2), data = trainInput5)
  rip_p5 <- predict(rip_m5, newdata = testInput5,type = c("class", "probability"))
  rip_c5 <- confusionMatrix(testInput5[,4],rip_p5)
  
  cat("\n Accuracy metrics for algorithm 'Ripper' \n")
  
  cat("\n Average accuracy : ")
  cat(mean(c(rip_c1$overall['Accuracy'], rip_c2$overall['Accuracy'] , rip_c3$overall['Accuracy'] , rip_c4$overall['Accuracy'] , rip_c5$overall['Accuracy']))*100 ,'%')
  
  cat("\n Standard deviation : ")
  cat(sd(c(rip_c1$overall['Accuracy'], rip_c2$overall['Accuracy'] , rip_c3$overall['Accuracy'] , rip_c4$overall['Accuracy'] , rip_c5$overall['Accuracy'])))
  cat("\n")
  
  cat("\n  Sample 1:\n")
  print(rip_c1$overall)
  
  print(rip_c1$table)
  
  cat("\n  Sample 2:\n")
  print(rip_c2$overall)
  
  print(rip_c2$table)
  
  cat("\n  Sample 3:\n")
  print(rip_c3$overall)
  
  print(rip_c3$table)
  
  cat("\n  Sample 4:\n")
  print(rip_c4$overall)
  
  print(rip_c4$table)
  
  cat("\n  Sample 5:\n")
  print(rip_c5$overall)
  
  print(rip_c5$table)
  cat("\n")
  
}


######      
###### SVM based Classification
###### uses the svm function in the e1071 library to train the support vector machines 
###### Predicts the continent label in the test set using model and determines its accuracy
###### Plots the confusion matrix for each sample and prints the accuracy of classification for each model
######

SVM <- function(inputset){
  
  ds1 <- SplitDatasetSample(inputset,1150)
  trainInput1 <- inputset[ds1,]
  testInput1 <- inputset[-ds1,]
  inputlabels1 <- trainInput1[,4]
  
  svm_model1 <- svm(trainInput1$Continent~., data=trainInput1 , kernel = "radial" , cost=10 , gamma=5)
  svm_pred1 <- predict(svm_model1,testInput1)
  svm_c1 <- confusionMatrix(testInput1[,4],svm_pred1)
  
  
  ds2 <- SplitDatasetSample(inputset,3240)
  trainInput2 <- inputset[ds2,]
  testInput2 <- inputset[-ds2,]
  inputlabels2 <- trainInput2[,4]
  
  svm_model2 <- svm(trainInput2$Continent~., data=trainInput2 , kernel = "radial" , cost=10 , gamma=8)
  svm_pred2 <- predict(svm_model2,testInput2)
  svm_c2 <- confusionMatrix(testInput2[,4],svm_pred2)
  
  
  ds3 <- SplitDatasetSample(inputset,7200)
  trainInput3 <- inputset[ds3,]
  testInput3 <- inputset[-ds3,]
  inputlabels3 <- trainInput3[,4]
  
  svm_model3 <- svm(trainInput3$Continent~., data=trainInput3 , kernel = "radial" , cost=10 , gamma=7)
  svm_pred3 <- predict(svm_model3,testInput3)
  svm_c3 <- confusionMatrix(testInput3[,4],svm_pred3)
  
  
  ds4 <- SplitDatasetSample(inputset,7900)
  trainInput4 <- inputset[ds4,]
  testInput4 <- inputset[-ds4,]
  inputlabels4 <- trainInput4[,4]
  
  svm_model4 <- svm(trainInput4$Continent~., data=trainInput4, kernel = "radial" , cost=10 , gamma=8)
  svm_pred4 <- predict(svm_model4,testInput4)
  svm_c4 <- confusionMatrix(testInput4[,4],svm_pred4)
  
  
  ds5 <- SplitDatasetSample(inputset,9100)
  trainInput5 <- inputset[ds5,]
  testInput5 <- inputset[-ds5,]
  inputlabels5 <- trainInput5[,4]
  
  svm_model5 <- svm(trainInput5$Continent~., data=trainInput5 , kernel = "radial" , cost=10 , gamma=1)
  svm_pred5 <- predict(svm_model5,testInput5)
  svm_c5 <- confusionMatrix(testInput5[,4],svm_pred5)
  
  
  cat("\n Accuracy metrics for algorithm 'SVM' \n")
  
  cat("\n Average accuracy : ")
  cat(mean(c(svm_c1$overall['Accuracy'], svm_c2$overall['Accuracy'] , svm_c3$overall['Accuracy'] , svm_c4$overall['Accuracy'] , svm_c5$overall['Accuracy']))*100 ,'%')
  
  cat("\n Standard deviation : ")
  cat(sd(c(svm_c1$overall['Accuracy'], svm_c2$overall['Accuracy'] , svm_c3$overall['Accuracy'] , svm_c4$overall['Accuracy'] , svm_c5$overall['Accuracy'])))
  cat("\n")
  
  cat("\n  Sample 1:\n")
  print(svm_c1$overall)
  
  print(svm_c1$table)
  
  cat("\n  Sample 2:\n")
  print(svm_c2$overall)
  
  print(svm_c2$table)
  
  cat("\n  Sample 3:\n")
  print(svm_c3$overall)
  
  print(svm_c3$table)
  
  cat("\n  Sample 4:\n")
  print(svm_c4$overall)
  
  print(svm_c4$table)
  
  cat("\n  Sample 5:\n")
  print(svm_c5$overall)
  
  print(svm_c5$table)
  cat("\n")
  
}


inputset <- readFromXlsx()
KNN(inputset)
C45(inputset)
Ripper(inputset)
SVM(inputset)






