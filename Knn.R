library(class)
library(xlsx)

dataset <- read.xlsx("Dataset.xlsx",sheetIndex=1)
df <- dataset[,c(3:6)]
inputset <- data.frame(df)

set.seed(1234)
trainIndex1 <- sample(2,nrow(inputset),replace = TRUE,prob=c(0.8,0.2))
trainInput1 <- inputset[trainIndex1==1,1:3]
testInput1 <- inputset[trainIndex1==2,1:3]
inputlabels1 <- inputset[trainIndex1==1,4]

p1 <- knn(train = trainInput1, test = testInput1,cl = inputlabels1, k=14.5,prob=TRUE)

set.seed(2909)
trainIndex2 <- sample(2,nrow(inputset),replace = TRUE,prob=c(0.8,0.2))
trainInput2 <- inputset[trainIndex2==1,1:3]
testInput2 <- inputset[trainIndex2==2,1:3]
inputlabels2 <- inputset[trainIndex2==1,4]

p2 <- knn(train = trainInput2, test = testInput2,cl = inputlabels2, k=14.5,prob=TRUE)

set.seed(386)
trainIndex3 <- sample(2,nrow(inputset),replace = TRUE,prob=c(0.8,0.2))
trainInput3 <- inputset[trainIndex3==1,1:3]
testInput3 <- inputset[trainIndex3==2,1:3]
inputlabels3 <- inputset[trainIndex3==1,4]

p3 <- knn(train = trainInput3, test = testInput3,cl = inputlabels3, k=14.5,prob=TRUE)

set.seed(7900)
trainIndex4 <- sample(2,nrow(inputset),replace = TRUE,prob=c(0.8,0.2))
trainInput4 <- inputset[trainIndex4==1,1:3]
testInput4 <- inputset[trainIndex4==2,1:3]
inputlabels4 <- inputset[trainIndex4==1,4]

p4 <- knn(train = trainInput4, test = testInput4,cl = inputlabels4, k=14.5,prob=TRUE)

set.seed(9310)
trainIndex5 <- sample(2,nrow(inputset),replace = TRUE,prob=c(0.8,0.2))
trainInput5 <- inputset[trainIndex5==1,1:3]
testInput5 <- inputset[trainIndex5==2,1:3]
inputlabels5 <- inputset[trainIndex5==1,4]

p5 <- knn(train = trainInput5, test = testInput5,cl = inputlabels5, k=14.5,prob=TRUE)

c1 <- confusionMatrix(p1,inputset[trainIndex1==2,4])
c2 <- confusionMatrix(p2,inputset[trainIndex2==2,4])
c3 <- confusionMatrix(p3,inputset[trainIndex3==2,4])
c4 <- confusionMatrix(p4,inputset[trainIndex4==2,4])
c5 <- confusionMatrix(p5,inputset[trainIndex5==2,4])

c1$overall
c2$overall
c3$overall
c4$overall
c5$overall

