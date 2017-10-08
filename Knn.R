library(class)
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

p1 <- knn(train = trainInput1[,1:3] , test = testInput1[,1:3] ,cl = inputlabels1, k=26 , prob=TRUE)
p2 <- knn(train = trainInput2[,1:3] , test = testInput2[,1:3] ,cl = inputlabels2, k=10 , prob=TRUE)
p3 <- knn(train = trainInput3[,1:3] , test = testInput3[,1:3] ,cl = inputlabels3, k=6 , prob=TRUE)
p4 <- knn(train = trainInput4[,1:3], test = testInput4[,1:3] ,cl = inputlabels4, k=17, prob=TRUE)
p5 <- knn(train = trainInput5[,1:3], test = testInput5[,1:3],cl = inputlabels5, k=32 , prob=TRUE)

c1 <- confusionMatrix(p1,testInput1[,4])
c2 <- confusionMatrix(p2,testInput2[,4])
c3 <- confusionMatrix(p3,testInput3[,4])
c4 <- confusionMatrix(p4,testInput4[,4])
c5 <- confusionMatrix(p5,testInput5[,4])

# c1$overall
# c2$overall
# c3$overall
# c4$overall
c5$overall

