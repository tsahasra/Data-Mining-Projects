library(class)
library(xlsx)

dataset <- read.xlsx("Dataset.xlsx",sheetIndex=1)
df <- dataset[,c(3:6)]
inputset <- data.frame(df)

# set.seed(7900)
set.seed(1150)
ds1 <- createDataPartition(inputset$Continent, p = .8,list = FALSE,times = 1)
trainInput1 <- inputset[ds1,]
testInput1 <- inputset[-ds1,]
inputlabels1 <- trainInput1[,4]

p1 <- knn(train = trainInput1[,1:3] , test = testInput1[,1:3] ,cl = inputlabels1, k=7 , prob=TRUE)


set.seed(3240)
ds2 <- createDataPartition(inputset$Continent, p = .8,list = FALSE,times = 1)
trainInput2 <- inputset[ds2,]
testInput2 <- inputset[-ds2,]
inputlabels2 <- trainInput2[,4]

p2 <- knn(train = trainInput2[,1:3] , test = testInput2[,1:3] ,cl = inputlabels2, k=12 , prob=TRUE)


set.seed(7200)
ds3 <- createDataPartition(inputset$Continent, p = .8,list = FALSE,times = 1)
trainInput3 <- inputset[ds3,]
testInput3 <- inputset[-ds3,]
inputlabels3 <- trainInput3[,4]

p3 <- knn(train = trainInput3[,1:3] , test = testInput3[,1:3] ,cl = inputlabels3, k=9 , prob=TRUE)


set.seed(7900)
ds4 <- createDataPartition(inputset$Continent, p = .8,list = FALSE,times = 1)
trainInput4 <- inputset[ds4,]
testInput4 <- inputset[-ds4,]
inputlabels4 <- trainInput4[,4]

p4 <- knn(train = trainInput4[,1:3], test = testInput4[,1:3] ,cl = inputlabels4, k=15, prob=TRUE)

set.seed(9100)
ds5 <- createDataPartition(inputset$Continent, p = .8,list = FALSE,times = 1)
trainInput5 <- inputset[ds5,]
testInput5 <- inputset[-ds5,]
inputlabels5 <- trainInput5[,4]

p5 <- knn(train = trainInput5[,1:3], test = testInput5[,1:3],cl = inputlabels5, k=13 , prob=TRUE)

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

