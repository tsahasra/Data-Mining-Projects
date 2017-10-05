library(RWeka)
library(party)

dataset <- read.xlsx("Dataset.xlsx",sheetIndex=1)
df <- dataset[,c(3:6)]
inputset <- data.frame(df)

set.seed(1234)
trainIndex <- sample(2,nrow(inputset),replace = TRUE,prob=c(0.8,0.2))
trainInput <- inputset[trainIndex==1,]
testInput <- inputset[trainIndex==2,]

set.seed(2345)
trd <- sample(5,nrow(trainInput),replace = TRUE,prob=c(0.2,0.2,0.2,0.2,0.2))
train1 <- trainInput[trd==1,]
train2 <- trainInput[trd==2,]
train3 <- trainInput[trd==3,]
train4 <- trainInput[trd==4,]
train5 <- trainInput[trd==5,]

set.seed(1)
vd <- sample(5,nrow(testInput),replace = TRUE,prob=c(0.2,0.2,0.2,0.2,0.2))
test1 <- testInput[vd==1,]
test2 <- testInput[vd==2,]
test3 <- testInput[vd==3,]
test4 <- testInput[vd==4,]
test5 <- testInput[vd==5,]

m1 <- JRip(Continent~.,control = Weka_control(O = 0 , F = 5 , N = 1), data = train1)
m2 <- JRip(Continent~.,control = Weka_control(O = 0 , F = 5 , N = 1), data = train2)
m3 <- JRip(Continent~.,control = Weka_control(O = 0 , F = 5 , N = 1), data = train3)
m4 <- JRip(Continent~.,control = Weka_control(O = 0 , F = 5 , N = 1), data = train4)
m5 <- JRip(Continent~.,control = Weka_control(O = 0 , F = 5 , N = 1), data = train5)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)

p1 <- predict(m1, newdata = test1,type = c("class", "probability"))
p2 <- predict(m2, newdata = test2,type = c("class", "probability"))
p3 <- predict(m3, newdata = test3,type = c("class", "probability"))
p4 <- predict(m4, newdata = test4,type = c("class", "probability"))
p5 <- predict(m5, newdata = test5,type = c("class", "probability"))

summary(p1)
summary(p2)
summary(p3)
summary(p4)
summary(p5)




