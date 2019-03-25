install.packages(c("AppliedPredictiveModeling","caret","ElemStatLearn","pgmm",
                   "rpart","EIAdata", "gdata", "ggmap", "ggplot2","e1071","randomForest"))
library(AppliedPredictiveModeling)
library(caret)
data(segmentationOriginal)
#Subset the data to a training set and testing set based on the Case variable in the data set.
set.seed(125)
data <- segmentationOriginal
inTrain <- data$Case == "Train"
trainData <- data[inTrain, ]
testData <- data[!inTrain, ]
modelFit <- train(Class ~ ., data=trainData, method="rpart")
modelFit$finalModel
plot(modelFit$finalModel, uniform=T)
text(modelFit$finalModel, cex=0.8)


library(pgmm)
library(randomForest)
data(olive)
olive = olive[,-1]
treeModel <- train(Area ~ ., data=olive, method="rpart2")
treeModel
newdata <- as.data.frame(t(colMeans(olive)))
predict(treeModel, newdata) 


library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
logitModel <- train(chd ~ age + alcohol + obesity + tobacco + 
                      typea + ldl, data=trainSA, method="glm", 
                    family="binomial")
logitModel
missClass <- function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

predictTrain <- predict(logitModel, trainSA)
predictTest <- predict(logitModel, testSA)
# Training Set Misclassification rate
missClass(trainSA$chd, predictTrain) # 0.2727273
# Test Set Misclassification rate
missClass(testSA$chd, predictTest) # 0.3116883


library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
head(vowel.train)
head(vowel.test)
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
modelRf <- randomForest(y ~ ., data = vowel.train, importance = FALSE)
order(varImp(modelRf), decreasing=T)
