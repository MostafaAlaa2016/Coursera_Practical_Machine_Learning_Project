library(elasticnet)
library(forecast)
library(e1071)
library(ElemStatLearn)
install.packages(c("AppliedPredictiveModeling","caret","ElemStatLearn","pgmm",
                   "rpart","EIAdata", "gdata", "ggmap", "ggplot2","e1071","randomForest","forecast"))
library(AppliedPredictiveModeling)
library(caret)

data(vowel.train)
data(vowel.test)

#Set the variable y to be a factor variable in both the training and test set. 
#Then set the seed to 33833. Fit (1) a random forest predictor relating the 
#factor variable y to the remaining variables and (2) a boosted predictor using 
#the "gbm" method. Fit these both with the train() command in the caret package.

#What are the accuracies for the two approaches on the test data set? 
#What is the accuracy among the test set samples where the two methods agree?
  
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
modelFitRf <- train(y ~ ., data=vowel.train, method="rf")
predictTestRf <- predict(modelFitRf, vowel.test)
cmRf <- confusionMatrix(predictTest, vowel.test$y)$overall["Accuracy"]

modelFitGbm <- train(y ~ ., data=vowel.train, method="gbm")
predictTestGbm <- predict(modelFitGbm, vowel.test)
cmGbm <- confusionMatrix(predictTestGbm, vowel.test$y)$overall["Accuracy"]

confusionMatrix(predictTestGbm,predictTestRf)$overall['Accuracy']

#predDf <- data.frame(predictTestRf,predictTestGbm,y=vowel.test$y)
#modelFitCombined <- train(y ~ ., data=predDf, method="gam")
#predictTestComb <- predict(modelFitCombined, vowel.test)
#cmComb <- confusionMatrix(predictTestComb, vowel.test$y)$overall["Accuracy"]

########################

library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]


#Set the seed to 62433 and predict diagnosis with all the other variables 
#using a random forest ("rf"), boosted trees ("gbm") and linear discriminant analysis ("lda") model. 
#Stack the predictions together using random forests ("rf"). 
#What is the resulting accuracy on the test set? 
#Is it better or worse than each of the individual predictions?

set.seed(62433)
ModelRf <- train(diagnosis ~ ., data=training, method="rf")
ModelGbm <- train(diagnosis ~ ., data=training, method="gbm", verbose=FALSE)
ModelLda <- train(diagnosis ~ ., data=training, method="lda")

PredictRf <- predict(ModelRf, testing)
PredictGbm <- predict(ModelGbm, testing)
PredictLda <- predict(ModelLda, testing)

predDfCombined <- data.frame(PredictRf, PredictGbm, PredictLda, diagnosis = testing$diagnosis)
ModelCombinedRf <- train(diagnosis ~ ., data=predDfCombined, method="rf")
predictModelCombinedRf <- predict(ModelCombinedRf, testing)

print(paste0("RF accuracy = ", confusionMatrix(PredictRf, testing$diagnosis)$overall['Accuracy']))
print(paste0("GBM accuracy = ", confusionMatrix(PredictGbm, testing$diagnosis)$overall['Accuracy']))
print(paste0("LDA accuracy = ", confusionMatrix(PredictLda, testing$diagnosis)$overall['Accuracy']))
print(paste0("Combined accuracy = ", confusionMatrix(predictModelCombinedRf, testing$diagnosis)$overall['Accuracy']))

  
#################

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
#Set the seed to 233 and fit a lasso model to predict Compressive Strength. 
#Which variable is the last coefficient to be set to zero as the penalty increases? 
#(Hint: it may be useful to look up ?plot.enet).

set.seed(3523)
ModelLasso <- train(CompressiveStrength~., data=training, method = "lasso")
plot.enet(ModelLasso$finalModel, xvar = "penalty", use.color = TRUE)


############

library(lubridate) # For year() function below
dat = read.csv("./Master/CombinedWorkspace/R/7_Practical_Machine_Learning/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

ModelBats <- bats(tstrain)
forecastBats <- forecast(ModelBats, nrow(testing), level = c(95))
plot(forecastBats)
points(dat$visitsTumblr)
print(sum(forecastBats$lower <= testing$visitsTumblr & testing$visitsTumblr <= forecastBats$upper) / dim(testing)[1])



##############

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testin = concrete[-inTrain,]

#Set the seed to 325 and fit a support vector machine using the 
#e1071 package to predict Compressive Strength using the default settings. 
#Predict on the testing set. What is the RMSE?

set.seed(325)
ModelSvm <- svm(CompressiveStrength~., data=training)
predictSvm <- predict(ModelSvm, testing)
accuracy(predictSvm, testing$CompressiveStrength)




