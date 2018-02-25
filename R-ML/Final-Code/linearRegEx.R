#http://r-statistics.co/Linear-Regression.html

# use cars data: 
#The data give the speed of cars and the distances taken to stop. Note that the data were recorded in the 1920s.
plot(x=cars$speed, y=cars$dist, main="Dist ~ Speed")  # scatterplot

# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))  # row indices for training data
trainingData <- cars[trainingRowIndex, ]  # model training data
testData  <- cars[-trainingRowIndex, ]   # test data

# Build the model on training data -
lmMod <- lm(dist ~ speed, data=trainingData)  # build the model
lmMod
distPred <- predict(lmMod, testData)  # predict distance
#summary (lmMod)  # model summary


#Calculate prediction accuracy and error rates
actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
