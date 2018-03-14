#set.seed(123)

d1 <- read.csv ("/Users/muhbandtekamshuru/Desktop/wireless/NN/Weather_2500_6x_1y_AppTemp_y.csv")

DataFrame = d1

# add this line to remove non-numeric columns.
DataFrame <- DataFrame[, sapply(DataFrame, is.numeric)]

#not normalized 
notNormalizedDF = DataFrame

ind <- sample(1:nrow(notNormalizedDF), 1500)
trainDF <- notNormalizedDF[ind,]
remainingInd <- c(1:2499)[-ind]
validInd <- sample(remainingInd, 500)
validDF <- notNormalizedDF[validInd,]
#testInd <- remainingInd[-validInd]
testInd <- setdiff(remainingInd, validInd)
testDF <- notNormalizedDF[testInd,]

allVars <- colnames(notNormalizedDF)
predictorVars <- allVars[!allVars%in%"Apparent.Temperature..C."]
predictorVars <- paste(predictorVars, collapse = "+")
form=as.formula(paste("Apparent.Temperature..C.~",predictorVars, collapse = "+"))

neuralModel <- neuralnet(formula = form, hidden = c(4,2), linear.output = T, data = trainDF, stepmax=1e6)

neuralModel

##neuralModel$net.result[[1]]


plot(neuralModel)

## predict for test data
predictions <- compute(neuralModel, validDF[, 0:6])
str(predictions)

#predictions <- predictions$net.result*(max(validDF$Temperature..C.)-min(validDF$Temperature..C.))+min(validDF$Temperature..C.)
#actualValues <- (validDF$Temperature..C.)*(max(validDF$Temperature..C.) - min(validDF$Temperature..C.)) + min(validDF$Temperature..C.)
predictions <- predictions$net.result
actualValues <- validDF$Temperature..C.

MSE <-sum((predictions - actualValues)^2)/nrow(validDF)
MSE

plot(testDF$Temperature..C., predictions, col = 'blue')
plot(testDF$Temperature..C., actualValues, col = 'red')


ggplot() +
  geom_line(data = trainDF, aes(x = ind, y = Temperature..C.)) +
  geom_point(data = validDF, aes(x = validInd, y = Temperature..C., color = "red")) +
  geom_point(data = validDF, aes(x = validInd, y = predictions, color = "blue"))

