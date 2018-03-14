install.packages('dplyr') # Only have to run once.
install.packages("neuralnet")
install.packages("ggplot2")
require(neuralnet)
library(dplyr)
library(reshape)
library(MASS)
require(ggplot2)

?neuralnet

#set.seed(123)

d1 <- read.csv ("/Users/muhbandtekamshuru/Desktop/wireless/NN/Weather_2500_6x_1y_AppTemp_y.csv")

DataFrame = d1

# add this line to remove non-numeric columns.
DataFrame <- DataFrame[, sapply(DataFrame, is.numeric)]

## find min max and scale 
maxValue <- apply(DataFrame, 2, max)
minValue <- apply(DataFrame, 2, min)
DataFrame <- as.data.frame(scale(DataFrame, center = minValue, scale = maxValue-minValue))

?scale

## train, validation and test data
## train on 1500 rows
## validate on 500
## test on remaining 500 rows
ind <- sample(1:nrow(DataFrame), 1500)
trainDF <- DataFrame[ind,]
remainingInd <- c(1:2499)[-ind]
validInd <- sample(remainingInd, 500)
validDF <- DataFrame[validInd,]
#testInd <- remainingInd[-validInd]
testInd <- setdiff(remainingInd, validInd)
testDF <- DataFrame[testInd,]

allVars <- colnames(DataFrame)
predictorVars <- allVars[!allVars%in%"Apparent.Temperature..C."]
predictorVars <- paste(predictorVars, collapse = "+")
form=as.formula(paste("Apparent.Temperature..C.~",predictorVars, collapse = "+"))

neuralModel <- neuralnet(formula = form, hidden = c(4,2), linear.output = T, data = trainDF, stepmax=1e6)

neuralModel

##neuralModel$net.result[[1]]


plot(neuralModel)

## predict for test data
predictions <- neuralnet::compute(neuralModel, validDF[, 0:6])
str(predictions)

predictions <- predictions$net.result*(max(validDF$Apparent.Temperature..C.)-min(validDF$Apparent.Temperature..C))+min(validDF$Apparent.Temperature..C)
actualValues <- (validDF$Apparent.Temperature..C)*(max(validDF$Apparent.Temperature..C) - min(validDF$Apparent.Temperature..C)) + min(validDF$Apparent.Temperature..C)
predictions

#predictions <- compute(neuralModel, testDF[, 0:6])
#str(predictions)

#predictions <- predictions$net.result*(max(testDF$Temperature..C.)-min(testDF$Temperature..C.))+min(testDF$Temperature..C.)
#actualValues <- (testDF$Temperature..C.)*(max(testDF$Temperature..C.) - min(testDF$Temperature..C.)) + min(testDF$Temperature..C.)

MSE <- sum((predictions - actualValues)^2)/nrow(validDF)
#MSE <-sum((predictions - actualValues)^2)
MSE

plot(testDF$Temperature..C., predictions, col = 'blue')
plot(testDF$Temperature..C., actualValues, col = 'red')


ggplot() +
  geom_line(data = validDF, aes(x = validInd, y = Apparent.Temperature..C., color = "red")) +
  geom_line(data = validDF, aes(x = validInd, y = predictions, color = "blue"))

#ggplot() +
 # geom_line(data = trainDF, aes(x = ind, y = Apparent.Temperature..C.)) +
 # geom_point(data = validDF, aes(x = validInd, y = Apparent.Temperature..C., color = "red")) +
 # geom_point(data = validDF, aes(x = validInd, y = predictions, color = "blue"))
