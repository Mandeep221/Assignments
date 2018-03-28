install.packages('dplyr') # Only have to run once.
install.packages("neuralnet")
install.packages("ggplot2")
require(neuralnet)
library(dplyr)
library(reshape)
library(MASS)
require(ggplot2)

?neuralnet

set.seed(123)

d1 <- read.csv ("/Users/muhbandtekamshuru/Desktop/wireless/NN/Weather_2500_4x_1y.csv")

DataFrame = d1

# add this line to remove non-numeric columns.
DataFrame <- DataFrame[, sapply(DataFrame, is.numeric)]

d<- DataFrame[,c("Temperature..C.","Humidity","Wind.Speed..km.h.", "Pressure..millibars.", "Apparent.Temperature..C.")]
d_cor <- as.matrix(cor(d))

d_cor
## find min max and scale 
maxValue <- apply(DataFrame, 2, max)
minValue <- apply(DataFrame, 2, min)
DataFrame <- as.data.frame(scale(DataFrame, center = minValue, scale = maxValue-minValue))
DataFrame
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
testInd <- setdiff(remainingInd, validInd)
testDF <- DataFrame[testInd,]

allVars <- colnames(DataFrame)
predictorVars <- allVars[!allVars%in%"Apparent.Temperature..C."]
predictorVars <- paste(predictorVars, collapse = "+")
form=as.formula(paste("Apparent.Temperature..C.~",predictorVars, collapse = "+"))

set.seed(123)

sigmoid = function(x) {
  1 / (1 + exp(-x))
}

relu = function(x) {log(1+exp(x))}

#neuralModel <- neuralnet(formula = form, hidden = c(4,2), linear.output = T, data = trainDF, stepmax=1e7, algorithm = 'backprop', learningrate = 0.0001, act.fct = "tanh")
neuralModel <- neuralnet(formula = form, hidden = c(2,2), linear.output = T, data = trainDF, stepmax=1e7,algorithm = "rprop+", act.fct = "tanh")

neuralModel$result.matrix
neuralModel

##neuralModel$net.result[[1]]


plot(neuralModel)

## predict for test data
predictions <- neuralnet::compute(neuralModel, validDF[, 0:4])
str(predictions)

predictions <- predictions$net.result*(max(validDF$Apparent.Temperature..C.)-min(validDF$Apparent.Temperature..C))+min(validDF$Apparent.Temperature..C)
actualValues <- (validDF$Apparent.Temperature..C)*(max(validDF$Apparent.Temperature..C) - min(validDF$Apparent.Temperature..C)) + min(validDF$Apparent.Temperature..C)

predictions
actualValues
#predictions <- compute(neuralModel, testDF[, 0:6])
#str(predictions)

#predictions <- predictions$net.result*(max(testDF$Temperature..C.)-min(testDF$Temperature..C.))+min(testDF$Temperature..C.)
#actualValues <- (testDF$Temperature..C.)*(max(testDF$Temperature..C.) - min(testDF$Temperature..C.)) + min(testDF$Temperature..C.)

MSE <- sum((predictions - actualValues)^2)/nrow(validDF)
#MSE <-sum((predictions - actualValues)^2)
MSE

plot(testDF$Temperature..C., predictions, col = 'blue')
plot(testDF$Temperature..C., actualValues, col = 'red')

#ggplot() +
 # geom_line(data = validDF, aes(x = validInd, y = Apparent.Temperature..C., color = ticker )) +
 # geom_line(data = validDF, aes(x = validInd, y = predictions, color = ticker))

ggplot() +
  geom_line(data = validDF, aes(x = validInd, y = Apparent.Temperature..C., colour = "Actual Values")) +
  geom_line(data = validDF, aes(x = validInd, y = predictions, colour = "Predicted Values"))+
  ggtitle("Actual vs Predicted when Temperature(real temperature) is present among independent attributes") +
  labs(x = "Index Values", y="Apparent Temperature")

# observation1 neuralModel <- neuralnet(formula = form, hidden = c(2), linear.output = T, data = trainDF, stepmax=1e7, algorithm = 'backprop', learningrate = 0.0001, act.fct = "tanh")
# observation1 neuralModel <- neuralnet(formula = form, hidden = c(2,2), linear.output = T, data = trainDF, stepmax=1e7, algorithm = 'backprop', learningrate = 0.0001, act.fct = "tanh")
# observation1 neuralModel <- neuralnet(formula = form, hidden = c(4), linear.output = T, data = trainDF, stepmax=1e7, algorithm = 'backprop', learningrate = 0.0001, act.fct = "tanh")
# observation1 neuralModel <- neuralnet(formula = form, hidden = c(4,2), linear.output = T, data = trainDF, stepmax=1e7, algorithm = 'backprop', learningrate = 0.0001, act.fct = "tanh")
# observation2 neuralModel <- neuralnet(formula = form, hidden = c(4,2), linear.output = T, data = trainDF, stepmax=1e7, algorithm = "rprop+, act.fct = "tanh")
# observation2 neuralModel <- neuralnet(formula = form, hidden = c(4,2), linear.output = T, data = trainDF, stepmax=1e7, algorithm = "rprop+, act.fct = sigmoid)
# observation3 neuralModel <- neuralnet(formula = form, hidden = c(4,2), linear.output = T, data = trainDF, stepmax=1e7, algorithm = "rprop+, act.fct = relu)
