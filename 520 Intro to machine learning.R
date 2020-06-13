## Bellevue University
## DSC520-T302: Statistics for Data Science 
## Title: "9.2 Assignment: Introduction to Machine Learning"
## Author: TAI NGO
## Date: February 25, 2020

## Part a

library(ggplot2)

binaryData <- read.csv("binary-classifier-data.csv")
trinaryData <- read.csv("trinary-classifier-data.csv")

# plot the data for the binary classifier dataset
ggplot(data = binaryData, aes(x = x, y = y)) +
  geom_point()


# plot the data for the trinary classifier datasset
ggplot(data = trinaryData, aes(x = x, y = y)) +
  geom_point()


## Part b

library(class)
# create a KNN model for binaryData


dat.d <- sample(1:nrow(binaryData), 0.7*nrow(binaryData)) #generate random numbers from 70% of binary data
trainBinary <- binaryData[dat.d,] # 70% training data
testBinary <- binaryData[-dat.d,] # 30% test data

trainBinaryLabel <- binaryData[dat.d,1]
testBinaryLabel <- binaryData[-dat.d,1]

# with k = 3
k3.binary <- knn(train = trainBinary, test = testBinary, cl = trainBinaryLabel, k = 3)
acc.k3b <- 100 * sum(trainBinaryLabel == k3.binary)/nrow(trainBinaryLabel)
t3Binary <- table(k3.binary, testBinaryLabel)
t3Binary
acc.k3b
# with k = 5
k5.binary <- knn(train = trainBinary, test = testBinary, cl = trainBinaryLabel, k = 5)
acc.k5b <- 100 * sum(trainBinaryLabel == k5.binary)/NROW(trainBinaryLabel)
t5Binary <- table(k5.binary, testBinaryLabel)
t5Binary
acc.k5b
# with k = 10
k10.binary <- knn(train = trainBinary, test = testBinary, cl = trainBinaryLabel, k = 10)
acc.k10b <- 100 * sum(trainBinaryLabel == k10.binary)/nrow(trainBinaryLabel)
t10Binary <- table(k10.binary, testBinaryLabel)
t10Binary
acc.k10b
# with k = 15
k15.binary <- knn(train = trainBinary, test = testBinary, cl = trainBinaryLabel, k = 15)
acc.k15b <- 100 * sum(trainBinaryLabel == k15.binary)/nrow(trainBinaryLabel)
t15Binary <- table(k15.binary, testBinaryLabel)
t15Binary
acc.k15b
# with k = 20
k20.binary <- knn(train = trainBinary, test = testBinary, cl = trainBinaryLabel, k = 20)
acc.k20b <- 100 * sum(trainBinaryLabel == k20.binary)/nrow(trainBinaryLabel)
t20Binary <- table(k20.binary, testBinaryLabel)
t20Binary
acc.k20b
# with k = 25
k25.binary <- knn(train = trainBinary, test = testBinary, cl = trainBinaryLabel, k = 25)
acc.k25b <- 100 * sum(trainBinaryLabel == k25.binary)/nrow(trainBinaryLabel)
t25Binary <- table(k25.binary, testBinaryLabel)
t25Binary
acc.k25b

# Only k=5 gives me a percent accuracy of the model, which changes every time I run the script.


# create a KNN model for trinaryData


dat.d1 <- sample(1:nrow(trinaryData), 0.7*nrow(trinaryData)) #generate random numbers from 70% of binary data
trainTrinary <- trinaryData[dat.d1,] # 70% training data
testTrinary <- trinaryData[-dat.d1,] # 30% test data

trainTrinaryLabel <- trinaryData[dat.d1,1]
testTrinaryLabel <- trinaryData[-dat.d1,1]

# with k = 3
k3.trinary <- knn(train = trainTrinary, test = testTrinary, cl = trainTrinaryLabel, k = 3)
acc.k3t <- 100 * sum(trainTrinaryLabel == k3.trinary)/nrow(trainTrinaryLabel)
t3Trinary <- table(k3.trinary, testTrinaryLabel)
t3Trinary
acc.k3t
# with k = 5
k5.trinary <- knn(train = trainTrinary, test = testTrinary, cl = trainTrinaryLabel, k = 5)
acc.k5t <- 100 * sum(trainTrinaryLabel == k5.trinary)/NROW(trainTrinaryLabel)
t5Trinary <- table(k5.trinary, testTrinaryLabel)
t5Trinary
acc.k5t
# with k = 10
k10.trinary <- knn(train = trainTrinary, test = testTrinary, cl = trainTrinaryLabel, k = 10)
acc.k10t <- 100 * sum(trainTrinaryLabel == k10.trinary)/nrow(trainTrinaryLabel)
t10Trinary <- table(k10.trinary, testTrinaryLabel)
t10Trinary
acc.k10t
# with k = 15
k15.trinary <- knn(train = trainTrinary, test = testTrinary, cl = trainTrinaryLabel, k = 15)
acc.k15t <- 100 * sum(trainTrinaryLabel == k15.trinary)/nrow(trainTrinaryLabel)
t15Trinary <- table(k15.trinary, testTrinaryLabel)
t15Trinary
acc.k15t
# with k = 20
k20.trinary <- knn(train = trainTrinary, test = testTrinary, cl = trainTrinaryLabel, k = 20)
acc.k20t <- 100 * sum(trainTrinaryLabel == k20.trinary)/nrow(trainTrinaryLabel)
t20Trinary <- table(k20.trinary, testTrinaryLabel)
t20Trinary
acc.k20t
# with k = 25
k25.trinary <- knn(train = trainTrinary, test = testTrinary, cl = trainTrinaryLabel, k = 25)
acc.k25t <- 100 * sum(trainTrinaryLabel == k25.trinary)/nrow(trainTrinaryLabel)
t25Trinary <- table(k25.trinary, testTrinaryLabel)
t25Trinary
acc.k25t

# Only k=5 gives me a percent accuracy of the model, which changes every time I run the script.
