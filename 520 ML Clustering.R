## Bellevue University
## DSC520-T302: Statistics for Data Science 
## Title: "9.3 Assignment: Clustering"
## Author: TAI NGO
## Date: February 25, 2020

## Part a
library(tidyverse)
library(ggplot2)
mydata <- read.csv("clustering-data.csv")

ggplot(mydata, aes(x = x, y = y)) +
  geom_point(size = 0.1)

## Part 
library(factoextra)
library(cluster)

# fit the dataset with kmeans algorithm for k = 2
d.cluster <- kmeans(mydata, 2, nstart= 25)
plot(mydata, col = d.cluster$cluster)

# fit the dataset with kmeans algorithm for k = 12
d.cluster2 <- kmeans(mydata, 12, nstart=25)
plot(mydata, col = d.cluster2$cluster)


## Part c


avgsil <- function(k) {
  d.cluster <- kmeans(mydata, 2, nstart= 25)
  ss <- silhouette(d.cluster$cluster, dist(mydata))
  mean(ss[, 3])
}

kvalues <- 2:12

avgSilValues <- map_dbl(d.cluster, avgsil)

plot(kvalues, avgSilValues,
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

avgsil2 <- function(k) {
  d.cluster2 <- kmeans(mydata, 12, nstart= 25)
  ss2 <- silhouette(d.cluster2$cluster, dist(mydata))
  mean(ss2[, 3])
}


avgSilValues2 <- map_dbl(d.cluster2, avgsil2)

plot(kvalues, avgSilValues2,
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")