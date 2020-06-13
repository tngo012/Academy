## Title: "6.1 Assignment: GSS 2016 Survey Data"
## Author: TAI NGO
## Date: January 26, 2020


## Part 1: 
library(ggplot2)

gss16 <- read.csv("gss-2016.csv", header = TRUE)

## Part 1a
ggplot(data = gss16, aes(x = SIBS , y = CHILDS)) +
  geom_point() +
  geom_smooth(method = "lm")

## After examining the best-fit linear regression line on the scatterplot, the relationship between the 
## number of siblings of the responddent (SIBS) and the number of his or her children (CHILDS) is linearly 
## positive. The strength of the relationship is moderately strong. However, the strength of the relationship
## is getting weaker as SIBS gets higher.

## Part 1b
SIBS <- gss16$SIBS
CHILDS <- gss16$CHILDS
cov(SIBS, CHILDS, use = "complete.obs", method = "pearson")
## The covariance of SIBS and CHILDS is equal to 1.06853.
## I choose to use cov because it is the function to calculate the covariance for two variables in R.
## I choose to use pearson for my method because pearson for normally distributed data. 
## With covariance equal to 1, it suggests that the number of siblings of the respondent and his/her 
## number of childrens have a strong, positive relationship.

## Part 1c
## There are four types of correlation tests: Pearson, Kendall, Spearman and Point-Biserial.
## I choose to use the Pearson correlation test because it works for normally distributed data. In addition,
## when I tried to use other correlation tests, I receive no result.
## I predict that the Pearson correlation test will yield positive result.

## Part 1d
cor(SIBS, CHILDS, use = "complete.obs",method = "pearson")
## the correlation coefficient is equal to 0.1988582. The correlation suggests that there is a weak, 
## positive relationship between two variables. The correlation coeficient is a positive number, so
## the relationship is posisitve. As the number of siblings of the respondent increases, it is likely
## that his or her number of childrens also increases and vice versa. However, since the coefficient
## is very close to zero, it suggests that the relationship is weak.

## Part 1e
## The correlation coefficient is equal to 0.1988582. 
rH.lm <- lm(CHILDS ~ SIBS)
summary(rH.lm)$r.squared
## The coefficient of determination is equal to 0.03954459.
## The correlation coefficient tells us that the relationship between two variables are weak. 
## The coefficient of determination is fairly low, which suggests that the real data is not very close
## to its fitted regression line. 

## Part 1f
## Based on my analysis, the data suggests that there is a linear, positive relationship between the 
## number of siblings of the respondent and the number of his or her children. However, this relationship
## is not strong, which indicates that we should not strongly believe in this result and further analysis
## with different techniques may need to be applied.

## Part 1g
plot(SIBS, CHILDS)
## This graph provides some useful information such as where the points are most concentrated at. 
## However, when it comes to further analysis to draw any meaningful conclusion, the graphs fails to do so.
## One of the main reasons is because it does not represent the data by using the dots very well. We cannot
## visualy look at the graph and see any sort of trends or form a persuasive conclusion.
## Regarding the skewness of the graph, on the x-axis the graph is heavily skewed toward the right-hand side.
## On the y-axis the graph has almost no skew.

## Part 2a
mod <- lm(CHILDS ~ SIBS)
summary(mod)

## Part 2b
## The intercept is 1.467767. The slope is 0.103577.
## The coefficient of determination is 0.03954
cor(SIBS, CHILDS, use = "complete.obs")
## The correlation coefficient is 0.1988582.

## Part 2c
hist(CHILDS)
## The number of childrens varies from 0 up to 8. After drawing a historgram for CHILDS, the graph seems
## to skew toward the right hand side, which suggests that people tend to have less than 4 childrens.
## The graph shows most people have zero children, and the second highest is 2 childrens.
hist(SIBS)
## For the number of siblings, the amount of variation for having more than 10 siblings is not explained.

## Part 2d
## Based on the calculated F-ratio, this regression model results in a better prediction of the number of
## children than if I had chosen to use the mean value of siblings.

## Part 2e
new_data <- data.frame(SIBS = 3)
predict(mod, newdata = new_data)
## With number of siblings equal to 3, this model predicts that the number of children will be 1.778498,


## Part 2f
new2 <- data.frame(SIBS = 0)
predict(lm(CHILDS ~ SIBS), newdata = new2)
## With the number of siblings equal to 0, this model predicts that the number of children will be 1.467767.


