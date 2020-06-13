## Title: "8.1 Assignment: Fit a Logistic Regression Model to the Thoracic Surgery Binary Dataset"
## Author: TAI NGO
## Date: February 09, 2020


library(foreign)

myData <- read.arff("ThoraricSurgery.arff")

# Part a
patientSurvived <- glm(formula = Risk1Yr ~ PRE4 + PRE5 + PRE6 + PRE7 + PRE8 + PRE9 + PRE10 + PRE11 + PRE14
                       + PRE17 + PRE19 + PRE25 + PRE30 + PRE32 + AGE, data = myData, family = binomial)
summary(patientSurvived)

# Output
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.7065  -0.5556  -0.4576  -0.3301   2.6082  

#Coefficients:
#             Estimate Std. Error z value Pr(>|z|)   
#(Intercept) -2.262e+00  1.393e+00  -1.624  0.10430   
#PRE4        -1.581e-01  1.758e-01  -0.899  0.36850   
#PRE5        -2.246e-02  1.697e-02  -1.324  0.18554   
#PRE6PRZ1    -4.713e-01  5.073e-01  -0.929  0.35294   
#PRE6PRZ2    -2.844e-01  7.596e-01  -0.374  0.70809   
#PRE7T        6.323e-01  5.339e-01   1.184  0.23631   
#PRE8T        2.593e-01  3.723e-01   0.696  0.48613   
#PRE9T        1.185e+00  4.771e-01   2.483  0.01301 * 
#PRE10T       4.836e-01  4.726e-01   1.023  0.30628   
#PRE11T       5.423e-01  3.866e-01   1.403  0.16063   
#PRE14OC12    4.387e-01  3.195e-01   1.373  0.16974   
#PRE14OC13    1.281e+00  5.904e-01   2.170  0.03000 * 
#PRE14OC14    1.674e+00  5.804e-01   2.884  0.00392 **
#PRE17T       9.511e-01  4.307e-01   2.208  0.02723 * 
#PRE19T      -1.380e+01  1.003e+03  -0.014  0.98902   
#PRE25T       3.013e-01  8.910e-01   0.338  0.73526   
#PRE30T       7.976e-01  4.486e-01   1.778  0.07541 . 
#PRE32T      -1.325e+01  1.002e+03  -0.013  0.98945   
#AGE         -6.039e-03  1.722e-02  -0.351  0.72588   
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 395.61  on 469  degrees of freedom
#Residual deviance: 359.28  on 451  degrees of freedom
#AIC: 397.28

#Number of Fisher Scoring iterations: 14


# Part b
# The variable that has the greatest effect on the survival rate is PRE140C14 because its estimate is the
# highest at 1.674

# Part c

# Predict the outcome variable
predOutcome <- predict(patientSurvived, myData,type ="response")
summary(predOutcome)

# Output
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.7065  -0.5556  -0.4576  -0.3301   2.6082  
#Coefficients: .... [TRUNCATED] 
  

#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0000001 0.0846751 0.1197241 0.1489362 0.1783867 0.7668386 

confmatrix <- table(Actual_value = myData$Risk1Yr, Predicted_value = predOutcome >0.5)
confmatrix

# Output
#               Predicted_value
# Actual_value  FALSE TRUE
#           F   395    5
#           T    69    1

# calculate the percent of accurate predictions
(confmatrix[[1,1]] + confmatrix[[2,2]])/sum(confmatrix)
# The percent of accurate predictions is 84.26%

