## Title: "7.1 Assignment: Housing Data"
## Author: TAI NGO
## Date: February 05, 2020

# Part a
# skipped

# Part b
library(tidyr)
library(readxl)
housing <- read_excel("week-7-housing.xlsx")
salePrice <- housing$`Sale Price`
sqFtLot <- housing$sq_ft_lot
bedRoom <- housing$bedrooms
bathFullCount <- housing$bath_full_count

mod <- lm(salePrice ~ sqFtLot)

#Output
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-2016064  -194842   -63293    91565  3735109 

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 6.418e+05  3.800e+03  168.90   <2e-16 ***
#sqFtLot     8.510e-01  6.217e-02   13.69   <2e-16 ***


#Residual standard error: 401500 on 12863 degrees of freedom
#Multiple R-squared:  0.01435,	Adjusted R-squared:  0.01428 
#F-statistic: 187.3 on 1 and 12863 DF,  p-value: < 2.2e-16


mod1 <- lm(salePrice ~ bedRoom + bathFullCount)

#Output
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-3566590  -157368   -55794    67256  3891256 

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     148987      14890   10.01   <2e-16 ***
#bedRoom          70566       4048   17.43   <2e-16 ***


#Residual standard error: 383200 on 12862 degrees of freedom
#Multiple R-squared:  0.1023,	Adjusted R-squared:  0.1022 
#F-statistic: 733.2 on 2 and 12862 DF,  p-value: < 2.2e-16


# Part c
summary(mod)
# R2 = 0.01435
# Adjusted R2 = 0.01428
summary(mod1)
# R2 = 0.1023
# Adjusted R2 = 0.1022

# The R2, or the coefficient of determination, tells us how close the data are to the fitted regression
# line. Because the R2 of SalePrice with the SquaredFootLot  is 10 times smaller than the R2 of other
# additional indicators, this suggests that additional indicators help explain the large variation
# in the Sale Price much better. In other words, the bed rooms and the bath count have much higher impact
# on the sale price than the square foot lot alone.

# Part d
library(QuantPsyc)
lm.beta(mod)
lm.beta(mod1)

#Output
#> lm.beta(mod)
#sqFtLot 
#0.1198122 

#> lm.beta(mod1)
#bedRoom bathFullCount 
#0.1528878     0.2382795 


# After obtained the standardized betas for each parameter, the result is as followed:
# the standardized beta for the square foot lot is equal to 0.1198122
# the standardized beta for the bed room is 0.1528878
# the standardized beta for the bath count is 0.2382795
# The standardized beta shows how much each variable is associated with the outcome variable. Therefore,
# the result shows that the bath count has the most impact. The bed room has the second highest impact,
# and the square foot lot has the lowest impact out of the three variables on the sale price.

# Part e

library(Rmisc)

CI(salePrice, ci = 0.95)
#   upper     mean    lower 
#667726.1 660737.7 653749.4 
# The 95% confidence interval for the sale price is between 653749.4 to 667726.1, 
# with the mean equal 660737


CI(sqFtLot, ci = 0.95)
#upper     mean    lower 
#23212.47 22228.57 21244.67 
# The 95% confidence interval for the square foot lot is between 21244.67 to 23212.47,
# with the mean equal 22228.57

CI(bedRoom, ci = 0.95)
#upper     mean    lower 
#3.493804 3.478663 3.463522 
# The 95% confidence interval for the bed room is between 3.463522 to 3.493804, 
# with the mean equal 3.478663

CI(bathFullCount, ci = 0.95)
#upper     mean    lower 
#1.809692 1.798445 1.787199 
# The 95% confidence interval for the bath room count is between 1.787199 to 1.809692,
# with the mean equal 1.798445

# Part f
aov1 <- aov(salePrice ~ sqFtLot)
summary(aov1)
aov2 <- aov(salePrice ~ bedRoom + bathFullCount)
summary(aov2)
# The result shows that the F-value for the first model (Sale Price ~ square foot lot) is 187.3
# The result shows that the F-value for the second model (Sale Price ~ bed room + bath full count) is
# 728.4 for bedRoom and 738 for bathFullCount. 
# The F-value in the first model is significantly smaller than the F-values in the second model.
# The ANOVA confirms that the second model is a huge improvement over the first model.

# Part g

# casewise diagnostics for the function of Sale Price vs Square Foot lot
cdModel1 <- resid(mod)
# casewise diagnostics for the function of Sale Price vs Bed Room and Bath Full Count
cdModel2 <- resid(mod1)


# Part h

# create the standardized residual for the first linear model (sale price vs square foot lot)
mod.stdres <- rstandard(mod)
mod.largeresid <- mod.stdres > 2 | mod.stdres < -2

# create the standardized residual for the second linear model (sale price vs bed room + bath full count)
mod1.stdres <- rstandard(mod1)
mod1.largeresid <- mod1.stdres > 2 | mod.stdres < -2

# Create a new data frame for the newly created variables
stdres.table <- data.frame(mod.stdres, mod1.stdres)


#Part i
sum(mod.largeresid)
# The sum of large residuals for this model is 334
sum(mod1.largeresid)
# The sum of large residuals for this model is also 334

# Part j
largeRes1 <- data.frame(mod.largeresid = TRUE, salePrice, sqFtLot)
largeRes2 <- data.frame(mod1.largeresid = TRUE, salePrice, bedRoom, bathFullCount)
# According to the result, all variables (Sale Price, Square Foot Lot, Bed Room and Bath Full Count)
# have large residuals.

# Part k

mod.leverage <- hatvalues(mod)
mod.cooksd <- cooks.distance(mod)
mod.covratio <- covratio(mod)
# the result does not look problematic.

mod.leverage <- hatvalues(mod1)
mod.cooksd <- cooks.distance(mod1)
mod.covratio <- covratio(mod1)
# the result does not look problematic.

# Part l
library(lmtest)
dwtest(mod)
# Output
#data:  mod
#DW = 0.73804, p-value < 2.2e-16
#alternative hypothesis: true autocorrelation is greater than 0
# the condition is met
dwtest(mod1)

# Output
#data:  mod1
#DW = 0.71677, p-value < 2.2e-16
#alternative hypothesis: true autocorrelation is greater than 0
# the condition is met


# Part m


library(carData)

vif(mod1)
# the variance inflation factor for both variables bedRoom and bathFullCount is equal to 1.102269.
# because the smallest possible value of VIF is 1, and our result is very close to 1, which indicates that
# there is an absence of multicollinearity. Therefore, the condition is met.

# Part n
plot(cdModel1)
# The plot of the residuals for the first linear model shows anomalies are present, and they are more floating across
# the x-axis and around the 1x10^6 to 3x10^6 on the y-axis.

plot(cdModel2)
# The plot of the residuals for the second linear model shows similar pattern and trend as in the previous plot. The anomalies
# are shown across the entire x-axis, and on the 2x10^6 to 4x10^6 of the y-axis.

hist(cdModel1)
# The histogram displays the anomalies better than the plot. It shows that the anomalies concentrate the most at 3x10^6

hist(cdModel2)
# This histogram displays exactly the same pattern as the previous histogram, the anomolies show up the most near the 
# 3x10^6.

# Part o
# Overall, this regression model can be considered to be unbiased since it passes many tests. Because it is unbiased,
# its sample can represent the entire population model fairly well. It tells us that we can rely on using the bedRoom and
# bathFullCount to predict the sale price since they are proven to be good predictors as demonstrated in the second linear
# model.
