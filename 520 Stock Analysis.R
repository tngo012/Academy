### Bellevue University
### DSC520 - Statistics for Data Science


##Title: "Final Project"
## Author: TAI NGO
## Date: February 27, 2020


# the datasets for this project can be downloaded from the links below:
# bank stock data https://www.kaggle.com/rohan8594/stock-data
# growth stock data https://www.kaggle.com/fjgonzalez/growth-technology-stocks

library(tidyverse)
library(dplyr)

# clean up and organize the data for the dataframe of bank stocks
# four bank stocks are Bank of America, JP Morgan, Goldman Sachs and Wells Fargo

bacStock <- read.csv("BAC.csv", header = TRUE)
bacStock1 <- data.frame(bacStock$High, bacStock$Low)
names(bacStock1)[names(bacStock1) == "bacStock.High"] <- "BoA High"
names(bacStock1)[names(bacStock1) == "bacStock.Low"] <- "BoA Low"

jpmStock <- read.csv("JPM.csv", header = TRUE)
jpmStock1 <- data.frame(jpmStock$High, jpmStock$Low)
names(jpmStock1)[names(jpmStock1) == "jpmStock.High"] <- "JPM High"
names(jpmStock1)[names(jpmStock1) == "jpmStock.Low"] <- "JPM Low"

gsStock <- read.csv("GS.csv", header = TRUE)
gsStock1 <- data.frame(gsStock$High, gsStock$Low)
names(gsStock1)[names(gsStock1) == "gsStock.High"] <- "GS High"
names(gsStock1)[names(gsStock1) == "gsStock.Low"] <- "GS Low"

wfcStock <- read.csv("WFC.csv", header = TRUE)
wfcStock1 <- data.frame(wfcStock$High, wfcStock$Low)
names(wfcStock1)[names(wfcStock1) == "wfcStock.High"] <- "WFC High"
names(wfcStock1)[names(wfcStock1) == "wfcStock.Low"] <- "WFC Low"

bankStock <- data.frame(bacStock1, jpmStock1, gsStock1, wfcStock1)

boa.high <- bacStock$High
boa.low <- bacStock$Low

jpm.high <- jpmStock$High
jpm.low <- jpmStock$Low

gs.high <- gsStock$High
gs.low <- gsStock$Low

wf.high <- wfcStock$High
wf.low <- wfcStock$Low

# Calculation for the mean and percent difference of high and low values for each bank stock

boa.hmean <- mean(boa.high)
boa.lmean <- mean(boa.low)
boa.d = (boa.hmean - boa.lmean) * 50 / (boa.hmean + boa.lmean)
# % difference is 65.9% between the average of high values versus low values

jpm.hmean <- mean(jpm.high)
jpm.lmean <- mean(jpm.low)
jpm.d = (jpm.hmean - jpm.lmean) * 50 / (jpm.hmean + jpm.lmean)
# % difference is 61.2%

gs.hmean <- mean(gs.high)
gs.lmean <- mean(gs.low)
gs.d = (gs.hmean - gs.lmean) * 50 / (gs.hmean + gs.lmean)
# % difference is 63.0%

wf.hmean <- mean(wf.high)
wf.lmean <- mean(wf.low)
wf.d = (wf.hmean - wf.lmean) * 50 / (wf.hmean + wf.lmean)
# % difference is 59.4%



# data visualization: plotting the bank stocks

boxplot(bacStock$High, bacStock$Low, jpmStock$High, jpmStock$Low, gsStock$High, gsStock$Low, wfcStock$High, wfcStock$Low,
        main = "Bank Stocks from 2008 to 2016",
        xlab = "Red: BofA, Blue: JP Morgan, Yellow: Goldman Sachs, Green: Wells Fargo",
        ylab = "$ USD",
        col = c("red", "red", "blue", "blue", "yellow", "yellow", "green", "green"))



# clean up and organize the data for the dataframe of growth stocks
# four growth stocks are Salesforce, Adobe, Shopify and RingCentral

growthStock <- read.csv("growth_tech_stocks.csv")

CRM <- growthStock$X[c(87:127)]
CRM.high <- growthStock$high[c(87:127)]
CRM.low <- growthStock$low[c(87:127)]

ADBE <- growthStock$X[c(302:342)]
ADBE.high <- growthStock$high[c(302:342)]
ADBE.low <- growthStock$low[c(302:342)]

SHOP <- growthStock$X[c(388:428)]
SHOP.high <- growthStock$high[c(388:428)]
SHOP.low <- growthStock$low[c(388:428)]

RNG <- growthStock$X[c(732:772)]
RNG.high <- growthStock$high[c(732:772)]
RNG.low <- growthStock$low[c(732:772)]

gs.frame <- data.frame(CRM.high, CRM.low, ADBE.high, ADBE.low, SHOP.high, SHOP.low, RNG.high, RNG.low)
gs.name <- data.frame(CRM, ADBE, SHOP, RNG)


# calculating the mean and the percent difference of high values and low values for each growth stock

mhADBE <- mean(ADBE.high)
mlADBE <- mean(ADBE.low)
mADBE <- c(mhADBE, mlADBE)
dADBE = (mhADBE - mlADBE) * 50 / (mhADBE + mlADBE)
# % difference is 47.6%

mhCRM <- mean(CRM.high)
mlCRM <- mean(CRM.low)
mCRM <- c(mhCRM, mlCRM)
dCRM = (mhCRM - mlCRM) * 50 / (mhCRM + mlCRM)
# % difference is 55.0%

mhSHOP <- mean(SHOP.high)
mlSHOP <- mean(SHOP.low)
mSHOP <- c(mhSHOP, mlSHOP)
dSHOP = (mhSHOP - mlSHOP) * 50 / (mhSHOP + mlSHOP)
# % difference is 87.7%

mhRNG <- mean(RNG.high)
mlRNG <- mean(RNG.low)
mRNG <- c(mhRNG, mlRNG)
dRNG = (mhRNG - mlRNG) * 50 / (mhRNG + mlRNG)
# % difference is 78.9%

# data visualzation: plotting the growth stocks

boxplot(ADBE.high, ADBE.low, CRM.high, CRM.low, SHOP.high, SHOP.low, RNG.high, RNG.low,
        main = "Growth Stocks",
        xlab = "Red: Adobe, Blue: Salesforce, Yellow: Shopify, Green: RingCentral",
        ylab = "$ USD",
        col = c("red", "red", "blue", "blue", "yellow", "yellow", "green", "green"))



boxplot(mADBE, mCRM, mSHOP, mRNG, 
        main = "Growth Stocks",
        xlab = "Stock Name",
        ylab = "$ USD",
        col = (c("red", "blue","yellow", "green")))
              
              














