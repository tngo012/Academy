
#title: "5.1 Assignment: Student Survey  "
#author: "Tai Ngo"
#date: January 19th 2020

  



studentSurvey <- read.csv("student-survey.csv", header = TRUE)
studentSurvey

cov(studentSurvey$TimeReading, studentSurvey$TimeTV)
plot(studentSurvey$TimeReading, studentSurvey$TimeTV)

## Answering A
## The covariance of time spent reading and time spent watching TV is equal to -20.36364.
## I used this calculation for covariance to see how these two variables, TimeReading and TimeTV, are
## linearly related because covariance can tell how two variables vary together.
## The results indicate that time spent reading and time spent watching TV are negatively related. If time
## spent reading is higher, then time spent watching TV is lower, and vice versa.

## Answering B
## For variables: TimeReading and TimeTV, the measurement being used is the unit of time. However, the exact
## unit (second, minute or hour) is not explicitly specified. For variable: Happiness, the unit is also not
## explicitly demonstrated. The measurement for gender is represented as 1 and 0; again it does not say
## whether 1 is male or female.

## The covariance calculation would greatly change if the measurement being used for the variables is 
## changed. For example, if we designated hour as the unit for TimeReading and minute as the unit for
## TimeTV, then the covariance calculation would change once we changed hour to minute for TimeReading.
## The best way to perform covariance calculation is to have the same unit on both TimeReading and TimeTV.
## Also as we provide the dataset to the users, we need to include the unit for each variable.

## Answering C

cor.test(studentSurvey$TimeReading, studentSurvey$TimeTV, method = c("pearson"))

## I choose Pearson for my correlation test. I've tried all three tests: Pearson, Kendall and Spearman
## and only Pearson works, whereas Kendall and Spearman shows errors due to being unable to compute p-value
## I predict that the curve would yield a positive correlation because I think as we spend more time 
## reading we would have less time to watch TV and vice versa.
## Using pearon correlation test, I've got correlation coefficient equal to -0.8830677. It also gives me
## the p-value = 0.0003153, which means that the correlation between TimeReading and TimeTV is 
## significant.

## Answering D

## Correlation analysis for all variables
cor(studentSurvey, method = "pearson")


##             TimeReading       TimeTV  Happiness       Gender
## TimeReading  1.00000000 -0.883067681 -0.4348663 -0.089642146
## TimeTV      -0.88306768  1.000000000  0.6365560  0.006596673
## Happiness   -0.43486633  0.636555986  1.0000000  0.157011838
## Gender      -0.08964215  0.006596673  0.1570118  1.000000000


## Correlation analysis for two variables
cor.test(studentSurvey$TimeReading, studentSurvey$TimeTV, method = c("pearson"))

## Running correlation analysis for two variables with confidence interval set at 99%
cor.test(studentSurvey$TimeReading, studentSurvey$TimeTV, method = c("pearson"), conf.level = 0.99)

## The correlation matrix suggests that the time spent reading and the happniness are negatively related,
## which means that the more time spent reading, the lower the happiness and vice versa.

## Answering E

## The correlation coefficient of TimeReading and TimeTV = -0.8830677
cor(studentSurvey$TimeReading, studentSurvey$TimeTV, method = c("pearson"))

## The coefficient of determination of TimeReading and TimeTV = 0.7798085
rH.lm <- lm(studentSurvey$TimeReading ~ studentSurvey$TimeTV, data = studentSurvey)
summary(rH.lm)$r.squared

## Answering F
## Yes, after the analysis I think that the data supports the claim "watching more TV caused students to
## read less". Because the correlation coefficient is negative, which means as time spent watching TV
## increases, the time spent reading decreases, and vice versa. Also the coefficient of determination 
## shows that although this model is not perfect, it provides a fair assessment to the outcome. Under
## normal logical sense, we have a limited time for a day, so when we spend more time on certain thing,
## we have less time for other things.

## Answering G
## The correlation coeffient of TimeTV and Happiness = 0.636556
cor(studentSurvey$TimeTV, studentSurvey$Happiness, method = c("pearson"))

## Because the correlation coefficient of TimeTV and Happiness is a positive number, their relationship
## is positively related. This result implies that as students spen more time to watch TV, they are more
## happy; whereas when they spend more time reading, they become less happy.