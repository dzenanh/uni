####

#  Dzenan Hamzic 
#    00327029
# 
# The R Notebook (Markdown) of this exercise can be found @ https://bit.ly/2Knnig4
# 

####


# install.packages("PerformanceAnalytics")
# install.packages("fastDummies")
# install.packages("corrplot")
library(corrplot)
set.seed(123)



### Load Data
data(OJ, package = "ISLR")
# count rows
data <- na.omit(OJ)
# set CH as 1. 0 othervise
data$Purchase <- as.numeric(data$Purchase == "CH")
nrow(data)


head(data)

## find colinear variables

res <- cor(data.matrix(data))
#round(res, 2)

corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)


# remove redundant columns that cause all kinds of collinearity problems
data$Store7 <- NULL
data$PriceDiff <- NULL
data$ListPriceDiff <- NULL
data$STORE <- NULL
data$SalePriceMM <- NULL
data$SalePriceCH <- NULL



library("PerformanceAnalytics")
chart.Correlation(data, histogram=TRUE, pch=19)


#### make binary variables from categorical

library(fastDummies)
data <- dummy_cols(data, select_columns = "StoreID")
data$StoreID <- NULL
head(data)



summary(data)



str(data)


#### Split data randomly with 70/30 splits (train and test)

n <- dim(data)[1]
is.train <- sample(1:n, ceiling(2./3. * n))
data.train <- data[is.train,]
data.test <- data[-is.train,]

data.train


## 1. Univariate case - only LocalCH for prediction

g <- glm(Purchase ~ LoyalCH, data = data.train, family = "binomial")
summary(g)



g.predict <- predict(g, newdata = data.test, type = "response")
#head(g.predict)


data.test$predicted <- as.numeric(g.predict>0.5)# g.predict > 0.5
data.test$Ppredicted <- g.predict
head(data.test)


#### Confusion Matrix & Missclasification rate

TAB <- table(data.test$Purchase, g.predict > 0.5)
TAB
mklrate<- 1-sum(diag(TAB))/sum(TAB)
mklrate


### 1.a) Plot the predictor variable versus the response variable.

plot(data.test$LoyalCH, data.test$Purchase)
#curve(predict(g, newdata = test_data, type = "response"),add=TRUE)
#plot(glm.predicted)
#plot(mylogit)



plot(Purchase~LoyalCH, data=data.train, col="red4")
lines(Ppredicted~LoyalCH, data.test, col="green4", lwd=2)


#### extract estimated coefficients

coef(g)


## 2.b)
#### predictor vs. response plot + regression line

plot(Purchase~LoyalCH, data=data.test, col="red4")
abline(coef(g)[1], coef(g)[2])


#### predicted probabilities on the logit scale

plot(Purchase~LoyalCH, data=data.test, col="black")
plot(Ppredicted~LoyalCH, data=data.test, col="red4")


#### predicted probabilities on the logit scale smoothened

library(ggplot2)
plot(Purchase~LoyalCH, data=data.test, col="black")
#qplot(Purchase~LoyalCH, data=train_data, col="red4")
qplot(data.test$LoyalCH,data.test$Ppredicted, geom='smooth')



## 2.

g_multi <- glm(Purchase ~ ., data = data.train)
summary(g_multi)


## 2.a)

#### variables with significant contribution
# LoyalCH         0.9089956  0.0500265  18.170  < 2e-16 ***
# PriceMM         0.6380209  0.1429664   4.463 9.43e-06 ***
# DiscMM         -3.9220021  1.4392600  -2.725  0.00659 **
# PctDiscMM       7.7574002  3.0227997   2.566  0.01049 * 
# SpecialMM      -0.0985816  0.0442391  -2.228  0.02617 *

#### Confusion Matrix & Missclasification rate

g_multi.predict <- predict(g_multi, newdata = data.test)
TAB2 <- table(data.test$Purchase, g_multi.predict > 0.5)
TAB2
mklrate<- 1-sum(diag(TAB2))/sum(TAB2)
mklrate


## 2.b
#### Model Selection with step "both"

min.model <- glm(Purchase~1, data = data.train)
max.model <- glm(Purchase ~ ., data = data.train)

sglm <- step(min.model, direction = "both", scope = formula(max.model))
summary(sglm)


#### Model Selection with step forward


sglmF <- step(min.model, direction = "forward", scope = formula(max.model))
summary(sglmF)


#### Most significant variables ####
# LoyalCH + DiscMM + StoreID_7 + PriceMM + PctDiscMM + PctDiscCH + SpecialMM + PriceCH

### 2.b - missclasification rate with significant variables

glm.step <- glm(Purchase ~ LoyalCH + DiscMM + StoreID_7 + PriceMM + 
                  PctDiscMM + PctDiscCH + SpecialMM + PriceCH, data=data.train)

g_step.predict <- predict(glm.step, newdata = data.test)
TAB3 <- table(data.test$Purchase, g_step.predict > 0.5)
TAB3
mklrate<- 1-sum(diag(TAB3))/sum(TAB3)
mklrate


## 2.c

anova(g, glm.step, g_multi, test="Chisq")


# Analysis of Deviance Table
# 
# Model 1: Purchase ~ LoyalCH
# Model 2: Purchase ~ LoyalCH + DiscMM + StoreID_7 + PriceMM + PctDiscMM + 
#   PctDiscCH + SpecialMM + PriceCH
# Model 3: Purchase ~ WeekofPurchase + PriceCH + PriceMM + DiscCH + DiscMM + 
#   SpecialCH + SpecialMM + LoyalCH + PctDiscMM + PctDiscCH + 
#   StoreID_1 + StoreID_7 + StoreID_2 + StoreID_3 + StoreID_4
# Resid. Df Resid. Dev Df Deviance Pr(>Chi)    
# 1       712     614.80                         
# 2       705      90.07  7   524.73   <2e-16 ***
#   3       699      89.47  6     0.60   0.5882    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

####  there is no significant difference between models 2 and 3
#### Model 2, relative to Model1, is highly significant.


## 3.

d <- read.csv2("bank.csv")
d




d$y <- as.numeric(d$y == "yes")
d$month <- as.numeric(d$month)
d <- dummy_cols(d, select_columns = c("previous","poutcome","education", "contact", "loan", "housing", "default", "education", "marital", "job"))
d$previous <- NULL
d$poutcome <- NULL
d$education <- NULL
d$contact <- NULL
d$loan <- NULL
d$housing <- NULL
d$default <- NULL
d$education <- NULL
d$marital <- NULL
d$job <- NULL
head(d)


#### Split data randomly with 70/30 splits (train and test)

n <- dim(d)[1]
is.train <- sample(1:n, ceiling(2./3. * n))
data_train <- d[is.train,]
data_test <- d[-is.train,]

data_train



g <- glm(y ~ ., data = data_train, weights = rep(0.5, nrow(data_train)))
summary(g)


#### Confusion Matrix & Missclasification rate

g_b.predict <- predict(g, newdata = data_test)
TAB2 <- table(data_test$y, g_b.predict > 0.5)
TAB2
mklrate<- 1-sum(diag(TAB2))/sum(TAB2)
mklrate

# FALSE TRUE
# 0  1286   34
# 1   147   40
# [1] 0.1201062

#### Confusion Matrix & Missclasification rate

g_b.predict <- predict(g, newdata = data_test)
TAB2 <- table(data_test$y, g_b.predict > 0.7)
TAB2
mklrate<- 1-sum(diag(TAB2))/sum(TAB2)
mklrate

# FALSE TRUE
# 0  1307   13
# 1   170   17
# [1] 0.1214333

#### I have reduced the missclasification of the "yes" clients by increasing logit regression boundry from 0.5 to 0.7.
