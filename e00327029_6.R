####

#  Dzenan Hamzic 
#    00327029
# 
# The R Notebook (Markdown) of this exercise can be found @ https://bit.ly/2r8GRQj
# 

####


#install.packages("ElemStatLearn")
library(splines)
set.seed(123)
data(bone, package="ElemStatLearn")
bone



summary(bone)


#### make binary variables from categorical


#data <- dummy_cols(bone, select_columns = "gender")
data <- bone[which(bone$gender=='male'),]
#detach(bone)
#data$gender<-NULL
head(data)


#### Split data randomly with 70/30 splits (train and test)

n <- dim(data)[1]
is.train <- sample(1:n, ceiling(2./3. * n))
data.train <- data[is.train,]
data.test <- data[-is.train,]

data.train


## 1.

## 1.a)

lmod2 = loess(spnbmd ~ age, data=data.train, span = 0.3)
lmod5 = loess(spnbmd ~ age, data=data.train, span = 0.6)
lmod7 = loess(spnbmd ~ age, data=data.train, span = 0.9)



# get smoothed output
smoothed2 <- predict(lmod2) 
smoothed5 <- predict(lmod5) 
smoothed7 <- predict(lmod7) 


#### plot

#plot(bone$spnbmd, x=bone$age, type="l", main="Loess Smoothing and Prediction", xlab="Date", ylab="Unemployment (Median)")
scatter.smooth(data$spnbmd ~ data$age, span = 0.3, col="red", main="Loess Smoothing 0.2 and Prediction")
scatter.smooth(data$spnbmd ~ data$age, span = 0.6, main="Loess Smoothing 0.5 and Prediction")
scatter.smooth(data$spnbmd ~ data$age, span = 0.9, main="Loess Smoothing 0.7 and Prediction")


#### predict

test_pred_lmod2 <- predict(lmod2, newdata = data.test)
test_pred_lmod5 <- predict(lmod5, newdata = data.test)
test_pred_lmod7 <- predict(lmod7, newdata = data.test)


## 1.b)
#### 0.3 span

plot(data.test$age, test_pred_lmod2, xlab='age', ylab='spnbmd', xlim=range(data.test$age), ylim=range(data.test$spnbmd),col="red")
par(new=T)
scatter.smooth(data.test$spnbmd ~ data.test$age, xlab='', ylab='', span = 0.3, main="Predicted (Red) vs Actual (Black) - Span 0.3", xlim=range(data.test$age), ylim=range(data.test$spnbmd))

### MSE
mean((test_pred_lmod2 - data.test$spnbmd)^2, na.rm = TRUE)


#### 0.6 span

plot(data.test$age, test_pred_lmod5, xlab='age', ylab='spnbmd', xlim=range(data.test$age), ylim=range(data.test$spnbmd),col="red")
par(new=T)
scatter.smooth(data.test$spnbmd ~ data.test$age, xlab='', ylab='', span = 0.6, main="Predicted (Red) vs Actual (Black) - Span 0.6", xlim=range(data.test$age), ylim=range(data.test$spnbmd))

### MSE
mean((test_pred_lmod5 - data.test$spnbmd)^2, na.rm = TRUE)


#### 0.9 span

plot(data.test$age, test_pred_lmod7, xlab='age', ylab='spnbmd', xlim=range(data.test$age), ylim=range(data.test$spnbmd),col="red")
par(new=T)
scatter.smooth(data.test$spnbmd ~ data.test$age, xlab='', ylab='', span = 0.9, main="Predicted (Red) vs Actual (Black) - Span 0.9", xlim=range(data.test$age), ylim=range(data.test$spnbmd))

### MSE
mean((test_pred_lmod7 - data.test$spnbmd)^2, na.rm = TRUE)


## 2. B-splines

bs3 <- bs(data.test$age, df=3)
bs6 <- bs(data.test$age, df=9)
bsCV <- smooth.spline(data.test$age, data.test$spnbmd)

matplot(bs3, type="l", lty=1)
matplot(bs6, type="l", lty=1)


### 2.a)

lm2 = lm(data.test$spnbmd ~ bs3)
lm4 = lm(data.test$spnbmd ~ bs6)

x <- sort(data.test$age)
y1 <- lm2$fitted.values[order(data.test$age)]
y2 <- lm4$fitted.values[order(data.test$age)]

# lines function produces ERROR in my setting

scatter.smooth(x,y1, main="LS Regression with B-splines - basis functions:3")
scatter.smooth(x,y2, main="LS Regression with B-splines - basis functions:9")


### 2.b)

spnbmd_predicted_bs2 = predict.lm(lm2, list(x=data.test$age))
spnbmd_predicted_bs4 = predict.lm(lm4, list(x=data.test$age))
spnbmd_predicted_bsCV = predict(bsCV, x=data.test$age)

plot(data.test$age, data.test$spnbmd, xlab='age', ylab='spnbmd', xlim=range(data.test$age), ylim=range(data.test$spnbmd),col="red", main="LS Regression with B-splines - Prediction vs. Actual")
par(new=T)
scatter.smooth(spnbmd_predicted_bs2 ~ data.test$age, col="blue", xlab='', ylab='', xlim=range(data.test$age), ylim=range(data.test$spnbmd))


plot(data.test$age, data.test$spnbmd, xlab='age', ylab='spnbmd', xlim=range(data.test$age), ylim=range(data.test$spnbmd),col="red", main="LS Regression with B-splines - Prediction vs. Actual")
par(new=T)
scatter.smooth(spnbmd_predicted_bs4 ~ data.test$age, col="blue", xlab='', ylab='', xlim=range(data.test$age), ylim=range(data.test$spnbmd))


plot(data.test$age, data.test$spnbmd, xlab='age', ylab='spnbmd', xlim=range(data.test$age), ylim=range(data.test$spnbmd),col="red", main="LS Regression with CV B-splines - Prediction vs. Actual")
par(new=D)
scatter.smooth(spnbmd_predicted_bsCV, xlab='age', ylab='spnbmd', xlim=range(data.test$age), ylim=range(data.test$spnbmd), main='')



### MSE
mean((spnbmd_predicted_bs2 - data.test$spnbmd)^2, na.rm = TRUE)
mean((spnbmd_predicted_bs4 - data.test$spnbmd)^2, na.rm = TRUE)
mean((spnbmd_predicted_bsCV$y - data.test$spnbmd)^2, na.rm = TRUE)


## 3. Natural Cubic Splines

ns1 <- ns(data.test$age, df=3)
ns2 <- ns(data.test$age, df=9)

matplot(ns1, type="l", lty=1)
matplot(ns2, type="l", lty=1)


### 3.a)

lm5 = lm(data.test$spnbmd ~ ns1)
lm6 = lm(data.test$spnbmd ~ ns2)

x <- sort(data.test$age)
y1 <- lm5$fitted.values[order(data.test$age)]
y2 <- lm6$fitted.values[order(data.test$age)]

# lines function produces ERROR in my setting

scatter.smooth(x,y1, main="Natural cubic splines - fitted 3 splines")
scatter.smooth(x,y2, main="Natural cubic splines - fitted 9 splines")


### 2.b)

spnbmd_predicted_ns1 = predict.lm(lm5, list(x=data.test$age))
spnbmd_predicted_ns2 = predict.lm(lm6, list(x=data.test$age))

plot(data.test$age, data.test$spnbmd, xlab='age', ylab='spnbmd', xlim=range(data.test$age), ylim=range(data.test$spnbmd),col="red", main="LS Regression with (3) Natural-splines - Prediction vs. Actual")
par(new=T)
scatter.smooth(spnbmd_predicted_ns1 ~ data.test$age, col="blue", xlab='', ylab='', xlim=range(data.test$age), ylim=range(data.test$spnbmd))


plot(data.test$age, data.test$spnbmd, xlab='age', ylab='spnbmd', xlim=range(data.test$age), ylim=range(data.test$spnbmd),col="red", main="LS Regression with (9) Natural-splines - Prediction vs. Actual")
par(new=T)
scatter.smooth(spnbmd_predicted_ns2 ~ data.test$age, col="blue", xlab='', ylab='', xlim=range(data.test$age), ylim=range(data.test$spnbmd))



### MSE
mean((spnbmd_predicted_ns1 - data.test$spnbmd)^2, na.rm = TRUE)
mean((spnbmd_predicted_ns2 - data.test$spnbmd)^2, na.rm = TRUE)
