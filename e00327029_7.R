####

#  Dzenan Hamzic 
#    00327029
# 
# The R Notebook (Markdown) of this exercise can be found @ https://bit.ly/2Ea1dRO
# 

####

## 1.

library(ElemStatLearn)
data(bone)
d <- bone[bone$gender=="male",c("age","spnbmd")]
n <- nrow(d)
set.seed(123)
train <- sample(1:n,round(2/3*n))
test <- c(1:n)[-train]

plot(d[train,], main="Train data")
plot(d[test,], main="Test data")


### 1.a)

train_data <- d[train,]
test_data <- d[test,]

m1 <- smooth.spline(train_data$age, train_data$spnbmd, cv=TRUE)
plot(train_data$age, train_data$spnbmd, main="Smooth spline CV fit to train")
lines(m1, col="blue")
#Error in plot.xy(xy.coords(x, y), type = type, ...) : plot.new has not been called yet



predicted_m1 <- predict(m1, x=test_data$age)
mean((predicted_m1$y - test_data$spnbmd)^2)


# smooth.splines with CV chooses automatically number of spline basis functions.
# BS splines with df=5 from last exercise had MSE of 0.00119171. NS splines with df=5 MSE was 0.001585333.

### 1.b)

df_seq <- seq(2, 20, by=1)
for (df in df_seq){
  model <- smooth.spline(train_data$age, train_data$spnbmd, df=df)
  predicted_ <- predict(model, x=test_data$age)
  MSE <- round(mean((predicted_$y - test_data$spnbmd)^2),5)
  
  plot(test_data$age, test_data$spnbmd, xlab='age', ylab='spnbmd',  xlim=range(test_data$age),  ylim=range(test_data$spnbmd),col="blue", main=paste("Smooth spline, df=",df," - Pred. (red) vs. Actual, MSE:",MSE))
  par(new=D)
  plot(test_data$age, predicted_$y,  xlim=range(test_data$age), ylim=range(test_data$spnbmd),col="red", xlab='', ylab='')
  lines(model, col="yellow", xlab='', ylab='')
  
  print(paste("df:",df,"MSE:", MSE))
}


# Optimal df value seems to be 5. The resulting MSE is equal to the CV result from 1.a)
# 
# [1] "df: 2 MSE: 0.00147"
# [1] "df: 3 MSE: 0.00128"
# [1] "df: 4 MSE: 0.00116"
# [1] "df: 5 MSE: 0.00113"
# [1] "df: 6 MSE: 0.00114"
# [1] "df: 7 MSE: 0.00116"
# [1] "df: 8 MSE: 0.00118"
# [1] "df: 9 MSE: 0.00122"
# [1] "df: 10 MSE: 0.00126"
# [1] "df: 11 MSE: 0.0013"
# [1] "df: 12 MSE: 0.00135"
# [1] "df: 13 MSE: 0.00139"
# [1] "df: 14 MSE: 0.00144"
# [1] "df: 15 MSE: 0.00147"
# [1] "df: 16 MSE: 0.00151"
# [1] "df: 17 MSE: 0.00153"
# [1] "df: 18 MSE: 0.00156"
# [1] "df: 19 MSE: 0.00158"
# [1] "df: 20 MSE: 0.0016"

## 2.

d <- read.csv("starsdata.csv")
head(d)
plot(d)


### 2.a)

m2 <- smooth.spline(d$temp, d$light, cv=TRUE)
plot(d$temp, d$light, main="Smooth spline CV fit to DS")
lines(m2, col="blue")


### 2.b)

library(splines)

x.ns <- ns(d$temp,df=5)
lm1 <- lm(d$light~x.ns)
summary(lm1)


# Call:
#   lm(formula = d$light ~ x.ns)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1641.69  -203.30    53.96   261.54  1645.29 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2846.29     147.10 -19.350  < 2e-16 ***
#   x.ns1         413.77     142.12   2.911 0.003679 ** 
#   x.ns2         242.45     153.30   1.582 0.114071    
# x.ns3       -1440.19     232.56  -6.193 8.63e-10 ***
#   x.ns4        1286.43     342.64   3.754 0.000184 ***
#   x.ns5          38.30      92.96   0.412 0.680433    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 383.8 on 994 degrees of freedom
# Multiple R-squared:  0.3183,	Adjusted R-squared:  0.3148 
# F-statistic: 92.81 on 5 and 994 DF,  p-value: < 2.2e-16


plot(d$temp, d$light, main="NS spline df=5 fit to DS", xlim=range(d$temp),  ylim=range(d$light))
par(new=D)
plot(d$temp, predict.lm(lm1, list(x=d$temp)), col="red", main="", xlim=range(d$temp),  ylim=range(d$light))


### 2.c)

x.py <- poly(d$temp,df=5)
lm2 <- lm(d$light~x.py)
summary(lm2)


# Call:
#   lm(formula = d$light ~ x.py)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1720.68  -217.81    62.41   293.32   995.39 
# 
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)    
# (Intercept) -2442.83      12.93 -188.897  < 2e-16 ***
#   x.py1        -183.98     408.95   -0.450    0.653    
# x.py2        6691.72     408.95   16.363  < 2e-16 ***
#   x.py3         -63.42     408.95   -0.155    0.877    
# x.py4       -1799.65     408.95   -4.401  1.2e-05 ***
#   x.py5         672.89     408.95    1.645    0.100    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 408.9 on 994 degrees of freedom
# Multiple R-squared:  0.2259,	Adjusted R-squared:  0.222 
# F-statistic: 58.01 on 5 and 994 DF,  p-value: < 2.2e-16


plot(d$temp, d$light, main="Poly^5 regression fit to DS" , xlim=range(d$temp),  ylim=range(d$light))
par(new=D)
plot(d$temp, predict.lm(lm2, list(x=d$temp)), col="red", xlim=range(d$temp),  ylim=range(d$light), xlab='', ylab='')


### 2.d)


lm3 <- lm(d$light~d$temp)
summary(lm3)


# Call:
#   lm(formula = d$light ~ d$temp)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1995.61  -269.40    23.18   310.87  1235.76 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.426e+03  4.575e+01 -53.016   <2e-16 ***
#   d$temp      -1.589e-02  4.006e-02  -0.397    0.692    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 463.8 on 998 degrees of freedom
# Multiple R-squared:  0.0001576,	Adjusted R-squared:  -0.0008442 
# F-statistic: 0.1573 on 1 and 998 DF,  p-value: 0.6917


plot(d$temp, d$light, main="Linear regression fit to DS" , xlim=range(d$temp),  ylim=range(d$light))
abline(lm3)


### 2.e)

library(robustbase)
lm4 <- lmrob(d$light~d$temp)
summary(lm4)


# Call:
#   lmrob(formula = d$light ~ d$temp)
# \--> method = "MM"
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1760.87  -233.86    49.83   277.83  6867.00 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  894.039    232.538   3.845 0.000128 ***
#   d$temp        -3.274      0.230 -14.234  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Robust residual standard error: 377.4 
# Multiple R-squared:  0.2679,	Adjusted R-squared:  0.2671 


plot(d$temp, d$light, main="Robust Linear regression fit to DS" , xlim=range(d$temp),  ylim=range(d$light))
abline(lm4)


# Classic Linear model is a bad choice for nonlinear data. The classical linear model has even negative adjusted R².
# Slightly better fit, then those of classic linear, has the robust linear model. 
# Smooth spline method from 2.a tends to overfit the data (as seen from the plot). 
# This is no wonder since it searches for model with minimum CV error on the whole data. 
# Natural cubic splines with 5 splines fit the data better then Smooth spline from 2.a. 
# Poly regression shows overall best fit to the whole data set. 
# There is no overfitting, and underfitting seems resonable. 