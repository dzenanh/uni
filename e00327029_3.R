####

#  Dzenan Hamzic 
#    00327029
# 
# The R Notebook (Markdown) of this exercise can be found @ https://bit.ly/2zpHvg5
# 

####

### Clean empty rows and show number of raws again
# clean empty rows
Hitters = na.omit(Hitters)
nrow(Hitters)


### Split data randomly on 50% splits (train and test)
set.seed(0607)
ind <- sample(c(TRUE, FALSE), nrow(Hitters), replace=TRUE, prob=c(0.5, 0.5))
train_data <- Hitters[ind, ]
test_data <- Hitters[!ind, ]
#train_data
#test_data
#### X data
#names(train_data[,-19])


#### scale all but Salary
X_train <- data.frame(scale(data.matrix((train_data[, -19] ))))
X_test <- data.frame(scale(data.matrix(test_data[, -19])))
# here I choose not to scale y because model showed no performance gain on scaled y
# moreover, I wanted to be able to compare MSE with previous exercises
y_test <- test_data$Salary
#head(X_train)
#y <- as.matrix(train_data[,c("Salary")])


X_train["Salary"] <- train_data$Salary
#head(X_train)


model.ridge.basic <- lm.ridge(Salary ~ ., data = X_train, lambda = 1)
model.ridge.basic
plot(model.ridge.basic)

# 1. Ridge Regression:

## 1.a
lm_seq = seq(0,10,0.001)
head(lm_seq)


model.ridge <- lm.ridge(Salary ~ ., data = X_train, lambda = lm_seq)
#head(model.ridge)
plot(model.ridge, main="Schrinkaga of Ridge Regression")



select(model.ridge)
# modified HKB estimator is 0.7463882 
# modified L-W estimator is 19.59409 
# smallest value of GCV  at 0.322 




plot(model.ridge$lambda, model.ridge$GCV, type = "l", main="GCV of Ridge Regression")

#### Optimal lambda & minimal GCV

#head(model.ridge$GCV[,2])
#plot(model.ridge$GCV)
min_gcv <- min(model.ridge$GCV)
# find index of smallest GCV
min_gcv_index <- match(min_gcv, model.ridge$GCV)
model.ridge$GCV[min_gcv_index]
# 0.322 
# 959.7083 


#### INTEPRETATION
# Generalized cross validation GCV is a good approximation of the Leave one out CV by linear fits with quadratic errors. 
#In this case, the optimal lambda seems to be 0.322  (see both plots above) with GCV of 959.7083.


## 1.b

model.ridge.optimal <- lm.ridge(Salary ~ ., data = X_train, lambda = 0.322)


#### Regression coefficients
model.ridge.optimal$coef
# AtBat       Hits      HmRun       Runs        RBI      Walks      Years     CAtBat 
# -243.83684  277.71782   96.34941  -25.52128  -74.54042  112.10774   19.12622 -680.08896 
# CHits     CHmRun      CRuns       CRBI     CWalks     League   Division    PutOuts 
# 339.96845 -127.00449  554.47799  246.56462 -212.87731   54.76782  -41.27468   30.00531 
# Assists     Errors  NewLeague 
# 49.71976  -20.76523  -20.88672 



## 1.c
# predict with optimal ridge parameters
prediction <- as.matrix(cbind(const=1,X_test)) %*% coef(model.ridge.optimal)
head(prediction)
# [,1]
# -Alan Ashby       305.6254
# -Andres Galarraga 523.0791
# -Al Newman        373.2874
# -Andre Thornton   496.4768
# -Alan Trammell    959.7940
# -Alex Trevino     339.8108



### MSE
#model.ridge.optimal.predict <- predict(model.ridge.optimal , newdata = X_test)
#head(model.ridge.optimal.predict)
mean((y_test - prediction[,1]) ^ 2)
# [1] 112372

### MSE with using modified HKB estimator is 0.7463882 as lambda
model.ridge.optimal2 <- lm.ridge(Salary ~ ., data = X_train, lambda = 0.7463882)
# predict with optimal ridge parameters
prediction2 <- as.matrix(cbind(const=1,X_test)) %*% coef(model.ridge.optimal2)
mean((y_test - prediction2[,1]) ^ 2)
# [1] 110856.5




### Graphically
plot(prediction[,1], y_test, title("Predicted vs Measured - No log no scale"))
abline(c(0,1))
plot(log(prediction[,1]), log(y_test), title("Predicted vs Measured - log scale"))
abline(c(0,1))

#require(gridExtra)
#plot1 <- plot(prediction[,1], y_test)
#plot2 <- plot(log(prediction[,1]), log(y_test))
#grid.arrange(plot1, plot2, ncol=2)



#### INTEPRETATION 
# Ridge regression beats standard PCR on MSE. However, best subset PCR regression still performs significantly better.
# Visually, ridge shows decent fit but still with quite high predictive deviation.



# 2. Lasso Regression:
## 2.a 
X_train <- (scale(data.matrix((train_data[, -19] ))))
y_train <- train_data$Salary

lasso.model <- glmnet(X_train, y_train)

plot(lasso.model, label = TRUE)

#### INTEPRETATION
# Each curve on the plot corresponds to a variable.
# Top numbers(top axis) indicate the number of nonzero coefficients.
# Penalizaton gets stronger going from left(0 penalization) to right (max penalization).
# Less important variable parameters of are reduced to 0 (from left to right),
# thus removed alltogether based on absolute value of magnitude.
# The default value of alpha is in lasso regression is 1. 
# When alpha is 0 , Lasso regression produces the same coefficients as a linear regression.
# When alpha is very very large, all coefficients are zero.



## 2.b
cv_fit = cv.glmnet(X_train, y_train, nfolds = 10)
plot(cv_fit)

#### Optimal tuning parameters and coefficients
cv_fit$lambda.min
log(cv_fit$lambda.min)
log(cv_fit$lambda.1se)
coef(cv_fit, s = "lambda.min")

# [1] 20.24528
# [1] 3.007922
# [1] 5.147698
# 20 x 1 sparse Matrix of class "dgCMatrix"
# 1
# (Intercept) 538.095605
# AtBat         .       
# Hits         86.591423
# HmRun         .       
# Runs          6.614911
# RBI           .       
# Walks        42.090331
# Years         .       
# CAtBat        .       
# CHits        50.598520
# CHmRun        .       
# CRuns       112.914890
# CRBI          .       
# CWalks        .       
# League        9.338272
# Division    -30.518387
# PutOuts       .       
# Assists       .       
# Errors        .       
# NewLeague     .       

### Minimal model (lambda.1se)
coef(cv_fit, s = "lambda.1se")
# 20 x 1 sparse Matrix of class "dgCMatrix"
# 1
# (Intercept) 538.09560
# AtBat         .      
# Hits          .      
# HmRun         .      
# Runs          .      
# RBI           .      
# Walks         .      
# Years         .      
# CAtBat        .      
# CHits         .      
# CHmRun        .      
# CRuns        55.60107
# CRBI          .      
# CWalks        .      
# League        .      
# Division      .      
# PutOuts       .      
# Assists       .      
# Errors        .      
# NewLeague     .      


#### INTEPRETATION
 # Minimal model has least parameters and is in ragne of +/- 1 Standard error of the minimal one(lambda.min parameter).
 # On the plot above, that would be the right vertical line(lambda.1se parameter).


## 2.c
# measure with few other lambdas - (could and should be done with range in order to find
# best predictive model based on MSE)
lambdas <- c(2.91,log(cv_fit$lambda.min),4.00,log(cv_fit$lambda.1se))
lasso_prediction <- predict(cv_fit, newx = as.matrix(X_test), s=lambdas)
head(lasso_prediction)
#                       1        2        3        4
# -Alan Ashby       339.6283 343.3397 374.5321 397.4305
# -Andres Galarraga 484.7209 482.6293 464.2134 446.3149
# -Al Newman        294.7735 292.8788 276.3303 265.7142
# -Andre Thornton   569.6553 572.4283 600.5220 642.7771
# -Alan Trammell    926.6083 925.4887 913.2019 902.4187
# -Alex Trevino     335.7942 336.5141 339.8488 330.9831

#### MSE
mean((y_test - lasso_prediction[,2]) ^ 2)
# [1] 108756.9

#### MSE for all lambdas
for (model in colnames(lasso_prediction)){
  print(model) 
  print(mean((y_test - lasso_prediction[,model]) ^ 2))
}
# [1] "1"
# [1] 108771
# [1] "2"
# [1] 108756.9
# [1] "3"
# [1] 109192.4
# [1] "4"
# [1] 110305


#### PLOT
plot(lasso_prediction[,1], y_test)
abline(c(0,1))
plot(lasso_prediction[,2], y_test)
abline(c(0,1))

plot(lasso_prediction[,3], y_test)
abline(c(0,1))
plot(lasso_prediction[,4], y_test)
abline(c(0,1))
#plot(log(prediction[,1]), log(y_test))
#abline(c(0,1))


#### INTEPRETATION
#Here I have measured MSE on range of lambdas including lambda.min and lambda.1se.
#The best predictive performance shows model with lambda.min which has 7 variables.
#Amazingly, not that far away is the model with lambda.1se which, with only 1 variable, delivers pretty good(low) MSE of 110305. 
# Lasso regression, with setting variable coeficients to zero has the better predictive performance based on MSE then Ridge regression (112372, ~110000).
# Furthermore, it is more suitable for the model reduction.
