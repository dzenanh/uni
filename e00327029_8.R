####

#  Dzenan Hamzic 
#    00327029
# 
# The R Notebook (Markdown) of this exercise can be found @ https://bit.ly/2rxS6C6
# 

####


#### Helper Functions

RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}


#### Load and inspect data

library(ISLR)
library(splines)
library(mgcv)
data(Auto)
plot(Auto)
head(Auto)


#### train and test sets 70/30 %

## we do not need car names for prediction purpose
Auto$name <- NULL

set.seed(123)
n <- nrow(Auto)
train <- sample(1:n,round(2/3*n))
test <- c(1:n)[-train]

plot(Auto[train,], main="Train data")
plot(Auto[test,], main="Test data")

train_ds <- Auto[train,]
test_ds <- Auto[test,]



# empty model
lm0 <- lm(mpg ~ 1, data = train_ds)

# full model
slm <- lm(mpg ~ ., data = train_ds)
summary(slm)


#### RMSE simple linear regression (BASELINE)

# empty model
RMSE(test_ds$mpg, predict.lm(lm0, newdata = test_ds))
# full model
RMSE(test_ds$mpg, predict.lm(slm, newdata = test_ds))
# [1] 7.419486
# [1] 3.326522

## 1
### 1.a)

lm1 <- lm(mpg ~ cylinders + ns(displacement, df=4) + ns(horsepower, df=4) + ns(weight, df=4) +
            ns(acceleration, df=4) + ns(year, df=4) + ns(origin, df=4), data = train_ds)
summary(lm1)
# The significant variables are:
#   (Intercept)                40.1702     3.4648  11.594  < 2e-16 ***,
# ns(horsepower, df = 4)1    -7.2847     1.8085  -4.028 7.58e-05 ***,
# ns(horsepower, df = 4)2   -12.1264     2.4399  -4.970 1.28e-06 ***,
# ns(horsepower, df = 4)3   -20.7882     4.0059  -5.189 4.53e-07 ***,
# ns(horsepower, df = 4)4   -11.1796     2.6409  -4.233 3.30e-05 ***,
# ns(weight, df = 4)1        -8.5461     2.1794  -3.921 0.000115 ***,
# ns(weight, df = 4)2        -8.4074     2.5808  -3.258 0.001288 ** ,
# ns(weight, df = 4)3       -12.0465     4.4202  -2.725 0.006903 ** ,
# ns(weight, df = 4)4        -6.9901     2.8263  -2.473 0.014093 *  ,
# ns(acceleration, df = 4)1  -5.6173     2.4282  -2.313 0.021558 *  ,
# ns(acceleration, df = 4)2  -4.9497     1.7814  -2.779 0.005897 ** ,
# ns(acceleration, df = 4)3 -10.0962     5.0607  -1.995 0.047184 *  ,
# ns(year, df = 4)2           6.6721     0.8621   7.740 2.87e-13 ***,
# ns(year, df = 4)3           6.3139     1.6330   3.867 0.000143 ***,
# ns(year, df = 4)4           8.2366     0.7326  11.242  < 2e-16 ***,
# ns(origin, df = 4)1        -2.6123     1.2931  -2.020 0.044493 *  


RMSE(test_ds$mpg, predict.lm(lm1, newdata = test_ds))
# [1] 3.020192

### 1.b)
step(lm0, direction = "both", scope = formula(lm1))


#### reduced model
lm2 <- lm(formula = mpg ~ ns(weight, df = 4) + ns(year, df = 4) + 
            ns(horsepower, df = 4) + ns(origin, df = 2) + ns(acceleration, df = 4), 
          data = train_ds)

summary(lm2)
# The significant variables are:
#   (Intercept)                47.3897     3.5856  13.217  < 2e-16 ***,
# ns(weight, df = 4)1        -8.5066     1.7440  -4.878 1.94e-06 ***,
# ns(weight, df = 4)2        -9.5221     1.8283  -5.208 4.06e-07 ***,
# ns(weight, df = 4)3       -12.2856     3.6926  -3.327 0.001013 ** ,
# ns(weight, df = 4)4        -8.9866     2.0032  -4.486 1.12e-05 ***,
# ns(year, df = 4)2           6.5506     0.8386   7.811 1.70e-13 ***,
# ns(year, df = 4)3           6.1547     1.5988   3.849 0.000151 ***,
# ns(year, df = 4)4           8.1906     0.6955  11.777  < 2e-16 ***,
# ns(horsepower, df = 4)1    -7.4711     1.7648  -4.233 3.27e-05 ***,
# ns(horsepower, df = 4)2   -10.7417     2.2472  -4.780 3.04e-06 ***,
# ns(horsepower, df = 4)3   -19.3738     3.9015  -4.966 1.29e-06 ***,
# ns(horsepower, df = 4)4   -10.9931     2.5714  -4.275 2.74e-05 ***,
# ns(origin, df = 2)1       -14.1503     5.1397  -2.753 0.006349 ** , 
# ns(acceleration, df = 4)1  -4.5320     2.0940  -2.164 0.031415 *  ,
# ns(acceleration, df = 4)2  -4.6316     1.5985  -2.897 0.004105 ** 


RMSE(test_ds$mpg, predict.lm(lm2, newdata = test_ds))
# [1] 2.961206

### 1.c)
# These plots show how individual variables contribute to the "mpg" consumption in the model.

plot(train_ds$weight, ns(train_ds$weight, df = 4)%*%lm2$coefficients[2:5])
plot(train_ds$year, ns(train_ds$year, df = 4)%*%lm2$coefficients[6:9])
plot(train_ds$horsepower, ns(train_ds$horsepower, df = 4)%*%lm2$coefficients[10:13])
plot(train_ds$origin, ns(train_ds$origin, df = 1)%*%lm2$coefficients[14:14])
plot(train_ds$acceleration, ns(train_ds$acceleration, df = 4)%*%lm2$coefficients[16:19])



## 2
### 2.a)
mod.gam <- gam(mpg ~ cylinders + s(displacement) + s(horsepower) + s(weight) 
               + s(acceleration) + s(year) + origin, data = train_ds)
summary(mod.gam)
# Most significant variables are (consecutive order):
# s(year) : complexity = 8.53->9,
# (Intercept),
# s(weight) : complexity = 2,
# s(horsepower) : compexity = 3,
# origin
# 
# Other variables are statistically insignificant.

### 2.c)
plot(mod.gam, page=1, shade=TRUE, shade.col = "green")
# The variable "year" is very "non-linear". 
# Displacement variable is most linear among others. variables "horsepower", "weight" and "acceleration" have slight nonlinearity. 
# The thicknes of the green area indicates error level.

### 2.d)
RMSE(test_ds$mpg, predict(mod.gam, test_ds))
# [1] 2.90203

### 2.e)
# Try first with reduction of non-linearity with the log scale.
mod.gam1 <- gam(mpg ~ cylinders + s(displacement) + s(horsepower) + s(weight) 
                + s(acceleration) + s(log(year)) + origin, data = train_ds)


summary(mod.gam1)
RMSE(test_ds$mpg, predict(mod.gam1, test_ds))
# [1] 2.86901

# Now, try with parameter optimization
mod.gam2 <- gam(mpg ~ cylinders + s(displacement) + s(horsepower) + s(weight) 
                + s(acceleration) + s(log(year)) + origin, data = train_ds, gamma=2.5, method = "REML", select = TRUE)

RMSE(test_ds$mpg, predict(mod.gam2, test_ds))
# [1] 2.803701
# FINAL RMSE -> 2.803701


summary(mod.gam2)
plot(mod.gam2, page=1, shade=TRUE, shade.col = "green")

