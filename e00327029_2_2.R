####

#  Dzenan Hamzic 
#    00327029
# 
# The R Notebook (Markdown) of this exercise can be found @ https://bit.ly/2z1WTzj
# or @ http://62.75.171.30/ds/

####


library(pls)
library(leaps)
library(devtools)
library(ggbiplot)

### Load Data
# load data
data(Hitters, package = "ISLR")
# count rows
nrow(Hitters)

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


## 1. Principal component regression (PCR)
#(a)
model.pcr<-pcr(Salary~., ncomp=19, validation="CV", segments=10, scale=TRUE, data=train_data)
summ.model <- summary(model.pcr)
#str(summ.model)


# Data: 	X dimension: 129 19 
# Y dimension: 129 1
# Fit method: svdpc
# Number of components considered: 19

# VALIDATION: RMSEP
# Cross-validated using 10 random segments.
# (Intercept)  1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps  9 comps  10 comps  11 comps
# CV           433.6    363.0    368.3    367.2    369.6    366.7    368.4    368.6    372.2    376.6     381.7     383.9
# adjCV        433.6    362.6    367.5    366.4    368.7    365.6    367.2    367.2    370.6    374.7     379.7     381.4
# 12 comps  13 comps  14 comps  15 comps  16 comps  17 comps  18 comps  19 comps
# CV        386.0     386.2     378.8     380.7     384.8     381.3     367.5     368.4
# adjCV     384.6     383.6     376.1     377.7     381.5     378.0     364.0     364.8
# 
# TRAINING: % variance explained
# 1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps  9 comps  10 comps  11 comps  12 comps
# X         38.70    60.91    72.15    79.73    85.26    89.56    92.92    95.34    96.72     97.62     98.31     98.77
# Salary    31.43    32.12    33.26    33.53    35.63    35.86    36.85    37.01    37.07     37.10     37.65     38.07
# 13 comps  14 comps  15 comps  16 comps  17 comps  18 comps  19 comps
# X          99.23     99.53     99.77     99.89     99.96     99.99    100.00
# Salary     39.39     41.88     42.68     43.43     45.65     50.37     50.66


### INTEPRETATION ###
# Two main results are printed, the cross validation error and the cumulative percentage of variance explained using n components.
# 
# Cross-validation:
# Root mean squared error on prediction RMSEP is measured with n components and CV.
# The optimal model has the lowest adjCV, which in this case, seems to be model with 18 principal 
# components with lowest adjCV of 364.1 Interestingly, the model with only 1 component shows suprisingly high predictive performance. 
# One component predictor deliveres CV RMSEP of 365.4(which is even lower then with 18 components) and adjCV of 364.9 which is slightly higher then with 18 components.
# 
# I shall compare MSE of both models (with 1 and with 18 components) below.
# 
# TRAINING: % variance explained:
# This parameter shows how much of the original datasets variance is described by every component 
# and cummulatively with each additional component.
# The 'optimal' model with 18 components describes 99.99% of datasets variance. 


##(1.b) Obtained CV prediction errors Plot

plot(model.pcr, plottype = "validation", val.type = "RMSEP", legend = "topright")
# or alternatively
# MSEP Plot
validationplot(model.pcr, val.type="MSEP")
### INTEPRETATION ###
#The optimal model seems to have 18 principal component with MSE of 117249.4 (see below). 
# In comparison to MSE (116160.1 with 5 components) from last exercise, the MSE is slightly higher.
# Comparatively, the optimal model in exercase 1 (at least in my case) hat 5 predictor variables what
# demonstrates predictive power of PCR with small number of principal components.

## MSE valuation considering the PCR optimal model (18 components)

optimal.pcr.model <- pcr(Salary~., ncomp=19, segments=10, validation="CV", scale=TRUE, data=train_data)
pred_test_pcr <- predict(optimal.pcr.model, test_data, ncomp = 18)
mean((test_data$Salary - pred_test_pcr) ^ 2)
#[1] 115183.1

## MSE valuation considering the PCR optimal model (1 component) - just for comparison
optimal.pcr.model2 <- pcr(Salary~., ncomp=19, validation="CV", segments=10, scale=TRUE, data=train_data)
pred_test_pcr2 <- predict(optimal.pcr.model2, test_data, ncomp= 5)
mean((test_data$Salary - pred_test_pcr2) ^ 2)
#[1] 113381.1


### 1.c optimal model with 18 predictors (based on train data)
# optimal model with 18 predictors
predplot(optimal.pcr.model)

### 1.c optimal(second best-this is just for exploration purposes :) model with 1 predictors (based on train data)
predplot(optimal.pcr.model2)


# 2. Partial least squares regression (PLS):
### 2.a
model.plsr<-plsr(Salary~. , validation="CV", segments=10 ,scale=TRUE, data=train_data)
summary(model.plsr)

# Data: 	X dimension: 129 19 
# Y dimension: 129 1
# Fit method: kernelpls
# Number of components considered: 19
# 
# VALIDATION: RMSEP
# Cross-validated using 10 random segments.
# (Intercept)  1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps  9 comps  10 comps
# CV           433.6    369.6    379.1    371.7    383.2    389.6    394.0    390.5    382.5    376.1     382.6
# adjCV        433.6    368.8    377.1    370.6    380.8    386.2    389.7    386.2    379.3    373.3     379.0
# 11 comps  12 comps  13 comps  14 comps  15 comps  16 comps  17 comps  18 comps  19 comps
# CV        380.7     377.7     373.9     372.1     370.6     369.6     369.9     370.4     371.6
# adjCV     377.2     373.7     370.1     368.6     367.0     366.1     366.3     366.8     367.9
# 
# TRAINING: % variance explained
# 1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps  9 comps  10 comps  11 comps  12 comps
# X         38.43    51.15    66.56    75.95    79.69    83.70    88.77    91.12    93.57     95.89     97.10      97.8
# Salary    33.60    36.75    38.07    39.26    42.05    44.13    44.87    45.78    46.76     47.53     48.55      49.5
# 13 comps  14 comps  15 comps  16 comps  17 comps  18 comps  19 comps
# X          98.30     98.95     99.21     99.42     99.62     99.99    100.00
# Salary     49.95     50.14     50.42     50.49     50.52     50.53     50.66

# INTEPRETATION
# Same as above.
# The lowest RMSE from CV is taken as the best model (with p number of components).
# Variance is explained above.

### 2.b
plot(RMSEP(model.plsr), legendpos = "topright")

# 16 components seem to be optimal (based on adjusted CV)
# 1 component seems to be optimal (based on CV)

## 2.b MSE valuation considering the optimal PLS model (16 component)
# optimal model
optimal.model.plsr <- plsr(Salary ~ ., validation="CV", segments=10 , scale=TRUE, data=train_data, ncomp=16)
pred.optimal.model.plsr <- predict(optimal.model.plsr, test_data)
mean((test_data$Salary - pred.optimal.model.plsr) ^ 2)
# [1] 114457.5

### 2.c - measured y vs predicted y plot
predplot(optimal.model.plsr)


# 3. PCR with variable selection:
### (3.1)

# X (18 predictors)
# all columns except "Salary""
# train
X <- scale(data.matrix((train_data[, !names(train_data) %in% c("Salary")] )))
y <- as.matrix(train_data[,c("Salary")])
# test
X_test <- scale(data.matrix((test_data[, !names(train_data) %in% c("Salary")] )))
y_test <- as.matrix(test_data[,c("Salary")])

#head(y)


column_names <- names(train_data[, !names(train_data) %in% c("Salary")] )
column_names

pca_object = princomp(X, center=TRUE)
#pca_object
#summary(pca_object)
#head(y)
#plot(pca_object, type = "l")

### (3.2) Biplot
ggbiplot(pca_object)
### INTEPRETATION ###
# The axes of the biplot show 2 principal components (PC1&PC2) and % of explained variance in the dataset. 
# This plot shows individuals, the first two PC scores and the loading vertors in a singple biplot display.

## 3.3
### Principal Component Scores as new X train
new_X <- pca_object$scores
#head(new_X)

### Create new data frame
#new_X
new_df <- data.frame(new_X)
new_df_test <- data.frame(X_test)
#colnames(new_df) = column_names
# train
new_df["Salary"] <- y

## Project test data on principal components
test.data <- predict(pca_object, newdata = new_df_test)
test.data <- as.data.frame(test.data)
test.data["Salary"] <- y_test


# Best Subset Regression on Principal Component Scores
lm.regsubset<-regsubsets(Salary ~ ., data=new_df, nbest = 3, nvmax = 10)
summary(lm.regsubset)


## Find the best subset regression based on principal component scores
#plot(lm.regsubset)

##3. PCR with variable selection:
### 3) Can you beat the results from standard PCR? -> YES 
model.pcr.subset <- pcr(Salary ~ Comp.1 + Comp.14  + Comp.17 + Comp.18, data = new_df, validation="CV", segments=10)
summary(model.pcr.subset)

# Data: 	X dimension: 129 4 
# Y dimension: 129 1
# Fit method: svdpc
# Number of components considered: 4
# 
# VALIDATION: RMSEP
# Cross-validated using 10 random segments.
# (Intercept)  1 comps  2 comps  3 comps  4 comps
# CV           433.6    362.2    358.7    356.3    349.8
# adjCV        433.6    361.9    358.2    355.6    348.7
# 
# TRAINING: % variance explained
# 1 comps  2 comps  3 comps  4 comps
# X         98.97    99.73    99.91   100.00
# Salary    31.43    33.92    36.15    40.86


### Predict with best subset and calculate MSE
pred.optimal.model.plsr <- predict(model.pcr.subset , newdata = test.data)
mean((test.data$Salary - pred.optimal.model.plsr[,,3]) ^ 2)
### MSE from best subset principal component regression -> better then standard PCR
# [1] 105287.1
#This result beats standard PCR with no "best" variable selection. Standard PCR MSE: 115183.1


