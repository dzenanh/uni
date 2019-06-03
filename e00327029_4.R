####

#  Dzenan Hamzic 
#    00327029
# 
# The R Notebook (Markdown) of this exercise can be found @ https://bit.ly/2B3gaSW
# 

####

#install.packages("magrittr")
#install.packages("dplyr", dependencies=TRUE)
library(reshape2)
library(magrittr)
library(MASS)
library(klaR)
#install.packages("splitstackshape", dependencies = TRUE)
library(splitstackshape)
set.seed(123)

### Load Data
data(OJ, package = "ISLR")
# count rows
data <- na.omit(OJ)
#nrow(data)

## 1.a) 

#### Create class variables
new_data <- data
# new_dataY <- array(rep(0), dim=c(nrow(data),2))
# for(row in 1:nrow(data)){
#   if(new_data[row,1] == "CH"){
#     new_dataY[row,1] = 1
#   }else
#     new_dataY[row, 2] = 1
# }
# 
# head(new_dataY)

#### Create class variables
# set CH as 1. 0 othervise
new_data$Purchase <- as.numeric(new_data$Purchase == "CH")
head(new_data)

#### Convert categorical to binary
new_data <- data.frame(data.matrix(new_data))

#### Split data randomly with 70/30 splits (train and test)


ind <- sample(c(TRUE, FALSE), nrow(new_data), replace=TRUE, prob=c(0.7, 0.3))
train_data <- new_data[ind, ]
test_data <- new_data[!ind, ]
head(train_data)

#### Model
mod.ind <- lm(Purchase ~ ., data = train_data)
#mod.ind2 <- lm(lpurchase ~., data = train_data)
#summary(mod.ind)
mod.ind

#### Predict
pred1 <- predict(mod.ind, newdata = test_data)
head(pred1)

#### Results
predicted <- predict(mod.ind, newdata = test_data)
TAB <- table(test_data$Purchase, predicted >= 0.5)
head(TAB)

#### Missclasification rate
mklrate<- 1-sum(diag(TAB))/sum(TAB)
mklrate

## 1.b) LDA
#I have tried transforming categorical and factor fariables to matrices with equal distances.
#Furthermore, I used principal component scores as new X data to resolve colinearity/singularity.
#### One train one test set
mod.lda<-lda(Purchase~., data = train_data)
#In lda.default(x, grouping, ...) : variables are collinear
mod.lda

#### Results (original X)
predicted <- predict(mod.lda, newdata = test_data)
TAB <- table(test_data$Purchase, predicted$class)
head(TAB)
# 0   1
# 0 87  25
# 1 33 167

#### Missclasification rate (original X)
mklrate<- 1-sum(diag(TAB))/sum(TAB)
mklrate
# 0.1858974

#### Cross Validation
mypred = function(object,newdata) UseMethod("mypred", object)
mypred.lda <- function(object, newdata){
  predict(object, newdata=newdata)$class
}
library(ipred)
CEE = control.errorest(k = 5, nboot=10)
ldacvest <- errorest(Purchase ~ ., data = data, model=lda, est.para=CEE, predict=mypred)
ldacvest

# errorest.data.frame(formula = Purchase ~ ., data = data, model = lda, 
#                     predict = mypred, est.para = CEE)
# 
# 5-fold cross-validation estimator of misclassification error 
# 
# Misclassification error:  0.1673 

### INTEPRETATION ###
# Since variables are colinear I shall TODO

#### binary and/or categorical columns to convert 
#### "StoreID", "SpecialCH", "SpecialMM", "Store7", "STORE", "rowID"
#### convert columns of the whole dataset
new_data$rowID <- 1:nrow(new_data)
head(new_data)

# columns to convert
columns_to_convert <- names(new_data) %in% c("StoreID", "SpecialCH", "SpecialMM", "Store7", "STORE", "rowID")
#columns_to_convert
selected_columns_data <- new_data[columns_to_convert]

# new data column converted
new_data_cc <- recast(selected_columns_data, rowID ~ variable + value, id.var = c("rowID"), fun.aggregate = function(x) (length(x) > 0) + 0L)

#head(new_data_cc)
original_columns <- new_data[!columns_to_convert]
new_data_columns_converted <- cbind(original_columns, new_data_cc)
new_data_columns_converted$rowID <- NULL
head(new_data_columns_converted)

#### Split converted data randomly on 70/30 splits (train and test)
ind <- sample(c(TRUE, FALSE), nrow(new_data_columns_converted), replace=TRUE, prob=c(0.7, 0.3))
# train data converted columns
train_data_cc <- new_data_columns_converted[ind, ]
# test data converted columns
test_data_cc <- new_data_columns_converted[!ind, ]
#head(train_data_cc)

#### LDA on converted X
mod.lda2 <-lda(Purchase~., data = train_data_cc)
mod.lda2

# variables are collinearCall:
#   lda(Purchase ~ ., data = train_data_cc)
# 
# Prior probabilities of groups:
#   0         1 
# 0.4023747 0.5976253 
# 
# Group means:
#   WeekofPurchase  PriceCH  PriceMM     DiscCH     DiscMM   LoyalCH SalePriceMM SalePriceCH
# 0       252.9443 1.873967 2.074328 0.02560656 0.16924590 0.3203285    1.905082    1.848361
# 1       255.5254 1.866799 2.099117 0.06532009 0.09523179 0.7324193    2.003885    1.801479
# PriceDiff  PctDiscMM  PctDiscCH ListPriceDiff StoreID_1 StoreID_2 StoreID_3  StoreID_4
# 0 0.05672131 0.08079513 0.01323538     0.2003607 0.1836066 0.2459016 0.3049180 0.06885246
# 1 0.20240618 0.04605959 0.03452180     0.2323179 0.1258278 0.1633554 0.1125828 0.17660044
# StoreID_7 SpecialCH_0 SpecialCH_1 SpecialMM_0 SpecialMM_1  Store7_1  Store7_2   STORE_0
# 0 0.1967213   0.9147541   0.0852459   0.7409836   0.2590164 0.8032787 0.1967213 0.1967213
# 1 0.4216336   0.8189845   0.1810155   0.8785872   0.1214128 0.5783664 0.4216336 0.4216336
# STORE_1   STORE_2   STORE_3    STORE_4
# 0 0.1836066 0.2459016 0.3049180 0.06885246
# 1 0.1258278 0.1633554 0.1125828 0.17660044


#plot(mod.lda2)
#### Predict
predicted <- predict(mod.lda2, newdata = test_data_cc)
TAB <- table(test_data_cc$Purchase, predicted$class)
TAB

# 0   1
# 0  96  37
# 1  19 177

#### Missclasification rate (Converted X)
mklrate<- 1-sum(diag(TAB))/sum(TAB)
mklrate

# [1] 0.1702128

### PCA-LDA
X_train = train_data_cc[-1]
X_test = test_data_cc[-1]
pca_object = princomp(X_train, center=TRUE)
summary(pca_object)

# Importance of components:
#   Comp.1      Comp.2      Comp.3      Comp.4      Comp.5       Comp.6       Comp.7       Comp.8
# Standard deviation     15.4904240 1.031895159 0.672898475 0.595626620 0.558714747 0.4907851760 0.4567481044 0.3220500058
# Proportion of Variance  0.9882532 0.004385436 0.001864836 0.001461133 0.001285647 0.0009920292 0.0008592017 0.0004271577
# Cumulative Proportion   0.9882532 0.992638595 0.994503431 0.995964565 0.997250212 0.9982422412 0.9991014428 0.9995286005
# Comp.9      Comp.10      Comp.11      Comp.12      Comp.13      Comp.14      Comp.15
# Standard deviation     0.2574784483 0.1632020827 1.304228e-01 6.705159e-02 4.152727e-03 2.239226e-03 3.183172e-09
# Proportion of Variance 0.0002730382 0.0001096966 7.005654e-05 1.851653e-05 7.102453e-08 2.065082e-08 4.173130e-20
# Cumulative Proportion  0.9998016387 0.9999113353 9.999814e-01 9.999999e-01 1.000000e+00 1.000000e+00 1.000000e+00
# Comp.16      Comp.17      Comp.18      Comp.19      Comp.20      Comp.21 Comp.22 Comp.23 Comp.24
# Standard deviation     3.177296e-09 3.162377e-09 3.152858e-09 2.652567e-09 2.268532e-09 2.366042e-10       0       0       0
# Proportion of Variance 4.157737e-20 4.118783e-20 4.094023e-20 2.897840e-20 2.119489e-20 2.305614e-22       0       0       0
# Cumulative Proportion  1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00       1       1       1
# Comp.25 Comp.26 Comp.27 Comp.28
# Standard deviation           0       0       0       0
# Proportion of Variance       0       0       0       0
# Cumulative Proportion        1       1       1       1

#### Use scores as new X
X_train_scores <- pca_object$scores
head(X_train_scores)
new_df_train_pc <- data.frame(X_train_scores)
new_df_test <- data.frame(X_test)
#colnames(new_df) = column_names
# train
new_df_train_pc["Purchase"] <- train_data_cc$Purchase

#### Project test data on principal components
test.data_pc <- predict(pca_object, newdata = new_df_test)
test.data_pc <- as.data.frame(test.data_pc)
test.data_pc["Purchase"] <- test_data_cc$Purchase
#head(test.data_pc)

#### Predict with N principal components
mat.pc.lda <- lda(Purchase ~Comp.1+Comp.2+Comp.3+Comp.4+Comp.5+Comp.6+Comp.7+Comp.8+Comp.9, new_df_train_pc)
plot(mat.pc.lda)  

#### Predict using Principal components
predicted <- predict(mat.pc.lda, newdata = test.data_pc)
#predicted$class
TAB <- table(test.data_pc$Purchase, predicted$class)
TAB
# 0   1
# 0  94  39
# 1  18 178

#### Missclasification rate PCA-LDA on 9 first components
mklrate<- 1-sum(diag(TAB))/sum(TAB)
mklrate
# [1] 0.1702128

## 1.c) QDA with PCAs
# Singularity problem can be solved by using principal components

mod.qda<-qda(formula = Purchase~Comp.1+Comp.2+Comp.3+Comp.4+Comp.5+Comp.6+Comp.7+Comp.8+Comp.9+Comp.10+Comp.11+Comp.12, data = new_df_train_pc)
predictqda <- predict(mod.qda, newdata = test.data_pc)

#### Missclasification rate with N principal components and QDA
#nrow(test_data)
#predictqda
TAB <- table(test.data_pc$Purchase, predictqda$class)
mkrqda<- 1-sum(diag(TAB)/sum(TAB))
mkrqda
# [1] 0.1793313

## 1.d) Regularized discriminant analysis with X transformed
#install.packages("klaR", dependencies = TRUE)
#detach(package:rda)     

mod.rda<-rda(Purchase~., data = train_data_cc)
predictrda<-predict(mod.rda,test_data_cc)
#predictrda
TAB<-table(test_data_cc$Purchase, predictrda$class)
mkrrda<-1-sum(diag(TAB))/sum(TAB)
mkrrda
# [1] 0.1671733
#### gamma and lambda
# Lambda in range [0,1] is there to keep the degrees of freedom flexible. It is a compromise between LDA(alpha=0) and QDA(alpha=1). It is used to select between joint and independent group covariances.
# Gamma is a regularization parameter that shrinks the covariance matrix towards the average eigenvalue.

#### Regularized discriminant analysis with PCAs
mod.rda<-rda(Purchase ~ Comp.1+Comp.2+Comp.3+Comp.4+Comp.5+Comp.6+Comp.7+Comp.8+Comp.9+Comp.10+Comp.11+Comp.12+Comp.13+Comp.14+Comp.15+Comp.16+Comp.17+Comp.18, data = new_df_train_pc, train.fraction=0.6)
#mod.rda$lambda
predictrda<-predict(mod.rda,test.data_pc)
#predictrda
TAB<-table(test_data_cc$Purchase, predictrda$class)
mkrrda<-1-sum(diag(TAB))/sum(TAB)
mkrrda

# 0.1671733 (with 18 PCs)
# 0.1666667 (with 17 PCs)


## 2. 
d <- read.csv2("/home/xxx/MScBI/S4/ClassificationAndDiscriminantAnalysis/exer4/bank.csv")
d <- na.omit(d)
#### converte "yes" to 1
d$y <- as.numeric(d$y == "yes")
#head(d)
#### convert X columns
d$rowID <- 1:nrow(d)
#head(d)

# columns to convert
columns_to_convert <- names(d) %in% c("job", "marital", "education", "default", "housing", "loan","contact","poutcome","rowID")
#columns_to_convert
selected_columns_data <- d[columns_to_convert]

# new data column converted
new_d_cc <- recast(selected_columns_data, rowID ~ variable + value, id.var = c("rowID"), fun.aggregate = function(x) (length(x) > 0) + 0L)
original_columns <- d[!columns_to_convert]
new_d_cc <- cbind(original_columns, new_d_cc)
new_d_cc$rowID <- NULL
new_d_cc <- data.frame(data.matrix(new_d_cc))

## 2.a)
sample_ind <- sample(nrow(new_d_cc), 3000)
d_train <- d[sample_ind,]
d_test <- d[-sample_ind,]
plot(d_train$y)

mod.lda3 <-lda(y~., data = d[sample_ind,])
mod.lda3

predicted3 <- predict(mod.lda3, newdata = d_test)
TAB3 <- table(d_test$y, predicted3$class)
TAB3

#### Result
mklrate<- 1-sum(diag(TAB3))/sum(TAB3)
mklrate
# [1] 0.07889546

## 2.b)
# I used 'balanced' sampling strategy with 13% of no and 80% of yes. Thus I made a new 'balanced 'train sample of ~1000 samples
# and tested on test sample with 200 samples what did not improve the recall. 

# add row id
new_d_cc$id <- 1:nrow(new_d_cc)
group_no <- new_d_cc[ which(new_d_cc$y==0),]
group_yes <- new_d_cc[ which(new_d_cc$y==1),]
#head(group_no)
#head(group_yes)

sample.balanced.no <- stratified(group_no, c("y"), .13)
#sample.balanced.no
sample.balanced.yes <- stratified(group_yes, c("y"), .8)
#sample.balanced.yes

sample.combined <- rbind(sample.balanced.no, sample.balanced.yes)
#sample.combined

commonID<-intersect(sample.combined$id, new_d_cc$id)
# test data unseen by model
test_data_dd <- new_d_cc[-commonID,]
nrow(test_data_dd)
test_data_reduced_indices <- sample(nrow(test_data_dd), 200)
test_data_dd
test_data_reduced_indices
test_data_dd[test_data_reduced_indices,]
# reduce test data set to 1000
test_data_dd <- test_data_dd[test_data_reduced_indices,]
nrow(test_data_dd)

#test_data_dd

plot(sample.combined$y)

test_data_dd$id<-NULL
sample.combined$id<-NULL

#### LDA on 'balanced' sample
mod.lda4 <-lda(y~., data = sample.combined)
mod.lda4

#### Confusion matrix
predicted4 <- predict(mod.lda4, newdata = test_data_dd)
TAB4 <- table(test_data_dd$y, predicted4$class)
TAB4
# 0   1
# 0 174  22
# 1   1   3

#### Result
mklrate<- 1-sum(diag(TAB4))/sum(TAB4)
mklrate
# 0.115



