####

#  Dzenan Hamzic 
#    00327029
# 
# The R Notebook (Markdown) of this exercise can be found @ https://bit.ly/2FqnCuf
# 

####



#install.packages("DMwR?", dependencies = TRUE)
library(DMwR)
#install.packages("randomForest")
library(randomForest)


#### Helper Functions

MissCRate = function(tab){
  1-sum(diag(tab))/sum(tab)
}

RF_train_test = function(traind, testd, strategy){
  rf <- randomForest(y ~ ., data=traind)
  predict <- predict(rf, testd, type = "class")
  tab <- table(testd$y, predict)
  print(strategy)
  print(tab)
  print(MissCRate(tab))
}



bankdata <- read.csv2("bank.csv")
head(bankdata)
table(bankdata$y)



set.seed(123)
n <- nrow(bankdata)
train <- sample(1:n,round(0.8*n))
test <- c(1:n)[-train]

train_df <- bankdata[train,]
test_df <- bankdata[test,]
table(train_df$y)


## 1.
bank.rf <- randomForest(y ~ ., data=bankdata, subset = train, importance=TRUE)
plot(bank.rf)
varImpPlot(bank.rf)

#First plot: Eror rates for different tree sizes.
#Second plot : variable importances.

### Baseline
T0.predict <- predict(bank.rf, bankdata[test,], type = "class")
T0.tab <- table(bankdata[test, "y"], T0.predict)
print(T0.tab)
MissCRate(T0.tab)


#T0.predict
#no yes
#no  768  25
#yes  67  44
#[1] 0.1017699


## 2.

### 2.a - Oversampling, undersampling, same-size-sampling
train_no <- train_df[train_df$y == "no", ]
train_yes <- train_df[train_df$y == "yes", ]

### oversampling
yes_supersample <- train_yes[sample.int(nrow(train_yes),size=3207,replace=TRUE),]
train_oversampled <- rbind(train_no, yes_supersample)
table(train_oversampled$y)

### undersampling
no_undersampling <- train_no[sample.int(nrow(train_no),size=410,replace=FALSE),]
train_undersampling <- rbind(train_yes, no_undersampling)
table(train_undersampling$y)

### same-size sampling
no_equal <- train_no[sample.int(nrow(train_no),size=300,replace=FALSE),]
yes_equal <- train_yes[sample.int(nrow(train_yes),size=300,replace=FALSE),]
train_equal <- rbind(no_equal, yes_equal)
table(train_equal$y)



RF_train_test(train_oversampled, test_df, "oversampling")
RF_train_test(train_undersampling, test_df, "undersampling")
RF_train_test(train_equal, test_df, "same-size")


# [1] "oversampling"
# predict
# no yes
# no  762  31
# yes  58  53
# [1] 0.09845133
# [1] "undersampling"
# predict
# no yes
# no  638 155
# yes   9 102
# [1] 0.1814159
# [1] "same-size"
# predict
# no yes
# no  618 175
# yes   6 105
# [1] 0.2002212

### 2.b - Modify the parameter sampsize
for (samps in c(20,50,100,200,500,1000,2000,3000,3500, nrow(train_df))){
  rf1 <- randomForest(y ~ ., data=train_df, sampsize=samps)
  T1.predict <- predict(rf1, bankdata[test,], type = "class")
  T1.tab <- table(test_df$y, T1.predict)
  print(samps)
  print(T1.tab)
  print(MissCRate(T1.tab))
}

#Parameter "sampsize" defines the size of a sample taken by a tree in random forest. 


### 2.c - Modify the parameter classwt
for (cw in list(list(1E-5,1E5), list(1E5,1E-5), list(0.1,500000))){
  print(cw)
  rf2 <- randomForest(y ~ ., data=train_df, classwt = cw) 
  T2.predict <- predict(rf2, bankdata[test,], type = "class")
  T2.tab <- table(test_df$y, T2.predict)
  print(samps)
  print(T2.tab)
  print(MissCRate(T2.tab))
  
}

# Parameter "classwt" defines the class weights in a sample taken by a tree in the forest.
# It is used if the data is unbalanced to improve classification performance in a smaller class.


### 2.d - Modify the parameter cutoff
for (co in list(c(1/2,1/2), c(0.2,0.8), c(0.8,0.2), c(0.7,0.3), c(0.9,0.1))){
  print(co)
  rf2 <- randomForest(y ~ ., data=train_df, cutoff = co) 
  T2.predict <- predict(rf2, bankdata[test,], type = "class")
  T2.tab <- table(test_df$y, T2.predict)
  print(samps)
  print(T2.tab)
  print(MissCRate(T2.tab))
}
# The cutoff parameter is used to define ensamble voting majority level. 


### 2.e - Modify the parameter strata
for (ss in list(c(50,50), c(50,100), c(100,300), c(300,100))){
  print(ss)
  rf2 <- randomForest(y ~ ., data=train_df, sampsize=ss, strata=train_df$y)
  T2.predict <- predict(rf2, bankdata[test,], type = "class")
  T2.tab <- table(test_df$y, T2.predict)
  print(samps)
  print(T2.tab)
  print(MissCRate(T2.tab))
}
# Parameter strata enables individual sample sizes for different classes in a dataset.


### 2.f - Use the function SMOTE to generate new artificial observations for the smaller class
train_df$y <- as.factor(train_df$y)
table(train_df$y)
test_df$y <- as.factor(test_df$y)
newData <- SMOTE(y ~ ., train_df)
table(newData$y)
RF_train_test(newData, test_df, "SMOTE")


# [1] "SMOTE"
# predict
# no yes
# no  693 100
# yes  26  85
# [1] 0.1393805
# Compared to the baseline, prediction performance of "yes" target has improved. 
# Overall, there is a slight decrease in pred. performance.



## Combining multiple approaches
train_no2 <- newData[newData$y == "no", ]
no_undersampling2 <- train_no2[sample.int(nrow(train_no2),size=1230,replace=FALSE),]
train_undersampling2 <- rbind(newData[newData$y == "yes", ], no_undersampling2)
table(train_undersampling2$y)

RF_train_test(train_undersampling2, test_df, "SMOTE+undersampling")

# no  yes 
# 1230 1230 
# [1] "SMOTE+undersampling"
# predict
# no yes
# no  675 118
# yes  21  90
# [1] 0.1537611


### Combining SMOTE, large sampsize, undersampling, and cutoff
for (co in list(c(1/2,1/2), c(0.2,0.8), c(0.8,0.2), c(0.6,0.4),c(0.65,0.35),c(0.4,0.6),c(0.7,0.3), c(0.9,0.1))){
  print(co)
  for (ss2 in c(50,100,200,400,1000,2000,2400)){ 
    print(ss2)
    rf2 <- randomForest(y ~ ., data=train_undersampling2, cutoff = co, sampsize=ss2) 
    T2.predict <- predict(rf2, bankdata[test,], type = "class")
    T2.tab <- table(test_df$y, T2.predict)
    print(samps)
    print(T2.tab)
    print(MissCRate(T2.tab))
  }
}

# Yes combining multiple aproaches makes sense. It however takes time to find the best one.
# 
# Approximately best one:
#   SMOTE+undersampling+increasing sample size

### applying on the whole dataset.
newData2 <- SMOTE(y ~ ., bankdata)
print(table(newData2$y))
train_no3 <- newData2[newData2$y == "no", ]

no_undersampling3 <- train_no3[sample.int(nrow(train_no3),size=1563,replace=FALSE),]
full_undersampling2 <- rbind(newData2[newData2$y == "yes", ], no_undersampling3)
print(table(full_undersampling2$y))

rf2 <- randomForest(y ~ ., data=full_undersampling2, sampsize=2400) 
T2.predict <- predict(rf2, bankdata[test,], type = "class")
T2.tab <- table(test_df$y, T2.predict)
print(samps)
print(T2.tab)
print(MissCRate(T2.tab))


# no  yes 
# 2084 1563 
# 
# no  yes 
# 1563 1563 
# [1] 3617
# T2.predict
# no yes
# no  726  67
# yes   0 111
# [1] 0.07411504