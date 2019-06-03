####

#  Dzenan Hamzic 
#    00327029
# 
# The R Notebook (Markdown) of this exercise can be found @ https://bit.ly/2TR9PAB
# 

####

#install.packages("e1071", dependencies = TRUE)
library(e1071)


#### Helper Functions

MissCRate = function(tab){
  1-sum(diag(tab))/sum(tab)
}



bankdata <- read.csv2("bank.csv")
head(bankdata)


table(bankdata$y)

# no  yes 
# 4000  521 


set.seed(123)
n <- nrow(bankdata)
train <- sample(1:n,round(0.8*n))
test <- c(1:n)[-train]

train_df <- bankdata[train,]
test_df <- bankdata[test,]
table(train_df$y)


## 1.a

svm.model <- svm(y ~ ., data=train_df)
svm.pred <- predict(svm.model, test_df)
tab <- table(test_df$y, svm.pred)
print(tab)
MissCRate(tab)

# svm.pred
# no yes
# no  780  13
# yes 100  11
# [1] 0.125

## 2.b
tuned_parameters <- tune.svm(y ~ ., data=train_df, gamma = 10^(-5:-1), cost = 10^(-5:1))
summary(tuned_parameters )

# Optimal parameters are: gamma=0.100, cost=1e+00 with error of 0.09813823
plot(tuned_parameters)

# Darker area implicate better parameters.


## 2.c
svm.model2 <- svm(y ~ ., data=train_df, gamma=0.100, cost=1e+00)
svm.pred2 <- predict(svm.model2, test_df)
tab2 <- table(test_df$y, svm.pred2)
print(tab2)
MissCRate(tab2)

# svm.pred2
# no yes
# no  780  13
# yes  90  21
# [1] 0.1139381

# With "best" parameters, the error has improved. "yes" client correct classification has doubled. 



## 2.d - tuning "class.weights" parameter
costs <- table(bankdata$y)  # the weight vector must be named with the classes names
costs[1] <- 1e1 # a class -1 mismatch has a terrible cost
costs[2] <- 1e20    # a class +1 mismatch not so much...
costs

# no   yes 
# 1e+01 1e+20 

### tune with new weights
tuned_parameters2 <- tune.svm(y ~ ., data=train_df, class.weights=costs, gamma = 10^(-5:2), cost = 10^(-5:2))
summary(tuned_parameters2)

## Best parameters gamma=1e-01, cost=1e+01

svm.model <- svm(y ~ ., data=train_df, class.weights=costs, gamma=1e-01, cost=1e+01)
svm.pred <- predict(svm.model, test_df)
tab <- table(test_df$y, svm.pred)
print(tab)
MissCRate(tab)


# svm.pred
# no yes
# no  744  49
# yes  53  58
# [1] 0.1128319

# With "best" parameters, the error has improved.
# "yes" client correct classification has more then doubled. 


## Apply the strategy also on the whole data set bank-full.csv.
svm.model.full <- svm(y ~ ., data=bankdata, class.weights=costs, gamma=1e-01, cost=1e+01)
svm.pred.full <- predict(svm.model.full, test_df)
tab.full <- table(test_df$y, svm.pred.full)
print(tab.full)
MissCRate(tab.full)

# svm.pred.full
# no yes
# no  793   0
# yes   0 111
# [1] 0