####

#  Dzenan Hamzic 
#    00327029
# 
# The R Notebook (Markdown) of this exercise can be found @ https://bit.ly/2V0WQhk
# 

####

#### Helper Functions

MissCRate = function(tab){
  1-sum(diag(tab))/sum(tab)
}


library(rpart)
# used for undersampling
# install.packages("ROSE")
library(ROSE)

bankdata <- read.csv2("bank.csv")
head(bankdata)

### class distribution

table(bankdata$y)
str(bankdata)


## 1
### 1.a)
set.seed(123)
n <- nrow(bankdata)
train <- sample(1:n,round(0.8*n))
test <- c(1:n)[-train]



T0 <- rpart(y~.,bankdata, subset = train, method = "class")
summary(T0)

# Variable importance
# duration poutcome    month      day    pdays      job  contact  marital      age previous 
# 41       28        9        7        5        3        3        2        1        1 

### 1.b)

plot(T0, uniform=TRUE, main="Bank Data - Customer deposit prediction")
text(T0, use.n=TRUE, all=TRUE, cex=.7)
#printcp(T0)
#post(T0, file="bank-T0")

# The tree splits "yes" to right and "no" to left. One attribute value(best one) is choosen for the split.
# The first choosen attribute has the largest "information gain". 
# The lower the attribute split in the tree, the lower is's significance for the classification. 

### 1.c)

T0.predict <- predict(T0, bankdata[-train,], type = "class")
T0.tab <- table(bankdata[-train, "y"], T0.predict)
T0.tab


# T0.predict
#     no yes
# no  766  27
# yes  71  40

#### Missclasification rate

MissCRate(T0.tab)

# [1] 0.1084071
#The "yes" missclasification rate is 40.3%

### 1.d)
printcp(T0)
plotcp(T0, upper = "size")

# The optimal complexity is 0.031 with tree size of 4.


### 1.e)
T0.1 <- prune(T0, cp=0.031)
plot(T0.1, uniform=TRUE, main="Bank Data - Customer deposit prediction")
text(T0.1, use.n=TRUE, all=TRUE, cex=.7)

# The prunned tree has only 2 levels (four leafs). 

### 1.f)

T0.1.predict <- predict(T0.1, bankdata[-train,], type = "class")
T0.1.tab <- table(bankdata[-train, "y"], T0.1.predict)
T0.1.tab


# T0.1.predict
#     no yes
# no  771  22
# yes  76  35

#### prunned tree missclasification rate

MissCRate(T0.1.tab)

# [1] 0.1084071
#"yes" missclasification rate is 38.6%

# The tree pruning has brought only slight improvement in "yes" class prediction (~2%) on the cost of "no" class
# which has now higher missclasification rate. 

### 1.g)
#### undersampling

bankdata_balanced_under <- ovun.sample(y ~ ., data = bankdata, method = "under", N = 1042, seed = 123)$data
table(bankdata_balanced_under$y)

# no yes 
# 521 521 



nr <- nrow(bankdata_balanced_under)
train_under <- sample(1:nr,round(0.8*nr))

T1 <- rpart(y~.,bankdata_balanced_under, subset = train_under, method = "class")
#summary(T1)
plot(T1, uniform=TRUE, main="Bank Data - Customer deposit prediction")
text(T1, use.n=TRUE, all=TRUE, cex=.7)

# The tree with "subsampling" strategy looks completely different from the original one.
# The spliting points and the attribute ordering is completely different.


T1.predict <- predict(T1, bankdata_balanced_under[-train_under,], type = "class")
T1.tab <- table(bankdata_balanced_under[-train_under, "y"], T1.predict)
T1.tab

# T1.predict
#     no yes
# no   74  23
# yes  10 101

# "yes" missclasification rate is ~18%. -> HUGE improvement


MissCRate(T1.tab)
# [1] 0.1586538


printcp(T1)
plotcp(T1, upper = "size")


### 1.h) Ensamble learning
#### build ensamble voting table


# make 100 predictions
for (i in 1:101){
  
  train_under2 <- sample(1:nr,round(0.8*nr))
  T2 <- rpart(y~. , bankdata_balanced_under, subset = train_under2, method = "class")
  p1 <- predict(T2, bankdata_balanced_under[-train_under2,], type = "class")
  
  if(i==1){
    ensamble_result_table <- data.frame(p1)
  }else{
    ensamble_result_table[, (paste("p", i, sep = ""))] <- as.vector(p1)
  }
}
head(ensamble_result_table)


#### prepare data
ensamble_df <- data.frame(t(ensamble_result_table))
colnames(ensamble_df) <- row.names(ensamble_result_table)
head(ensamble_df,10)


#### majority vote

ensamble.predict <- sapply(ensamble_df, function(x) tail(names(sort(table(x))),1))
head(ensamble.predict)


#### predict

test_indices <- names(ensamble.predict)
T2.tab <- table(bankdata_balanced_under[test_indices, "y"], ensamble.predict)
T2.tab


# ensamble.predict
#       no yes
# no   98   0
# yes   5 105


MissCRate(T2.tab)
#[1] 0.02403846
# The ensamble missclasification result is AMAZING. 
# Missclasification rates of both groups are almost perfect.

## 2. Run the following code.

paste(intToUtf8(acos(log(1))*180/pi-13),
      toupper(substr(month.name[2],2,2)),
      paste(rep(intToUtf8(acos(exp(0)/2)*180/pi+2^4+3*2),2), collapse = intToUtf8(0)),
      LETTERS[5^(3-1)], intToUtf8(atan(1/sqrt(3))*180/pi+2),
      toupper(substr(month.abb[10],2,2)),
      intToUtf8(acos(log(1))*180/pi-(2*3^2)),
      toupper(substr(month.name[4],3,4)),
      intToUtf8(acos(exp(0)/2)*180/pi+2^4+3*2+1),
      intToUtf8(acos(exp(0)/2)*180/pi+2^4+2*4),
      intToUtf8(acos(log(1))*180/pi-13),
      LETTERS[median(0:2)],
      intToUtf8(atan(1/sqrt(3))*180/pi*3-7),
      sep = intToUtf8(0)
)


#[1] "MERRY CHRISTMAS"

# MERRY CHRISTMAS TO YOU TOO. 
# P.S. happy NEW YEAR