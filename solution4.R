# Exercise 4:

# 1.

install.packages("ISLR")
library(ISLR)

data(OJ, package = "ISLR")
data <- OJ

# remove redundant columns that cause all kinds of collinearity problems
data$Store7 <- NULL
data$PriceDiff <- NULL
data$ListPriceDiff <- NULL
data$STORE <- NULL
data$SalePriceMM <- NULL
data$SalePriceCH <- NULL

set.seed(1)
n <- dim(data)[1]
is.train <- sample(1:n, ceiling(2./3. * n))
data.train <- data[is.train,]
data.test <- data[-is.train,]

mis <- function(y, y.hat) {
  sum(y != y.hat) / length(y)
}

# (a)

model.ls <- lm((as.numeric(Purchase) - 1) ~ ., data = data.train)
mis(as.numeric(data.test$Purchase) - 1, predict(model.ls, newdata = data.test) > .5)

# (b)

install.packages("MASS")
library(MASS)

model.lda <- lda(Purchase ~ ., data = data.train)
mis(data.test$Purchase, predict(model.lda, newdata = data.test)$class)

# There are not really problems with factors, more with collinearities, hower those were already 
# resolved during preprocessing.

# (c)

model.qda <- qda(Purchase ~ ., data = data.train)
mis(data.test$Purchase, predict(model.qda, newdata = data.test)$class)

# Again, the collinearity problems were already resolved during preprocessing.

# (d)

install.packages("klaR")
library(klaR)

model.rda <- rda(Purchase ~ ., data = data.train)
model.rda
mis(data.test$Purchase, predict(model.rda, newdata = data.test)$class)

# Gamma and lambda give the weighting between the quadratic (per class) and the linear (common
# for all classes) covariance matrices that are used throughout the algorithm. In this case the 
# model is pulled slightly more towards the linear version as lambda is larger than gamma.

# 2.

data <- read.csv2("bank.csv")

# (a)

set.seed(1)
n <- dim(data)[1]
is.train <- sample(1:n, 3000)
data.train <- data[is.train,]
data.test <- data[-is.train,]

model.lda <- lda(y ~ ., data = data.train)
y.hat.lda <- predict(model.lda, newdata = data.test)$class
table(data.test$y, y.hat.lda)
mis(data.test$y, y.hat.lda)

# (b)

# We use the suggested under-sampling, i.e. we use all samples from group "yes"
# and then add exactly as many random samples from group "no"

set.seed(1)
data.train1 <- data.train[data.train$y == "yes",]
tmp <- data.train[data.train$y == "no",]
data.train1 <- rbind(data.train1, tmp[sample(nrow(tmp), dim(data.train1[1])),])

model.lda1 <- lda(y ~ ., data = data.train1)
y.hat.lda1 <- predict(model.lda1, newdata = data.test)$class
table(data.test$y, y.hat.lda1)
mis(data.test$y, y.hat.lda1)

# This new model performs significantly better on "yes" instances but certainly
# worse on "no" ones, however as those are significantly less critical, this
# is probably a better model despite higher misclassification-rate.
