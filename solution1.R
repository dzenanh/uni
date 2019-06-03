# Exercise 1:

install.packages("ISLR")
install.packages("leaps")

# Make sure results are reproducible
set.seed(1)

data(Hitters, package="ISLR")
?Hitters
str(Hitters)

# Remove observations with missing values
data <- na.omit(Hitters)

# Is any preprocessing necessary or useful?

# Plot the histograms of all columns
for (c in names(data))
  hist(data[,c], main = c)

# The histograms show some outliers, but as they do not seem extremely too dramatic as being implausible, we do not
# remove them. Many distributions look very skewed, while our alogrithms are technically able to handle this without
# any problems, more symmetric distributions often tend to lead to better results. Therefore we sqrt-transform those
# variables.

for (c in c("HmRun", "Years", "CAtBat", "CHits", "CHmRun", "CRuns", "CRBI", "CWalks")) {
  data[,c] <- sqrt(data[,c])
  hist(data[,c], main = c)
  names(data)[names(data) == c] <- paste("sqrt", c, sep = "_")
}

# Plot a heatmap of absolute correlations for all numeric variables
heatmap(abs(cor(data[-c(14, 15, 20)])), symm = T) # 14, 15, 20 are the factors

# We see a lot of highly correlated (nearly white) variables. However, as variable selection will be done during
# later parts of the exercise we keep all of them for now.

n <- dim(data)[1]
is.train <- sample(1:n, ceiling(.5 * n))
data.train <- data[is.train,]
data.test <- data[-is.train,]

# 1.

model.full <- lm(Salary ~ ., data = data.train)
summary(model.full)

# The fit of the training data is mediocre at best with an adjusted R-squared of just 0.45. Also very few 
# predictors yield significant p-values and can hence be considered a likely not equal to 0.

# 2.

model.forward <- step(model.full, direction = "forward")
model.backward <- step(model.full, direction = "backward")
model.both <- step(model.full, direction = "both")

anova(model.forward, model.backward, model.both)

# The ANOVA test does not at all hint at any significant difference between the 3 models as the p-value is very high.

# 3.

library(leaps)

# (a)

models.regsubsets <- regsubsets(Salary ~ ., data = data.train, nbest = 3, nvmax = 8)

# (b)

plot(models.regsubsets)

# Which model seems to be the best?
# In theory, the best model should be the one with the lowest BIC, in this case (Intercept) + Walks + sqrt_CRuns 
# + DivisionW + PutOuts.

# (c)

models.regsubsets.summary <- summary(models.regsubsets)
str(models.regsubsets.summary)

plot(rownames(models.regsubsets.summary$which), models.regsubsets.summary$bic)

models.regsubsets.summary$which[which.min(models.regsubsets.summary$bic),]

# Which is the best model?
# The model best model is (as already identified before) Intercept + Walks + sqrt_CRuns + DivisionW + PutOuts.

# DivisionW is automatically created
model.best <- lm(Salary ~ Walks + sqrt_CRuns + Division + PutOuts, data = data.train)
summary(model.best)

# While the adjusted R-square is slightly worse (by about 3%) than the full model, all four of the selected 
# variables appear very significant with respect to the t-test.

# 4.

mse <- function(y, y.hat) {
  mean((y - y.hat)^2)
}

mse(data.test$Salary, predict(model.full, newdata = data.test))
mse(data.test$Salary, predict(model.forward, newdata = data.test))
mse(data.test$Salary, predict(model.backward, newdata = data.test))
mse(data.test$Salary, predict(model.both, newdata = data.test))
mse(data.test$Salary, predict(model.best, newdata = data.test))

summary(model.forward)

# Which model shows the best fit to the data?
# Surprisingly, the full model (or equivalently, the one obtained by forward stepwise selection) seems to perform
# the best on the test-set.

# How would you do the final model selection for repeated splits into training and test data, where you might end up
# with different “optimal” models for different training data sets?

# On possible approach could be to choose the model which is selected as the best one the most often. Another idea 
# would be to average the resulting MSE scores, or to be more robust take for instance the mean, and then select the 
# best overall model based on this score.
