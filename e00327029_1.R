####

#  Dzenan Hamzic 
#    00327029
# 
# The R Notebook (Markdown) of this exercise can be found @ https://bit.ly/2EtRirr
# or @ http://62.75.171.30/ds/exer1.nb.html

####

install.packages("ISLR")

### Load Data
# load data
data(Hitters, package = "ISLR")
# count rows
nrow(Hitters)

### Check dataframe details
str(Hitters)
summary(Hitters)


### How many empty rows?
# how many empty rows
nrow(Hitters) - NROW(na.omit(Hitters))


### Clean empty rows and show number of raws again
# clean empty rows
Hitters = na.omit(Hitters)
nrow(Hitters)


### Split data randomly on 50% splits (train and test)
# seed was unfortunately not executed before the splits. (I forgot it :( )).
# I have pasted pieces of output for the result "tracebility"
#set.seed(0607)
ind <- sample(c(TRUE, FALSE), nrow(Hitters), replace=TRUE, prob=c(0.5, 0.5))
train_data <- Hitters[ind, ]
test_data <- Hitters[!ind, ]
train_data
test_data

### QUESTION Any data preprocessing needed/meaningfull? ###
### ANSWER ###
# Categorical variables should be converted to numerical. I shall do that in the following step,
# although it meight be not necessary because R does that 'behind the sceenes in the lm function'. 

# Removing outliers is not wanted because the outliers in this dataset are important and it is not the case that the single values are faulty.
# I think that the outlier removal would possibly further reduce MSE (on average) since most of the players have averge salaries,
# but would make large errors with exceptional players.
# On the other hand, this outliers could improve the model by finding more relevant predictor variables which are crutial for salary amount.

# Scaling (so that predictor variables are onthe same value scales) might be usefull, but for this task, I think,
# is not necessary since the whole point of the taks is to choose best possible model with variety of different techniques,
# and to compare those "optimal" models against each other on prediction performance with MSE.





### Convert categorical to numerical (Division, NewLeague)
# convert categorical variables to numerical
train_data_mat <- data.matrix(train_data)
test_data_mat <- data.matrix(test_data)

# convert to data frame
train_data_processed <- as.data.frame(train_data_mat)
test_data_processed <- as.data.frame(test_data_mat)

# check them out
train_data_processed
test_data_processed

#### 1. Full model
### Test out the simple full model on Prediction performance
# build full model
lmFull<- lm(Salary ~ ., data = train_data_processed)
summary(lmFull)


# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  283.8360   217.3380   1.306 0.194240    
# AtBat         -1.9853     0.9818  -2.022 0.045546 *  
#   Hits           7.4358     3.7028   2.008 0.047031 *  
#   HmRun          0.8961     8.9512   0.100 0.920440    
# Runs          -5.4990     4.0924  -1.344 0.181761    
# RBI            0.7132     3.9227   0.182 0.856065    
# Walks          8.4469     3.0272   2.790 0.006190 ** 
#   Years        -25.3293    19.3883  -1.306 0.194085    
# CAtBat        -0.1002     0.1906  -0.526 0.600071    
# CHits          0.4792     0.9797   0.489 0.625683    
# CHmRun         3.2529     2.4420   1.332 0.185536    
# CRuns          1.0676     1.0611   1.006 0.316535    
# CRBI          -0.5685     1.0390  -0.547 0.585365    
# CWalks        -0.6292     0.5043  -1.248 0.214776    
# League       -75.6408   116.8092  -0.648 0.518595    
# Division    -100.3051    60.6764  -1.653 0.101108    
# PutOuts        0.4946     0.1340   3.692 0.000345 ***
#   Assists        0.7291     0.3385   2.154 0.033369 *  
#   Errors       -11.3968     7.3166  -1.558 0.122135    
# NewLeague    134.4394   111.2085   1.209 0.229249    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 322.2 on 112 degrees of freedom
# Multiple R-squared:  0.5936,	Adjusted R-squared:  0.5247 
# F-statistic: 8.611 on 19 and 112 DF,  p-value: 2.003e-14
# 
# 
# ##### INTEPRETATION #####
# The coefficients Assists, PutOuts, Walks, Hits, AtBat have very large t-values and very small p-values.
# Therefore, the null hypothesis beta-i=0 should be rejected.
# 
# p-value of the F-statistic is also small (2.003*10^-14) and the null hypothesis beta-i=0 for all i (i=1,...,p) should be rejected as well.
# 
# R-squared is 0.5936 is a reasonable to good fit. The regression line describes data reasonably.


## Calculate the F-statistics
qf(0.95, 19, 112)


# [1] 1.680165
# The value of F-statistic of 8.611 is larger than the F quantile F 19,112,0.95. Therefore the null hypothesis beta-i, (for all i (i=1,...,p)) can be rejected. 


### Test prediction power of the full model
# predict salary on test data
pred_full <- predict.lm(lmFull, test_data_processed)

### Evaluate the prediction power of the full model visually
#plot(pred_full,type="p",col="red")
#points(test_data_processed$Salary,col="green")


### Calculate the MSE of the full model
mean((test_data_processed$Salary - pred_full) ^ 2)
# The MSE of the full model is:
# [1] 132133.5


#### 2. Stepwise regression
### Now choose optimal model using AIC algorithm - stepwise method

## backward
summary(lmStepwise <- lm(Salary ~ ., data = train_data_processed))
slm1 <- step(lmStepwise, direction = "backward")
summary(slm1)

# Step:  AIC=1532.24
# Salary ~ AtBat + Hits + Runs + Walks + Years + CHmRun + CRuns + 
#   CWalks + Division + PutOuts + Assists + Errors
# 
# Df Sum of Sq      RSS    AIC
# <none>                  11920108 1532.2
# - Errors    1    214923 12135031 1532.6
# - Runs      1    361191 12281299 1534.2
# - CWalks    1    366676 12286784 1534.2
# - Division  1    367378 12287486 1534.2
# - Assists   1    383141 12303249 1534.4
# - Years     1    461682 12381790 1535.3
# - AtBat     1    541699 12461807 1536.1
# - Hits      1    747912 12668020 1538.3
# - CHmRun    1    798056 12718164 1538.8
# - CRuns     1   1115166 13035274 1542.0
# - Walks     1   1304974 13225082 1544.0
# - PutOuts   1   1450732 13370840 1545.4
# 
# Call:
#   lm(formula = Salary ~ AtBat + Hits + Runs + Walks + Years + CHmRun + 
#        CRuns + CWalks + Division + PutOuts + Assists + Errors, data = train_data_processed)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -781.7 -170.6  -39.7  110.5 1827.3 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  386.2081   147.6995   2.615 0.010082 *  
#   AtBat         -2.0872     0.8975  -2.325 0.021740 *  
#   Hits           8.0916     2.9612   2.732 0.007244 ** 
#   Runs          -5.7329     3.0191  -1.899 0.059999 .  
#   Walks          9.3442     2.5889   3.609 0.000450 ***
#   Years        -31.9731    14.8929  -2.147 0.033833 *  
#   CHmRun         2.1493     0.7615   2.823 0.005585 ** 
#   CRuns          1.2030     0.3605   3.337 0.001132 ** 
#   CWalks        -0.7808     0.4081  -1.913 0.058117 .  
#   Division    -107.9515    56.3688  -1.915 0.057881 .  
#   PutOuts        0.4818     0.1266   3.806 0.000225 ***
#   Assists        0.6059     0.3098   1.956 0.052839 .  
#   Errors        -9.9791     6.8126  -1.465 0.145616    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 316.5 on 119 degrees of freedom
# Multiple R-squared:  0.5835,	Adjusted R-squared:  0.5415 
# F-statistic: 13.89 on 12 and 119 DF,  p-value: < 2.2e-16


### Test Prediction Accurary with MSE -> step backward
lmTrain_backward <- lm( Salary ~ AtBat + Hits + Runs + Walks + Years + CHmRun + 
                          CRuns + CWalks + Division + PutOuts + Assists + Errors, data = train_data_processed)

prediction_backward <- predict.lm(lmTrain_backward, test_data_processed)

### Calculate the MSE of the optimal model from step backward
mean((test_data_processed$Salary - prediction_backward) ^ 2)

### MSE step backward
# [1] 124942




### Now choose optimal model using AIC algorithm - stepwise method
## forward
min.model <- lm(Salary~1, data = train_data_processed)
biggest <- formula(lm(Salary ~ ., data = train_data_processed))

slm2 <- step(min.model, direction = "forward", scope = biggest)
summary(slm2)

# best model
# .
# .
# Step:  AIC=1536.04
# Salary ~ CHmRun + Walks + PutOuts + CRuns + CWalks + Division + 
#   Years
# 
# Df Sum of Sq      RSS    AIC
# <none>                   13233225 1536.0
# + NewLeague  1    197549 13035676 1536.0
# + Runs       1    189036 13044189 1536.1
# + Assists    1    124867 13108358 1536.8
# + CHits      1    123130 13110095 1536.8
# + HmRun      1     65852 13167373 1537.4
# + League     1     56048 13177177 1537.5
# + AtBat      1     43547 13189678 1537.6
# + CRBI       1     40084 13193141 1537.6
# + CAtBat     1     18027 13215198 1537.9
# + RBI        1      8155 13225070 1538.0
# + Errors     1      3431 13229794 1538.0
# + Hits       1      1216 13232009 1538.0
# 
# Call:
#   lm(formula = Salary ~ CHmRun + Walks + PutOuts + CRuns + CWalks + 
#        Division + Years, data = train_data_processed)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -701.92 -165.17  -22.98  151.29 2013.70 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  240.5774   131.8316   1.825 0.070426 .  
#   CHmRun         1.6889     0.7351   2.298 0.023262 *  
#   Walks          5.2080     1.8790   2.772 0.006438 ** 
#   PutOuts        0.4170     0.1253   3.327 0.001157 ** 
#   CRuns          1.0632     0.2912   3.651 0.000384 ***
#   CWalks        -0.5640     0.3541  -1.593 0.113787    
#   Division    -120.6423    57.4120  -2.101 0.037635 *  
#   Years        -23.1628    14.2472  -1.626 0.106536    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 326.7 on 124 degrees of freedom
# Multiple R-squared:  0.5376,	Adjusted R-squared:  0.5115 
# F-statistic:  20.6 on 7 and 124 DF,  p-value: < 2.2e-16

### Test Prediction Accurary with MSE -> step forward
lmTrain_forward <- lm(formula = Salary ~ CHmRun + Walks + PutOuts + CRuns + CWalks + 
                        Division + Years, data = train_data_processed)

prediction_forward <- predict.lm(lmTrain_forward, test_data_processed)
mean((test_data_processed$Salary - prediction_forward) ^ 2)

### MSE step forward
# [1] 119050.9


### Model Selection with step "both"

min.model <- lm(Salary~1, data = train_data_processed)
biggest <- lm(Salary ~ ., data = train_data_processed)

slm3 <- step(min.model, direction = "both", scope = formula(biggest))
summary(slm3)

# best model
# Step:  AIC=1536.04
# Salary ~ CHmRun + Walks + PutOuts + CRuns + CWalks + Division + 
#   Years
# 
# Df Sum of Sq      RSS    AIC
# <none>                   13233225 1536.0
# + NewLeague  1    197549 13035676 1536.0
# + Runs       1    189036 13044189 1536.1
# - CWalks     1    270695 13503920 1536.7
# + Assists    1    124867 13108358 1536.8
# + CHits      1    123130 13110095 1536.8
# - Years      1    282075 13515300 1536.8
# + HmRun      1     65852 13167373 1537.4
# + League     1     56048 13177177 1537.5
# + AtBat      1     43547 13189678 1537.6
# + CRBI       1     40084 13193141 1537.6
# + CAtBat     1     18027 13215198 1537.9
# + RBI        1      8155 13225070 1538.0
# + Errors     1      3431 13229794 1538.0
# + Hits       1      1216 13232009 1538.0
# - Division   1    471235 13704460 1538.7
# - CHmRun     1    563353 13796578 1539.5
# - Walks      1    819793 14053018 1542.0
# - PutOuts    1   1181090 14414315 1545.3
# - CRuns      1   1422197 14655422 1547.5
# 
# Call:
#   lm(formula = Salary ~ CHmRun + Walks + PutOuts + CRuns + CWalks + 
#        Division + Years, data = train_data_processed)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -701.92 -165.17  -22.98  151.29 2013.70 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  240.5774   131.8316   1.825 0.070426 .  
#   CHmRun         1.6889     0.7351   2.298 0.023262 *  
#   Walks          5.2080     1.8790   2.772 0.006438 ** 
#   PutOuts        0.4170     0.1253   3.327 0.001157 ** 
#   CRuns          1.0632     0.2912   3.651 0.000384 ***
#   CWalks        -0.5640     0.3541  -1.593 0.113787    
#   Division    -120.6423    57.4120  -2.101 0.037635 *  
#   Years        -23.1628    14.2472  -1.626 0.106536    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 326.7 on 124 degrees of freedom
# Multiple R-squared:  0.5376,	Adjusted R-squared:  0.5115 
# F-statistic:  20.6 on 7 and 124 DF,  p-value: < 2.2e-16


### Test Prediction Accurary with MSE -> step "both"
lmTrain_both <- lm(formula = Salary ~ CHmRun + Walks + PutOuts + CRuns + CWalks + 
                     Division + Years, data = train_data_processed)

prediction_both <- predict.lm(lmTrain_both, test_data_processed)
mean((test_data_processed$Salary - prediction_both) ^ 2)

### MSE step both
# [1] 119050.9



## Model comparison with ANOVA

### MODELS
## empty model
#lm.empty
## full model
# lmFull
## step both
#lmTrain_both
## step forward
#lmTrain_forward
## step backward
#lmTrain_backward

# initiate empty model just for comparison
lm.empty <- lm(Salary ~ 1, data = train_data_processed)

anova(lm.empty, lmTrain_both, lmTrain_forward, lmTrain_backward, lmFull)

# 
# Analysis of Variance Table
# 

# 
# Model 1: Salary ~ 1
# Model 2: Salary ~ CHmRun + Walks + PutOuts + CRuns + CWalks + Division + 
#   Years
# Model 3: Salary ~ CHmRun + Walks + PutOuts + CRuns + CWalks + Division + 
#   Years
# Model 4: Salary ~ AtBat + Hits + Runs + Walks + Years + CHmRun + CRuns + 
#   CWalks + Division + PutOuts + Assists + Errors
# Model 5: Salary ~ AtBat + Hits + HmRun + Runs + RBI + Walks + Years + 
#   CAtBat + CHits + CHmRun + CRuns + CRBI + CWalks + League + 
#   Division + PutOuts + Assists + Errors + NewLeague
# Res.Df      RSS Df Sum of Sq       F  Pr(>F)    
# 1    131 28618714                                    (0 predictors) empty model
# 2    124 13233225  7  15385489 21.1665 < 2e-16 ***   (7 predictors) step->both
# 3    124 13233225  0         0                       (7 predictors) step->forward
# 4    119 11920108  5   1313117  2.5291 0.03293 *     (12 predictors) step->backward
# 5    112 11630061  7    290047  0.3990 0.90114       (19 predictors) full model
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

### TODO ANOVA Analysis
# An F-test is computed for every model with 
# additional predictor variables in order to evaluate significance of the additional parameters.
# Starting out from an empty model (with no predictors except the intercept), 
# we can measure significance of every other model with additional predictor variables.
# 
# Model 2 (compared to empty model 1) is highly significant (low p and high f values) comparing to empty model.
# Model 3 is equal to the model 2.
# Model 4 is also significant but much less then model 2.


#### 3. Best subset regression:

## a)
library(leaps)
lm.regsubset<-regsubsets(train_data_processed$Salary ~ ., data=train_data_processed, nbest = 3, nvmax = 8)
summary(lm.regsubset)

# Subset selection object
# Call: regsubsets.formula(train_data_processed$Salary ~ ., data = train_data_processed, 
#                          nbest = 3, nvmax = 8)
# 19 Variables  (and intercept)
# Forced in Forced out
# AtBat         FALSE      FALSE
# Hits          FALSE      FALSE
# HmRun         FALSE      FALSE
# Runs          FALSE      FALSE
# RBI           FALSE      FALSE
# Walks         FALSE      FALSE
# Years         FALSE      FALSE
# CAtBat        FALSE      FALSE
# CHits         FALSE      FALSE
# CHmRun        FALSE      FALSE
# CRuns         FALSE      FALSE
# CRBI          FALSE      FALSE
# CWalks        FALSE      FALSE
# League        FALSE      FALSE
# Division      FALSE      FALSE
# PutOuts       FALSE      FALSE
# Assists       FALSE      FALSE
# Errors        FALSE      FALSE
# NewLeague     FALSE      FALSE
# 3 subsets of each size up to 8
# Selection Algorithm: exhaustive
# AtBat Hits HmRun Runs RBI Walks Years CAtBat CHits CHmRun CRuns CRBI CWalks League Division
# 1  ( 1 ) " "   " "  " "   " "  " " " "   " "   " "    " "   "*"    " "   " "  " "    " "    " "     
# 1  ( 2 ) " "   " "  " "   " "  " " " "   " "   " "    " "   " "    "*"   " "  " "    " "    " "     
# 1  ( 3 ) " "   " "  " "   " "  " " " "   " "   " "    " "   " "    " "   "*"  " "    " "    " "     
# 2  ( 1 ) " "   " "  " "   " "  " " "*"   " "   " "    " "   " "    " "   "*"  " "    " "    " "     
# 2  ( 2 ) " "   " "  " "   " "  " " "*"   " "   " "    " "   "*"    " "   " "  " "    " "    " "     
# 2  ( 3 ) " "   " "  " "   " "  " " "*"   " "   " "    " "   " "    "*"   " "  " "    " "    " "     
# 3  ( 1 ) " "   " "  " "   " "  " " "*"   " "   " "    " "   " "    "*"   " "  " "    " "    " "     
# 3  ( 2 ) " "   " "  " "   " "  " " "*"   " "   " "    " "   " "    " "   "*"  " "    " "    " "     
# 3  ( 3 ) " "   " "  " "   " "  " " " "   "*"   " "    " "   " "    "*"   " "  " "    " "    " "     
# 4  ( 1 ) " "   " "  " "   " "  " " "*"   "*"   " "    " "   " "    "*"   " "  " "    " "    " "     
# 4  ( 2 ) " "   " "  " "   " "  " " "*"   " "   " "    " "   " "    " "   "*"  " "    " "    "*"     
# 4  ( 3 ) " "   " "  " "   " "  " " "*"   " "   " "    " "   "*"    "*"   " "  " "    " "    " "     
# 5  ( 1 ) " "   " "  " "   " "  " " "*"   " "   "*"    "*"   "*"    " "   " "  " "    " "    " "     
# 5  ( 2 ) " "   " "  " "   " "  " " "*"   "*"   " "    "*"   "*"    " "   " "  " "    " "    " "     
# 5  ( 3 ) " "   " "  " "   " "  " " "*"   "*"   " "    " "   " "    "*"   " "  " "    " "    "*"     
# 6  ( 1 ) " "   " "  " "   " "  " " "*"   "*"   " "    "*"   "*"    " "   " "  " "    " "    "*"     
# 6  ( 2 ) " "   " "  " "   " "  " " "*"   " "   "*"    "*"   "*"    " "   " "  " "    " "    "*"     
# 6  ( 3 ) " "   " "  " "   " "  " " "*"   "*"   " "    " "   "*"    "*"   " "  " "    " "    "*"     
# 7  ( 1 ) " "   " "  " "   " "  " " "*"   "*"   "*"    "*"   "*"    " "   " "  " "    " "    "*"     
# 7  ( 2 ) " "   " "  " "   " "  " " "*"   " "   "*"    "*"   "*"    " "   " "  " "    " "    "*"     
# 7  ( 3 ) " "   " "  " "   " "  " " "*"   "*"   " "    "*"   "*"    " "   " "  "*"    " "    "*"     
# 8  ( 1 ) " "   " "  " "   " "  " " "*"   " "   "*"    "*"   "*"    " "   " "  " "    " "    "*"     
# 8  ( 2 ) "*"   "*"  " "   " "  " " "*"   "*"   " "    "*"   "*"    " "   " "  " "    " "    "*"     
# 8  ( 3 ) " "   " "  " "   " "  " " "*"   "*"   "*"    "*"   "*"    " "   " "  " "    " "    "*"     
# PutOuts Assists Errors NewLeague
# 1  ( 1 ) " "     " "     " "    " "      
# 1  ( 2 ) " "     " "     " "    " "      
# 1  ( 3 ) " "     " "     " "    " "      
# 2  ( 1 ) " "     " "     " "    " "      
# 2  ( 2 ) " "     " "     " "    " "      
# 2  ( 3 ) " "     " "     " "    " "      
# 3  ( 1 ) "*"     " "     " "    " "      
# 3  ( 2 ) "*"     " "     " "    " "      
# 3  ( 3 ) "*"     " "     " "    " "      
# 4  ( 1 ) "*"     " "     " "    " "      
# 4  ( 2 ) "*"     " "     " "    " "      
# 4  ( 3 ) "*"     " "     " "    " "      
# 5  ( 1 ) "*"     " "     " "    " "      
# 5  ( 2 ) "*"     " "     " "    " "      
# 5  ( 3 ) "*"     " "     " "    " "      
# 6  ( 1 ) "*"     " "     " "    " "      
# 6  ( 2 ) "*"     " "     " "    " "      
# 6  ( 3 ) "*"     " "     " "    " "      
# 7  ( 1 ) "*"     " "     " "    " "      
# 7  ( 2 ) "*"     "*"     " "    " "      
# 7  ( 3 ) "*"     " "     " "    " "      
# 8  ( 1 ) "*"     "*"     "*"    " "      
# 8  ( 2 ) "*"     " "     " "    " "      
# 8  ( 3 ) "*"     "*"     " "    " "      


##  3 b) plot results
plot(lm.regsubset)

#### INTEPRETATION
# Rading from the plot, the best model (with minimal bic of -67) has eather 4 or 6 regressors + intercept
####

## best model with 4 variables (bic -67)
lmTrain_best_subset <- lm(formula = Salary ~ Walks + Years + Runs + PutOuts, data = train_data_processed)

prediction_best_subset <- predict.lm(lmTrain_best_subset, test_data_processed)
mean((test_data_processed$Salary - prediction_best_subset) ^ 2)
### MSE best subset (bic -67) 4 variables
#[1] 124064

## best model with 6 variables (bic -67)
lmTrain_best_subset2 <- lm(formula = Salary ~ Walks + Years + Hits + HmRun + Division + PutOuts, data = train_data_processed)

prediction_best_subset2 <- predict.lm(lmTrain_best_subset2, test_data_processed)
mean((test_data_processed$Salary - prediction_best_subset2) ^ 2)

### MSE best subset (bic -67) 6 variables
#[1] 117786.2


## 3. c)
summary_object <- summary(lm.regsubset)

str(summary_object$bic)
plot(summary_object$bic)

#### INTEPRETATION
#the plot in c also confirms that the best model has 6 regressors

# results of the best model
summary(lmTrain_best_subset2)

# Call:
#   lm(formula = Salary ~ Walks + Years + Hits + HmRun + Division + 
#        PutOuts, data = train_data_processed)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -648.99 -204.26  -42.01  114.94 2209.31 
# 
# Coefficients:
#                Estimate Std. Error t value   Pr(>|t|)    
#   (Intercept)  -65.7237   143.6717  -0.457  0.64814    
#   Walks          5.5280     1.9904   2.777  0.00633 ** 
#   Years         31.7440     7.2519   4.377 2.51e-05 ***
#   Hits           1.6444     0.9645   1.705  0.09068 .  
#   HmRun          5.9331     4.6142   1.286  0.20087    
#   Division    -106.4880    63.5362  -1.676  0.09623 .  
#   PutOuts        0.3218     0.1368   2.352  0.02022 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 360.2 on 125 degrees of freedom
# Multiple R-squared:  0.4332,	Adjusted R-squared:  0.406 
# F-statistic: 15.92 on 6 and 125 DF,  p-value: 1.562e-13


#### Best subset regression model Intepretation ####
# The predictors Years, Walks, PutOuts are highly significant indicated by low p values and high t values.
# The variance, described by R-squared is much lower then in the full model.
# The F-statistic is also significant indicated by p-value. 
#
# In order to possibly find even better predictor model in term of MSE errors,
# we can try to even further optimize this model by removing unsignificant predictors.
# The code below measures MSE on test data with the same model without unsignificant predictor HmRun.

lm.bm2 = lm(formula = Salary ~ Walks + Years + Hits + Division + PutOuts, data = train_data_processed)
summary(lm.bm2)
prediction_best_subset_model2 <- predict.lm(lm.bm2, test_data_processed)
## MSE
mean((test_data_processed$Salary - prediction_best_subset_model2) ^ 2)
## [1] 116160.1


lm.bm3 = lm(formula = Salary ~ Walks + Years + Hits + PutOuts, data = train_data_processed)
summary(lm.bm3)
prediction_best_subset_model3 <- predict.lm(lm.bm3, test_data_processed)
mean((test_data_processed$Salary - prediction_best_subset_model3) ^ 2)


#### MSE for test data comparison ###

### MSE full (19 predictors)
# [1] 132133.5
### MSE step backward (12 predictors)
# [1] 124942
### MSE step forward (7 predictors)
# [1] 119050.9
### MSE step both (7 predictors) - same model as step forward
# [1] 119050.9
### MSE best subset (bic -67) (4 predictors)
#[1] 124064
### MSE best subset (bic -67) (6 predictors)
#[1] 117786.2
### MSE best subset (5 predictors) + further predictor reduction using unsignificant p values from summary.
## [1] 116160.1

## Intepretation ##
# Second best prediction power has the model with 6 predictors and minimal found bic of -67 found with best subset regression.
# This model has the MSE of 117786.2. 

# By further optimization, even better model is found by removing unsignificant predictors from the model found in best subset regression.
# 116160.1



## QUESTION ##
#How would you do the final model selection for repeated splits into training and test data,
#where you might end up with different "optimal" models for different training data sets? 

## ANSWER ##
# One possibility would be to measure prediction MSE on the whole dataset of all "optimal models".
# The model with lowest MSE, on the whole dataset, has obviously best prediction performance.

# Other possibility would be to use prediction averages of all "optimal" models.
# This way, the bias would be reduced. 
