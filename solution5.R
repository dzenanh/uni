################################################################################# 
# Logistic regression:
################################################################################# 

library(ISLR)
data(OJ)

res <- glm(Purchase~LoyalCH,data=OJ,family="binomial")

xtest <- seq(from=-1,to=2,length=1000)
ypred1 <- predict(res,type="response")
ypred2 <- predict(res,type="link")

yobs <- as.numeric(OJ$Purchase)-1
par(mfrow=c(2,2))
plot(OJ$LoyalCH,ypred2,col=yobs+2)
plot(OJ$LoyalCH,ypred1,col=yobs+2)

z <- res$coef[1]+xtest*res$coef[2]
plot(xtest,z,xlim=range(OJ$LoyalCH),type="l",ylim=c(-0.5,1.5))
points(OJ$LoyalCH,yobs,col=yobs+2)

plot(xtest,exp(z)/(1+exp(z)),xlim=range(OJ$LoyalCH),type="l")
points(OJ$LoyalCH,yobs,col=yobs+2)

################################################################################# 
# with all predictors:
OJnew <- OJ
OJnew$Store7 <- as.numeric(OJ$Store7)
OJnew$StoreID <- factor(OJ$StoreID,levels=c(1,2,3,4,7),labels=1:5)

n <- nrow(OJ)
set.seed(123)
train <- sample(1:n,round(2/3*n))
test <- (1:n)[-train]

res <- glm(Purchase~.,data=OJnew,family="binomial",subset=train)
summary(res)

res.step <- step(res)
summary(res.step)
names(res.step$coef)

anova(res.step,res,test="Chisq")

res.p <- predict(res,OJnew[test,])
(TAB <- table(OJnew[test,1],res.p>0))
(mcl1 <- 1-sum(diag(TAB))/sum(TAB))

res.p2 <- predict(res.step,OJnew[test,])
(TAB <- table(OJnew[-train,1],res.p2>0))
(mcl2 <- 1-sum(diag(TAB))/sum(TAB))


##############################################################
# bank data:
d <- read.csv2("bank.csv")
train_size <- 3000                     # has taken 3000 observations
          
set.seed(123)
i_train <- sample(dim(d)[1],train_size)
d_train <- d[i_train,]
d_test <- d[-i_train,]          
          
res <- glm(y~.,data=d_train,family="binomial")
summary(res)

res.p <- predict(res,d_test)
(TAB <- table(d_test$y,res.p>0))
(mcl <- 1-sum(diag(TAB))/sum(TAB))

(TAB <- table(d_test$y,res.p>-2))
(mcl <- 1-sum(diag(TAB))/sum(TAB))

