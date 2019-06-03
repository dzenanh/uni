################################################################################# 
# Non-linear approximation
################################################################################# 

library(ElemStatLearn)
data(bone)
d <- bone[bone$gender=="male",c("age","spnbmd")]
n <- nrow(d)
set.seed(123)
train <- sample(1:n,round(2/3*n))
test <- c(1:n)[-train]

plot(d[train,])

# smoothing splines:
res.spline <- smooth.spline(d$age[train], d$spnbmd[train],cv=TRUE)
lines(res.spline, col="blue")

age.ord <- order(d[test,1])
dtest.age <- d[test,1][age.ord]
dtest.s <- d[test,2][age.ord]
spline.pred <- predict(res.spline,dtest.age)
lines(dtest.age,spline.pred$y,col=3)
mean((spline.pred$y-dtest.s)^2)
# 0.00113546

# LOESS:
plot(d[train,])
dtrainage.ord <- order(d[train,1])
dtrain.age <- d[train,1][dtrainage.ord]
dtrain.s <- d[train,2][dtrainage.ord]
dtrain <- data.frame(age=dtrain.age,spnbmd=dtrain.s)
res.loess <- loess(spnbmd~age,data=dtrain,span=0.75)
lines(dtrain.age,res.loess$fitted)
pred.loess <- predict(res.loess,dtest.age)
mean((pred.loess-dtest.s)^2,na.rm=TRUE)
# 0.001218825

# bs
library(splines)
x.bs <- bs(dtrain.age,df=5)
bs.train <- data.frame(spnbmd=dtrain.s,age=as.matrix(x.bs))
res.bs <- lm(spnbmd~.,data=bs.train)
test.bs <- bs(dtest.age,df=5)
bs.test <- data.frame(spnbmd=dtest.s,age=as.matrix(test.bs))
bs.pred <- predict(res.bs,newdata=bs.test)
plot(d[train,])
lines(dtrain.age,res.bs$fitted,col=3)
lines(dtest.age,bs.pred,col=4)
mean((bs.pred-bs.test$spnbmd)^2,na.rm=TRUE)
# 0.00119171

# ns:
x.ns <- ns(dtrain.age,df=5)
ns.train <- data.frame(spnbmd=dtrain.s,age=as.matrix(x.ns))
res.ns <- lm(spnbmd~.,data=ns.train)
test.ns <- ns(dtest.age,df=5)
ns.test <- data.frame(spnbmd=dtest.s,age=as.matrix(test.ns))
ns.pred <- predict(res.bs,newdata=ns.test)
plot(d[train,])
lines(dtrain.age,res.ns$fitted,col=3)
lines(dtest.age,ns.pred,col=4)
mean((ns.pred-ns.test$spnbmd)^2,na.rm=TRUE)
# 0.001585333



