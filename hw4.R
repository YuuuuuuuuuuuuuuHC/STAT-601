#1

#(a)

my_data <- read.table('F:/STAT 601/HW4/softdrink.txt',head=TRUE)

pairs(my_data)
cor(my_data)

#(b)
my_model <- lm(y~x1+x2, data = my_data)
summary(my_model)

#(d)
standard_res <- rstandard(my_model)
library(MASS)
student_res <- studres(my_model)
plot(my_model$fitted.values,standard_res)
plot(my_model$fitted.values,student_res)

#(e)
plot(dffits(my_model))
plot(cooks.distance(my_model))
plot(dfbetas(my_model))
#

#(f)
my_data <- my_data[c(1:8,10:25),]
my_model_f <- lm(y~x1+x2, data = my_data)
summary(my_model_f)
#1



#2
library(MASS)
Sigma <- matrix(c(10,3,3,2),2,2)
Sigma
a <- mvrnorm(n=1000, rep(0, 2), Sigma)
mean(a[,1])
var(a[,1])
#mu:input mean; Sigma: Covariance matrix; z: data point
mvdnorm <- function(mu,Sigma,z ){
  f = 1/(2*pi)*(det(Sigma))^(-0.5)*exp(-0.5*(z-mu)%*%solve(Sigma)%*%(z-mu))
  return(f)
}
mu = c(0,0)
z <- c(1.96,1.96)
mvdnorm(mu,Sigma,z)



#3
#(a)
my_data <- read.table("F:/STAT 601/HW4/patient-1.txt",head = TRUE)
my_model <- lm(y~x2,data = my_data)
summary(my_model)
confint(my_model)

my_model <- lm(y~x1+x2+x3,data = my_data)
library(car)
vif(my_model)


#(b)
x1.model <- lm(y~x1,data = my_data)
ssr.x1 <- sum((fitted(x1.model) - mean(my_data$y))^2)
x1x3.model <- lm(y~x1+x3,data = my_data)
sse.x1x3 <- sum((fitted(x1x3.model) - my_data$y)^2)
x3.model <- lm(y~x3,data = my_data)
sse.x3 <- sum((fitted(x3.model) - my_data$y)^2)
sse.x3-sse.x1x3


x2.model <- lm(y~x2,data = my_data)
ssr.x2 <- sum((fitted(x2.model) - mean(my_data$y))^2)
x2x3.model <- lm(y~x2+x3,data = my_data)
sse.x2x3 <- sum((fitted(x2x3.model) - my_data$y)^2)
sse.x3-sse.x2x3

#(d)
sse.f <- sum((fitted(my_model) - my_data$y)^2)
R <- (sse.x1x3-sse.f)/sse.x1x3
