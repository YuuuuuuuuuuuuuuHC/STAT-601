#2

#(a)
my_data <- read.table("D:/work/STAT 601/HW3/patient.txt",head = TRUE)


#(c)

my_model <- lm(y~x1+x2+x3,data = my_data)
summary(my_model)
confint(my_model)

#(d)
yy1 <-predict(my_model,newdata = data.frame(x1=35,x2=45,x3=2.2),interval="confidence")

#(e)
yy2 <- predict(my_model,newdata = data.frame(x1=35,x2=45,x3=2.2),interval="prediction")

#(f)
plot(my_model)


#3



set.seed(46)
X1 <- runif(100,10,15)
X2 <- runif(100,5,10)
epsi <- rt(100,10,0)

Y <- 6.33-0.024*X1 + 0.15*X2 + epsi
my_model <- lm(Y~X1+X2)
summary(my_model)

plot(my_model)


hist(residuals(my_model))
boxplot(residuals(my_model))

i <- c(1:100)
plot(i,residuals(my_model))
plot(i,residuals(my_model),type ="b" )
