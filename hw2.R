#Prblem 3
x <- c(2,4,6,8,10,12,14,16,18)
y <- c(5,11,10,13,22,23,30,28,32)
lm(y~x)
plot(x,y,col = "blue",main = "temperature and heart rate",
     abline(lm(y~x)),cex = 1.3,pch = 16,xlab = "temperature",ylab = "heart rate")
text( 16,10,'Y = 1.917 + 1.742X')

#Problem 4

bar_x <- sum(x)/9
fenmu <- sum((x-bar_x)^2)
se <- sqrt(2.5*2.5/fenmu)

beta_1 <- pnorm(-1.96*se,1,1)+1-pnorm(1.96*se,1,1)
beta_neg_1 <- pnorm(-1.96*se,-1,1)+1-pnorm(1.96*se,-1,1)
beta_1.5 <- pnorm(-1.96*se,1.5,1)+1-pnorm(1.96*se,1.5,1)
beta_1.5_neg_1.5 <- pnorm(-1.96*se,-1.5,1)+1-pnorm(1.96*se,-1.5,1)




