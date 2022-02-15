#1
library(ggplot2)
library(faraway)

data(teengamb)

teengamb$sex <- factor(teengamb$sex)
summary(teengamb)

plot (density (teengamb$gamble))
plot (gamble ~ verbal,teengamb)
plot (gamble ~ status,teengamb)
boxplot(teengamb$gamble)
boxplot(teengamb$income)      

#4
md <- read.csv("F:/STAT 601/Spanish.csv")
D <- md[,3]-md[,2]
t.test(md[,3],md[,2],alternative = "less",paired = T)

boxplot(D)
Df <- data.frame(D)
ggplot(Df,aes(x = D)) +
  geom_density()
mean(((D -mean(D))/sd(D))^3) 

t.test(md[,3],md[,2],alternative = "less",paired = T,conf.level = 0.90)

pnorm(1.96,1,0.5)
n=1
p=0
while (p <= 0.8) {
  p = 1 - pnorm(1.96,sqrt(n)*0.5,1) + pnorm(-1.96,sqrt(n)*0.5,1)
  n = n+1
}

#5
X <- c(1870,1324,1446,1325,1759,1652,1364,1515,1065)
Y <- c(1121,408,184,16,741,170,991,711,734,202,893,742,335,444)
t.test(X,Y,alternative = "two.sided",mu=0)
