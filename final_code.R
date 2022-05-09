dt <- read.csv('F:/STAT 601/final/common_household_food.csv')
dt <- dt[,c(2:22)]

model <- lm(KCal~Weight+Water+Protein+Fat+SatFat+MonoUnSatFat+
              PolyUnSatFat+Chol+Carb+Ca+P+Fe+K+Na+VitaA.IU.+VitaA.RE.+Thiamin+Riboflavin+Niacin+VitaC,data=dt)
summary(model)
library (car)
vif (model)

model.step <-  step(model, direction = "backward")
summary(model.step)

vif(model.step)

plot(model.step)

hist(residuals(model.step))
boxplot(residuals(model.step))
i <- c(1:961)
plot(i,residuals(model.step))
plot(i,residuals(model.step),type ="b" )

dt <- dt[c(1:961),]

#new_dt <- dt[dt$KCal != 0,]
library(MASS) 
bc <- boxcox(KCal ~ Weight + Protein + Fat + SatFat + MonoUnSatFat + Chol + 
               Carb + Ca + P + Fe + K + Na + Thiamin + Riboflavin + VitaC, data=new_dt)
lambda <- bc$x[which.max(bc$y)] # 0.99

bc_model <- lm(KCal-1~Weight+Water+Protein+Fat+SatFat+MonoUnSatFat+
              PolyUnSatFat+Chol+Carb+Ca+P+Fe+K+Na+VitaA.IU.+VitaA.RE.+Thiamin+Riboflavin+Niacin+VitaC,data=dt)


colnames(dat)

RSE7 <- numeric(0)

grid <- expand.grid(Fats = 4:7,
                    Minerals = 10:14,
                    Vitamin = 15:20)
grid


for (i in 1:120){
temp1 <- dat[, c( grid[i,]$Fats, grid[i,]$Minerals, grid[i,]$Vitamin ) ]
# str(temp1)

dat_new <- cbind.data.frame(dat[,c(1,2,3,8,9)], temp1 )
# dim(dat_new)

temp2 <- lm(KCal ~ . , dat_new)

# summary(temp2)$sigma
RSE7[i] <- summary(temp2)$sigma

}

which.min(RSE7) ## 53
RSE7[53]       ## 17.08033
RSS53 <- (RSE7[53])^2 *(961-7-1)  

best_model <- lm(KCal ~ . , dt <- data.frame(dat[,c(1,2,3,4,8,9,13,17)]))
pred_data <- data.frame(Weight = 33,KCal = 0,Protein=3,Fat=1.5,Chol=0,Carb=26,K=95,Thiamin=0)
predict(best_model,newdata=pred_data)
predict.lm(best_model,pred_data,interval = 'prediction')

x1 <- matrix(0,5,5) 
x1[,1] <- rep(1,5)
x1 <- x1 - diag(5)

x2 <- matrix(0,5,5) 
x2[,2] <- rep(1,5)
x2 <- x2 - diag(5)

x3 <- matrix(0,5,5) 
x3[,3] <- rep(1,5)
x3 <- x3 - diag(5)

x4 <- matrix(0,5,5) 
x4[,4] <- rep(1,5)
x4 <- x4 - diag(5)

x5 <- matrix(0,5,5) 
x5[,5] <- rep(1,5)
x5 <- x5 - diag(5)

X <- rbind(x1,x2,x3,x4,x5)

Y1 <- c(0, 0.414, 0.807, 0.876, 1.291, -0.414, 0, 0.429, 0.533, 0.886,
        -0.807, -0.429, 0, 0.043, 0.377, -0.876, -0.533, -0.043, 0, 0.271,
        -1.291, -0.886, -0.377, -0.271, 0)

Y2 <- c(0, 0.247, 0.856, 1.051, 1.402, -0.247, 0, 0.434, 0.521, 0.867,
        -0.856, -0.434, 0, 0.058, 0.443, -1.051, -0.521, -0.058, 0, 0.374,
        -1.402, -0.867, -0.443, -0.374, 0)

Y3 <- c(0, 0.443, 0.895, 0.973, 1.281, -0.443, 0, 0.477, 0.503, 0.856,
        -0.895, -0.477, 0, 0.107, 0.432, -0.973, -0.503, -0.107, 0, 0.392,
        -1.281, -0.856, -0.432, -0.392, 0)

solve( t(X)%*%X ) %*% t(X) %*% Y1

det(t(X)%*%X)   ### 7.105427e-12, very close to 0



X2 <- X[,-1]
det(t(X2)%*%X2)  ## 2000

solve( t(X2)%*%X2 ) %*% t(X2) %*% Y1


solve( t(X2)%*%X2 ) %*% t(X2) %*% Y2


solve( t(X2)%*%X2 ) %*% t(X2) %*% Y3




Yt <- c(Y1,Y2,Y3)
Xt <- rbind(X2,X2,X2)
dim(Xt)  

solve( t(Xt)%*%Xt ) %*% t(Xt) %*% Yt




lm_fit <- lm(Yt ~ Xt -1 )   
summary(lm_fit)

summary(lm_fit)$sigma  

rss <- (summary(lm_fit)$sigma)^2 * 71
rss 





