setwd('c:/Rdata')


library(rvest)
library(stringr)
library(dplyr)

View(attitude)
cov(attitude)
cor(attitude)

with(attitude, cor.test(rating, complaints))
cor.test(attitude)
plot(attitude)

fasu = data.frame(fa, su)
fasu
lm(su~fa, data = fasu)

data = read.csv("cars.csv")
data
out = lm(dist~speed, data = data) #설명변수를 종속변수에 회귀분석
summary(out)

plot(dist~speed, data = data, col="blue")
abline(out, col="red")
lm(dist~speed+0, data = data)
out1 = lm(dist~speed+0, data = data)
plot(out1)
par(mfrow = c(2, 2))
shapiro.test(data$dist)
shapiro.test(log(data$dist))
shapiro.test(sqrt(data$dist))
out3 = lm(sqrt(dist)~speed+0, data = data)
summary(out3)
plot(out3)

out3$fitted.values
cbind(data$speed, out3$fitted.values)

out2 = lm(sqrt(dist)~speed+0, data = cars)
plot(out2)
shapiro.test(resid(out2))

data_new = data.frame(speed = data$speed)
predict(out2, data_new)
predict(out2,data_new,interval="confidence")
cbind(data_new$speed, fitted(out2))

# 다중회귀분석
data = read.csv("salary_data.csv")
data
out = lm(Salary~Experience+score, data=data)

out = lm(rating~.-critical, data = attitude)
summary(out)
backward = step(out, direction = "backward", trace=FALSE)
summary(backward)

both = step(out, direction = "both", trace = FALSE)
summary(both)

install.packages("leaps")
library(leaps)
leaps = regsubsets(rating~., data = attitude, nbest=5)
summary(leaps)
plot(leaps)
plot(leaps, scale= "bic")
plot(leaps, scale="adjr2")
plot(leaps, scale = "Cp")

out_bic=glm(rating~complaints, data = attitude)
summary(out_bic)

summary.out = summary(leaps)
which.max(summary.out$adjr2)
summary.out$which[11,]

out3 = lm(rating~complaints+learning+advance, data = attitude)
summary(out3)
