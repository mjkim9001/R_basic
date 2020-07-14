setwd("c:/Rdata")
library(dplyr)
library(party)
library(e1071)
library(nnet)
data = read.csv("beverage.csv")

data1 = data %>% filter(CATEGORY =="스포츠,이온음료")
data2 = data %>% filter(CATEGORY =="에너지음료")

select.column <- c('CATEGORY','X', 'YM','ITEM_CNT','QTY', 'PRICE', 'MAXTEMP', 'SALEDAY','RAIN_DAY','HOLIDAY')
select.column
obj.view <- subset(data1, select = select.column)
obj.view <- obj.view[complete.cases(obj.view),]

obj.view

index <- sample(2, nrow(obj.view), replace = TRUE, prob = c(0.7, 0.3))
data.train <- obj.view#[index==1,]
data.test <- obj.view#[index==2,]



obj.view <- subset(data1, select = select.column)
m <- lm(QTY ~., data = obj.view)
m2 <- step(m , direction = "both")
m2
summary(m2)

obj.view <- subset(data1, select = select.column)
data.test1 <- obj.view#[index==2,]
drops <- c('QTY')
data.test1.temp <- data.test1[,!(names(data.test1) %in% drops)]
result <- predict(m2, newdata = data.test1.temp)
data.test1$pred <- result
predict <- subset(data.test1, select = c(QTY , pred))
predict$YM <- data1$YM

View(predict)
write.csv(predict, 'predict.csv')
