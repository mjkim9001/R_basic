setwd("c:/Rdata")
library(dplyr)
data = read.csv("beverage.csv")

data1 = data %>% filter(CATEGORY =="������,�̿�����")
data2 = data %>% filter(CATEGORY =="����������")

# ������ ��ȯ ����
head(data1)
head(data2)
data1
data2

shapiro.test(data1$QTY)

shapiro.test(data2$QTY)

hist(data1$QTY)
hist(data2$QTY)
set.seed(1)
#idx = sample(1:nrow(data1), 0.7*nrow(data1))
train_data1 = data1[0:48,]
train_data1
test_data1 = data1[49:60,]
test_data1
#idx = sample(1:nrow(data2), 0.7*nrow(data2))
train_data2 = data2[0:34,]
train_data2
test_data2 = data2[35:46,]
test_data2

# �Ǹŷ� ������� �м�
#data3 = data1["X", "ITEM_CNT","PRICE", "MAXTEMP","SALEDAY","RAIN_DAY","HOLIDAY"]
cor(data1[,4:9])
#data2 = data2[,4:9]
cor(data2[,4:9])

# ȸ�͸����� ���� ȸ�ͽ�
out1 = lm(QTY ~ X + ITEM_CNT + PRICE + MAXTEMP + SALEDAY + RAIN_DAY +HOLIDAY data = train_data1)
out2 = lm(QTY ~ X + ITEM_CNT + PRICE + MAXTEMP + SALEDAY + RAIN_DAY +HOLIDAY, data = train_data2)
both1 = step(out1, direction = "both", trace = FALSE)
both2 = step(out2, direction = "both", trace = FALSE)

summary(both1)
#summary(out1)
# ������� 86.62 ARQ 85.17 F_value = 59.87
summary(both2)
#summary(out2)
# ������� 83.31 ARQ 81.29 F_value = 45.89


pred1 = predict(out1, test_data1)
cor(pred1, test_data1$QTY)  #����ġ 89%

pred2 = predict(out2, test_data2)
cor(pred2, test_data2$QTY)  #����ġ 91%
