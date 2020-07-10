setwd('c:/Rdata')

df = read.csv("beverage.csv")

head(df)

summary(df)

# 단순회귀 : 가격 증가시 판매량 증가 상관관계
model = lm(PRICE ~ QTY, data = df)
summary(model)
# 다중회귀 : 경력 증가시 적성검사 점수 증가로 인한 연봉 증가까지 포함된 관계

model <- lm(salary ~ experience + score, data = df)
summary(model)