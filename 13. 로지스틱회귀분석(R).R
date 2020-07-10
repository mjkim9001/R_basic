setwd("c:/Rdata")

data = read.csv("programming.csv")

head(data)

model = glm(Success~Experience, data = data, family=binomial(logit))
summary(model)
# 종속변수 Success 독립변수 Experience

cbind(data$Experience, model$fitted.values)
plot(Success~Experience, data = data)

points(model$fitted.values~data$Experience, col=2)

c('민감도' = 8/11, '특이도'=11/14)
table(data$Success,model$fitted.values>0.5)

data = read.csv('coupon.csv')
head(data)
model2 = glm(cbind(N_redeemed, N-N_redeemed)~Price_reduc, data = data, family=binomial(logit))
summary(model2)
exp(0.096834)

data = read.csv('disease.csv')
head(data)
model3 = glm(disease~., data=data, family = binomial(logit))
summary(model3)

model4 = glm(disease~age+sector,data=data, family = binomial(logit))
summary(model4)

#Reduced Model과 Full Model의 차이가 유의한지 점검
#여러 설명변수가 주는 영향이 유의한지 한번에 점검(다중회귀분석의 F-test와 유사)
anova(model3, model4, test = "Chisq")

table(data$disease) # 총 98명 중 확진자 31명(1)
31/98 # 인계값

kk =table(data$disease, model4$fitted.values>0.3163265)
sum(kk)

#민감도(True를 True로 구분한 비율)
#특이도(False를 False로 구분한 비율)
reduce_M = c('민감도'=23/31, '특이도' = 47/(47+20))
kk1 = table(data$disease, model3$fitted.values>0.3163265)
kk1
fulmode_M = c('민감도'=23/31, '특이도'=49/(49+18))
reduce_M
fulmode_M
cbind(data$disease, model4$fitted)

err_m1 = 28/sum(kk)
kk1
err_m2 = 26/sum(kk1)
err_m1
err_m2

install.packages("Deducer")
library(Deducer)
rocplot(model3)


##연 습 문 제 풀 이

data = read.csv('flushot.csv')
head(data)
log_model = glm(flushot~., data = data, family = binomial(logit))
summary(log_model)
exp(0.07279)
exp(-0.09899)
exp(0.43397)

log_model2 = glm(flushot~age+aware, data = data, family = binomial())
summary(log_model2)
table(data$flushot)
24/(134+24)
tt=table(data$flushot, log_model2$fitted.values>0.1518987)
tab_02=table(data$flushot, log_model2$fitted.values>0.2)
tab_01=table(data$flushot, log_model2$fitted.values>0.1)
tab_015=table(data$flushot, log_model2$fitted.values>0.15)
c('민감도'=19/(5+19), '특이도'=95/(95/40), '에러율' = 45/(sum(tt)))
rocplot(log_model2)
tab_01
tab_015
tab_02
res01=c('민감도'=tab_01[2,2]/sum(tab_01[2,]),
  '특이도'=tab_01[1,1]/sum(tab_01[1,]),
  '에러율'=tab_01[1,2]+tab_01[2,1]/sum(tab_01))
res01

res015=c('민감도'=tab_015[2,2]/sum(tab_015[2,]),
        '특이도'=tab_015[1,1]/sum(tab_015[1,]),
        '에러율'=tab_015[1,2]+tab_015[2,1]/sum(tab_015))
res015
res02=c('민감도'=tab_02[2,2]/sum(tab_02[2,]),
        '특이도'=tab_02[1,1]/sum(tab_02[1,]),
        '에러율'=tab_02[1,2]+tab_02[2,1]/sum(tab_02))
res02

model4$fitted.values

jang = function(){
  k=seq(0.01, 0.5, 0.01)
  
  n=length(k)
  
  err_min=vector(length=n)
  sens=vector(lenath=n)
  spec=vector(length=n)
  
  for(i in 1:n){
    tab = table(data$flushot, log_model2$fitted.values>k[i])
    res = c('민감도'=tab[2,2]/sum(tab[2,]),
            '특이도'=tab[1,1]/sum(tab[1,]),
            '에러율'=tab[1,2]+tab[2,1]/sum(tab))
    
    err_min[i] = (tab[1,2]+tab[2,1]/sum(tab))
    sens[i] = tab[2,2]/sum(tab[2,])
    spec[i] = tab[1,1]/sum(tab[1,])
    print(res)
  }
  print(err_min)
  print(paste("최소의 Error Rate = ", min(err_min), "이다."))
  index = which(err_min<=min(err_min))
  print(index)
  print(paste("해당하는 민감도 = ", sens[min(index)], "이다."))
  print(paste("해당하는 특이도 = ", spec[min(index)], "이다."))
  print(paste("해당하는 에러율 = ", err_min[min(index)], "이다."))
  print(paste("해당하는 cutoff = ", k[min(index)], "이다."))
  
  plot(1-spec.sens.col=2)
  }
jang()
