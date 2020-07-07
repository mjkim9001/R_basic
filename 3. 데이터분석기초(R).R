hist(mpg$hwy, probability = T)
lines(density(mpg$hwy), col=2, type='h', lwd=1)
shapiro.test(mpg$hwy)

setwd('c:/Rdata') # 파일을 가져올 경로 설정

exam = read.csv("csv_exam.csv")

#앞부분 데이터 출력(디폴트 값 = 6)
head(exam)

head(exam, 10)

#뒷부분 데이터 출력(디폴트 값 =6)
tail(exam)
tail(exam, 10)

#뷰어 창에서 데이터 확인
View(exam)

#몇 행 몇 열로 구성되었는지
dim(exam)

#속성 파악
str(exam)

#요약통계량 산출
summary(exam)

boxplot(exam$math, horizontal = T,col=2)
hist(exam$math)

x=sample(0:100, 80, replace = T)
plot(x, pch = ifelse(x>=60, 7, 15), col = ifelse(x>=60, 4, 15)) # pch찍히는 점의 모양 선택
abline(h=60, col=2, lwd=2)

#ggplo2의 mpg 데이터를 데이터 프레임으로 불러오기
mpg = as.data.frame(ggplot2::mpg)
mpg
summary(mpg)

# dplyr 패키지 설치
install.packages('dplyr')
library(dplyr)

df_raw = data.frame(var1 = c(1, 2, 1),
                    var2 = c(2, 3, 2))
df_raw

#데이터 프레임 복사본
df_new = df_raw
df_new

#변수명 바꾸기(rename(새 변수명 = 기본 변수명))
df_new = rename(df_new, v2 = var2)
df_new

# ggplot2의 mpg데이터 불러온뒤 복사본
mpg = as.data.frame(ggplot2::mpg)
mpg_copy = mpg
mpg_copy
#복사본 데이터 이름변경
mpg_copy = rename(mpg_copy, city = cty, highway = hwy)
head(mpg_copy)

write.csv(mpg_copy, 'mpg.csv')

df = data.frame(var1 = c(4, 3, 8),
                var2 = c(2, 6, 1))
df

#파생변수 생성
df$var_sum = df$var1 +df$var2
df$var_mean = (df$var1 + df$var2)/2
df

#mpg 통합 연비 변수
mpg$total = (mpg$cty + mpg$hwy) /2
head(mpg)

mean(mpg$total)

# 기준값 정하기
summary(mpg$total)
hist(mpg$total)
boxplot(mpg$total, horizontal = T)

# 조건문으로 합격 판정 변수 만들기
# (20이상이면 pass, 그렇지 않으면 fail 부여)
mpg$test = ifelse(mpg$total >= 20, 'pass','fail')
head(mpg, 20)

# 빈도표로 합격 판정 자동차 수 살펴보기
table(mpg$test)

# 막대 그래프빈 빈도 표현
library(ggplot2)
qplot(mpg$test)

# 중첩 조건문 활용
mpg$grade = ifelse(mpg$total >= 30, 'A', ifelse(mpg$total >=20, 'B', 'C'))
head(mpg, 20)

# 원하는 만큼 범주 만들기
mpg$grade2 = ifelse(mpg$total >= 30, 'A', ifelse(mpg$total >= 25, 'B',
                                                 ifelse(mpg$total >= 20, 'C', 'D')))

midwest = as.data.frame(ggplot2::midwest)
library(dplyr)
midwest_new = rename(midwest, total = poptotal, asian = popasian)
head(midwest_new)

midwest_new$ratio = midwest_new$asian/midwest_new$total*100
hist(midwest_new$ratio)

mean(midwest_new$ratio)
midwest_new$group = ifelse(midwest_new$ratio > 0.4872462, 'large', 'small')

table(midwest_new$group)
library(ggplot2)
qplot(midwest_new$group)
