#데이터 프레임 만들기
english = c(90, 80, 60,70)
english

math = c(50, 60, 100, 20)
math

df_midterm = data.frame(english, math) # 데이터 프레임 생성하기
df_midterm

class = c(1, 1, 2, 2)
class

df_midterm = data.frame(english, math, class)
df_midterm

mean(df_midterm$english) # 데이터 프레임의 english 컬럼으로 평균 산출

mean(df_midterm$math) # 데이터 프레임의 math 컬럼으로 평균 산출

sd(df_midterm$english) # 데이터 프레임의 english 컬럼으로 표준편차 산출(표준편차 = 분산에 루트를 씌운값)
# = sqrt (var(df_midterm$english))

var(df_midterm$english) # 데이터 프레임의 english 컬럼으로 분산 산출

# 데이터 프레임 한 번에 만들기
df_midterm = data.frame(english = c(90, 80, 60, 70), 
                        math = c(50,60,100,20),
                        class = c(1,1,2,2))
df_midterm

# 과일 가격과 판매량에 대한 데이터 프레임 만들기
product = c('사과','딸기','수박')
price = c(1800, 1500, 3000)
amount = c(24, 38, 13)

df_fruit = data.frame(product, price, amount)
df_fruit

df_fruit = data.frame(product = c('사과','딸기','수박'),
                      price = c(1800, 1500, 3000),
                      amount = c(24, 38, 13))
df_fruit

p_mean = mean(df_fruit$price)
a_mean = mean(df_fruit$amount)
p_mean
a_mean

# 엑셀파일 불러오기 위한 패키지 설치
install.packages('readxl')
library(readxl)

setwd('c:/Rdata') # 파일을 가져올 경로 설정

#엑셀 파일 불러오기
df_exam = read_excel('파일명.xlsx')

#직접 경로 지정
df_exam = read_excel('full path')

#엑셀의 첫번째 행이 변수명이 아니라면
df_exam_novar = read_excel('파일명.xlsx', col_names = F)

#엑셀 파일에 시트가 여러 개라면
df_exam_sheet = read_excel('파일명.xlsx', sheet = 3)

# 데이터 프레임을 csv파일로 저장
write.csv(df_midterm, file = 'df_midterm.csv')

#csv파일 불러오기
df_mid_test = read.csv('df_midterm.csv')
df_mid_test

#RDate 파일 활용하기(R 전용 데이터 파일/ 용량 작고 빠름)
save(df_midterm, file = 'df_midterm.rda')

#RDate 불러오기
rm(df_midterm)
df_midterm

load('df_midterm.rda')
df_midterm
