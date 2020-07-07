# 결측치 찾기
gender = c('M','F',NA, 'M','F')
score = c(5, 4, 3, 4, NA)
df = data.frame(gender, score)
df

#결측치 확인
is.na(df)

table(is.na(df))  #결측치 빈도 출력

# 변수별로 결측치 확인
table(is.na(df$gender))
table(is.na(df$score))

#결측치 포함된 상태로 분석(NA로 출력)
mean(df$score)
sum(df$score)

#결측치 제거
library(dplyr)
df %>% filter(is.na(score)) #score가 NA인 데이터만 출력

df %>% filter(!is.na(score)) #score 결측치 제거

#결측치 제외한 데이터로 분석
df_nomiss = df %>% filter(!is.na(score))

mean(df_nomiss$score)
sum(df_nomiss$score)

#여러 변수 동시에 결측치 없는 데이터 추출(score, gender 결측치 제외)
df_nomiss = df %>% filter(!is.na(score) & !is.na(gender))
df_nomiss

#결측치가 하나라도 있으면 제거
df_nomiss2 = na.omit(df)
df_nomiss2

#함수의 결측치 제외 기능 이용
mean(df$score, na.rm = T) # 결측치를 제외하고 평균 산출
sum(df$score, na.rm = T)

#결측치 생성
setwd('c:/Rdata') # 파일을 가져올 경로 설정
exam = read.csv('csv_exam.csv')

exam[c(3, 8, 15), 'math'] = NA  #3, 8, 15행의 math에 NA할당
exam %>% summarise(mean_math = round(mean(math, na.rm=T)))
kk = table(is.na(exam$math))
tt = barplot(kk,col = rainbow(2))
text(tt, kk, labels =paste0(kk, "건"), pos=3)


exam %>% summarise(mean_math = mean(math))
exam %>% summarise(mean_math = mean(math, na.rm = T))

#다름 함수들에 적용
exam %>% summarise(mean_math = mean(math, na.rm = T),
                   sum_math = sum(math, na.rm = T),
                   median_math = median(math, na.rm = T))
mean(exam$math, na.rm = T)

# NA값을 평균으로 대체
exam$math = ifelse(is.na(exam$math), 55, exam$math)
table(is.na(exam$math))
exam

library(ggplot2)

#혼자서 해보기
mpg = as.data.frame(ggplot2::mpg)
mpg[c(65, 124, 131, 153, 212), 'hwy'] = NA

# Q1
table(is.na(mpg$drv))
table(is.na(mpg$hwy))

# Q2
df_mpg = mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy =round(mean(hwy, na.rm=T),1))
df_mpg
ggplot(data = df_mpg, aes(x=drv, y=mean_hwy))+geom_col() #이러한 그림을 그리겠다 정의해주는것
#geom_col(막대그래프형식)
ggplot(data = mpg, aes(x = drv))+geom_bar() # 빈도


# 이상치 제거
outlier = data.frame(gender = c(1, 2, 1, 3, 2, 1),
                     score = c( 5, 4, 3, 4, 2, 6))
outlier

# 이상치 확인
table(outlier$gender)
table(outlier$score)

# 결측처리 - gender (gender가 3이면 NA할당)
outlier$sex = ifelse(outlier$gender == 3, NA, outlier$gender)
outlier

# sex가 1~5 아니면 NA할당
outlier$score = ifelse(outlier$score > 5, NA, outlier$score)
outlier

# 결측치 제외하고 분석
outlier %>%
  filter(!is.na(gender) & !is.na(score)) %>%
  group_by(gender) %>%
  summarise(mean_score = mean(score))

# = group_by(gender, na.rm =T)
#   summarise(mean_score=mean(score, ma.rm=T))


# 상자그림 생성
mpg = as.data.frame(ggplot2::mpg)
boxplot(mpg$hwy)
boxplot(mpg$hwy, horizontal = T, col = 2)
summary(mpg$hwy)
IQR(mpg$hwy)
hist(mpg$hwy, probability = T)
lines(density(mpg$hwy), type = 'h', col=2)

# 상자그림 통계치 출력
boxplot(mpg$hwy)$stats

# 결측 처리하기
mpg$hwy = ifelse(mpg$hwy <12 | mpg$hwy >37, NA, mpg$hwy)
kk=table(is.na(mpg$hwy))
tt = barplot(kk, col=rainbow(2), ylim=c(0,250))
text(tt, paste0(kk, "건"), pos=3)

# 결측치 제외하고 분석
mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy, na.rm = T))

# 혼자서 해보기
mpg = as.data.frame(ggplot2::mpg)
mpg[c(10, 14, 58, 93), 'drv'] = 'k'
mpg[c(29,43, 129, 203), 'cty'] = c(3, 4, 39, 42)
mpg
# Q1
table(mpg$drv)
mpg$drv = ifelse(mpg$drv %in% c('4', 'f','r'), mpg$drv, NA)

table(mpg$drv)
kk = table(is.na(mpg$drv))
tt = barplot(kk, col=rainbow(2), ylim=c(0,250))
text(tt, paste0(kk, "건"), pos=3)

#Q2
boxplot(mpg$cty)$stats
mpg$cty = ifelse(mpg$cty < 9 | mpg$cty >26, NA, mpg$cty)
table (is.na(mpg$cty))
boxplot(mpg$cty)

#Q3
df_mpg = mpg %>%
  filter(!is.na(drv) & !is.na(cty)) %>%
  group_by(drv) %>%
  summarise(mean_cty = mean(cty))

install.packages('plotly')
library(plotly)
ggplot(data = df_mpg, aes(x=drv, y=mean_cty))+geom_col(fill = rainbow(3))
#오름차순으로 정렬
ggplot(data = df_mpg, aes(x=reorder(drv,mean_cty), y=mean_cty))+geom_col(fill = rainbow(3))
#수평
tt = ggplot(data = df_mpg, aes(x=reorder(drv,mean_cty), y=mean_cty))+geom_col(fill = rainbow(3)) + coord_flip()
#상호간 주고받기 위한 (각 바의 정보를 알려줌)
