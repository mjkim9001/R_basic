x = rnorm(100, 175, 2) #정규분포로부터 랜덤 추출
print(x)
hist(x,breaks = 5, probability = T) # 시각화를 위한 히스토그램을 그리는 함수
lines(density(x), col=2, type='h', lwd = '0.5')
shapiro.test(x)

a <-1
a = 2
b =5
a+b
var1 <- c(1, 2, 5, 7, 8)
var1
var2 = c(1:5) #1~5까지의 값
var2

var3 = seq(1,5)
var3
var4 = seq(1, 10, by =2)
var4
var5 = seq(1, 10, by = 3)
var5

var1 + var2
var1 +2

# 문자로 된 변수
str1 = 'a'
str1
str2 = 'text'
str2
str3 = 'Hollo World!'
str3
# 연속된 문자 변수 (문자로 된 변수는 연산 불가)
str4 = c('a', 'b','c')
str4
str5 = c('Hello!', 'World','is','good!')
str5

# 숫자를 다루는 함수
x1 = c(1,4,7)
x1
mean(x1)
max(x1)
min(x1)
sd(x1)

# 문자를 다루는 함수
paste(str5, collapse=',')
paste(str5, collapes=" ")

x_mean = mean(x1)
x_mean
str5_paste = paste(str5, collapse=' ')
str5_paste

# ggplot2 패키지
install.packages('ggplot2')
library(ggplot2) # 패키지 활성화
x <- c('a', 'a','b','c')
x
qplot(x)

# mpg데이터로 그래프 만들기(data에 mpg, x축에 hwy 변수 지정)
qplot(data = mpg, x = hwy) 

# qplot() 파라미터 바꿔보기
qplot(data = mpg, x=cty) # x축 cty

qplot(data=mpg, x=drv, y=hwy)

qplot(data = mpg, x=drv, y=hwy, geom = 'line') # 선 그래프 형태

qplot(data = mpg, x = drv, y=hwy, geom = 'boxplot') # 상자그림형태

qplot(data = mpg, x = drv, y = hwy, geom = 'boxplot', colour = drv) #  drv별 색

scores = c(80, 60, 70, 50, 90)
mean(scores)
s_mean = mean(scores)
s_mean
