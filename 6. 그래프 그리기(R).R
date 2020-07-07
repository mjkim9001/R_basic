library(ggplot2)
mpg = as.data.frame(ggplot2::mpg)

# 배경 설정
ggplot(data = mpg, aes(x = displ, y = hwy))

# 그래프 추가(배경에 산점도 추가)
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()

# 축 범위를 조정하는 설정 추가
ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  xlim(3, 6) + 
  ylim(10,30)

kk = ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()
ggplotly(kk)

# ggplot(): 최종 보고용(색, 크기, 폰트 등 세부 조작 가능)
# qplot() : 전처리 단계 데이터 확인용(문법 간단, 기능 단순)

#혼자서 해보기
# Q1
ggplot(data = mpg, aes(x = cty, y = hwy)) + geom_point()

# Q2
midwest = as.data.frame(ggplot2::midwest)
ggplot(data = midwest, aes(x = poptotal, y = popasian)) +
  geom_point() +
  xlim(0, 500000)+
  ylim(0, 10000)


# 10만 단위가 넘는 숫자는 표기법에 따라 표현(1e+05 = 10만(1x10의 5승))
# 정수로 표현하기: options(scipen = 99)
# 지수로 표현하기: options(scipen = 0)

# 막대그래프
# 집단별 평균표 만들기
library(dplyr)
df_mpg = mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy))
df_mpg

# 그래프 생성
ggplot(data = df_mpg, aes(x = drv, y = mean_hwy)) + geom_col()

# 크기 순으로 정렬
ggplot(data = df_mpg, aes(x = reorder(drv, -mean_hwy), y=mean_hwy)) + geom_col()

# 빈도 막대 그래프
ggplot(data = mpg, aes(x = drv)) + geom_bar()

# x축 연속 변수, y축 빈도
ggplot(data = mpg, aes(x = hwy)) + geom_bar()

#geom_cal(): 평균 막대 그래프(데이터를 요약한 평균표를 먼저 만든 후 평균표를 이용해 그래프 생성)
#geom_bar(): 빈도 막대 그래프(별도로 표를 만들지 않고 원자료를 이용해 바로 그래프 생성)

#혼자서 해보기
# Q1
mpg
df_mpg = mpg %>%
  filter(class == 'suv') %>%
  group_by(manufacturer) %>%
  summarise(mean_cty = mean(cty)) %>%
  arrange(desc(mean_cty)) %>%
  head(5)
df_mpg

ggplot(data = df_mpg, aes(x = reorder(manufacturer, -mean_cty), y = mean_cty)) + geom_col()

# Q2
ggplot(data = mpg, aes(x=class)) + geom_bar()

kk = table(mpg$class)
tt = barplot(kk, col=rainbow(8), ylim=c(0,70))
text(tt, kk, paste0(kk, "대"), pos =3, col =2, cex=2)

df = as.data.frame(mpg %>%
  filter(class == 'suv') %>%
  group_by(manufacturer) %>%
  summarise(mean_cty = mean(cty),
            mean_hwy = mean(hwy)) %>%
  arrange(desc(mean_cty)) %>%
  head(5))

df

ggplot(data = df, aes(x = reorder( manufacturer, mean_cty), y = mean_cty)) + 
  geom_col(fill = rainbow(5)) +
  coord_flip() + 
  xlab('차종') + 
  ylab('평균도시연비')

# 선그래프

tt= ggplot(data = economics, aes(x = date, y = unemploy)) + geom_line()
ggplotly(tt)

# 혼자서 해보기
# Q1
ggplot(data = economics, aes(x = date, y = psavert)) + geom_line()

# 상자그림
ggplot(data = mpg, aes(x = drv, y = hwy)) + geom_boxplot()

x =c(3, 2, 1, 5, 6, 7)
sort(x)   # 오름차순으로 정렬
median(x) # 중간값 찾기

# 혼자서 해보기
# Q1
df_mpg = mpg %>%
  filter(class =='compact' | class =='subcompact' | class == 'suv')
df_mpg
kk = ggplot(data = df_mpg, aes(x = class, y = cty)) + geom_boxplot()
ggplotly(kk)
