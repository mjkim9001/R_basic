library(dplyr)
exam = read.csv('csv_exam.csv')

# class가 1인 경우만 추출하여 출력(ctrl+shit+M)
exam %>% filter(class==1)
exam %>% filter(class==2)

# class 1이 아닌경우
exam %>% filter(class != 1)

# 여러 조건중 하나 이상 충족하는 행 추출
exam %>% filter(class != 1 | class != 3)

# 2반 이면서 영어점수가 80점 이상인 경우
exam %>% filter(class ==2 & english >= 80)

# 1, 3, 5반이면 출력
exam %>% filter(class %in% c(1,3,5))

# 혼자서 해보기
# Q1
mpg = as.data.frame(ggplot2::mpg)

a = mpg %>% filter(displ <=4)
b = mpg %>% filter(displ >=5)

mean4 = mean(a$hwy)
mean5 = mean(b$hwy)

# Q2
mpg_audi = mpg %>% filter(manufacturer == 'audi')
mpg_toyota = mpg %>% filter(manufacturer == "toyota")

mean(mpg_audi$cty)

mean(mpg_toyota$cty)

# Q3
mpg_new = mpg %>% filter(manufacturer %in% c('chevrolet', 'ford','honda'))
mean(mpg_new$hwy)

# 필요한 변수만 추출
exam %>% select(math)
exam %>% select(english)

# 여러 변수 추출
exam %>% select(class, math, english)

# 변수 제외
exam %>% select(-math)
exam %>% select(-math, -english)

#dplyr 함수 조합
exam %>% filter(class == 1) %>% select(english)

# 가독성 있게 줄 바꾸기
exam %>%
  filter(class ==1 ) %>%
  select(english)

# 일부만 출력
exam %>%
  select(id, math) %>%
  head
exam %>%
  select(id, math) %>%
  head(10)

#혼자서 해보기

# 오르차순으로 정렬
exam %>% arrange(math)

# 내림차순 정렬
exam %>% arrange(desc(math))

# 정렬 기준 변수 여러개 지정
exam %>% arrange(class, math)

# 혼자서 해보기

# 파생변수 추가
exam %>%
  mutate(total = math + english + science) %>%
  head
# 여러 파생변수 한 번에 추가
exam %>%
  mutate(total = math + english + science,
         mean = (math + english + science)/3) %>%
  head

#mutate()에 ifelse() 적용하기
exam %>%
  mutate(test = ifelse(science >=60, 'pass','fail')) %>%
  head

# 추가한 변수를 dplyr 코드에 바로 활용
exam %>%
  mutate(total = math + english + science) %>%
  arrange(total) %>%
  head

# 혼자서 해보기
# Q1
mpg_copy = mpg
mpg_copy$sum = mpg_copy$cty + mpg_copy$hwy

# Q2
mpg_copy$avg = mpg_copy$sum / 2

# Q3
mpg_copy %>% 
  arrange(desc(avg)) %>%
  head(3)

# Q4

# 요약
exam %>% summarise(mean_math = mean(math))

# 집단별 요약
exam %>%
  group_by(class) %>%
  summarise(mean_math = mean(math))

# 여러 요약통계량 한 번에 산출
exam %>%
  group_by(class) %>%
  summarise(mean_math = mean(math),
            sum_math = sum(math),
            median_math = median(math),
            n = n())

# 각 집단별로 다시 집단 나누기
mpg %>%
  group_by(manufacturer, drv) %>%
  summarise(mean_cty = mean(cty)) %>%
  head(10)

# dplyr 조합하기
mpg %>%
  group_by(manufacturer) %>%          #회사별로 분리
  filter(class == 'suv') %>%          #suv 추출
  mutate(tot = (cty+hwy)/2) %>%       #통합 연비 변수 생성
  summarise(mean_tot = mean(tot)) %>% #통합 연비 평균 산출
  arrange(desc(mean_tot)) %>%         #내림차순 정렬
  head(5)                             #1~5위까지 출력

# 혼자서 해보기
#Q1
mpg %>%
  group_by(class) %>%
  mutate(avg = mean(cty)) %>%
  arrange(avg) %>%
  head

#Q2

# 가로로 합치기
test1 = data.frame(id = c(1, 2, 3, 4, 5),
                   midterm = c(60, 80, 70, 90, 85))
  
test2 = data.frame(id = c(1, 2, 3, 4, 5),
                   final = c(70, 83, 65, 95, 80))

# id 기준으로 합치기
total = left_join(test1, test2, by = 'id')
total

# 다른 데이터 활용해 변수 추가
name = data.frame(class = c(1, 2, 3, 4, 5),
                 teacher = c('kim', 'lee','park','choi','jung'))
name

# class 기준 합치기

exam_new = left_join(exam, name, by = 'class')
exam_new

#세로로 합치기
group_a = data.frame(id = c(1, 2, 3, 4, 5),
                     test = c(60, 80, 70, 90, 85))
group_b = data.frame(id = c(6, 7, 8, 9, 10),
                     test = c(70, 83, 65, 95, 80))

group_all = bind_rows(group_a, group_b)


# 혼자서 해보기