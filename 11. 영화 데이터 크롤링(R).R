## movie 크롤링을 통한 시나리오 분석
setwd("c:/Rdata")
library(rvent)
library(stringr)
library(dplyr)
library(ggplot2)

base_url = "https://movie.naver.com/movie/point/af/list.nhn?&page="



title = c()
grade = c()
time = c()

t_css = ".color_b"
g_css = ".list_netizen_score em"
pt_css = '.title+ .num'
for (i in 1:1000){
  cr_url= paste0(base_url, i,sep='')
  hdoc = read_html(url, encoding = 'cp949')
  
  df = hdoc %>%
    html_nodes(g_css) %>%
    html_text()
  
  n_title = html_nodes(hdoc, t_css)
  n_grade = html_nodes(hdoc, g_css)
  n_time = html_nodes(hdoc, pt_css)
  
  p_title = html_text(n_title)
  p_grade = html_text(n_grade)
  p_time = str_sub(html_text(n_time), -8)
  
  title = c(title, p_title)
  grade = c(grade, p_grade)
  time = c(time, p_time)
}

movie = data.frame(title, grade, time)
View(movie)

write.csv(movie, "movie.csv")

data = read.csv("movie.csv")
head(data)

# 1. 가장 높은 평가총점을 가진 것의 Top10의 영화제목을 찾아라
top10_A= data %>%
  select(title, grade) %>%
  group_by(title) %>%
  summarise(total = sum(grade),
            count = n()) %>%
  arrange(desc(total), desc(count))%>%
  head(10)
top10_A

ggplot(data = top10_A, aes(x = reorder(title, total), y = total))+
  geom_col(fill = rainbow(10))+
  geom_text(aes(label=top10_A$total), hjust = -0.2)+
  coord_flip()

