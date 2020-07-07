setwd('c:/Rdata')


library(rvest)
library(stringr)
library(dplyr)

rm(list = ls())
title = c()
grade = c()
url_b = "https://movie.naver.com/movie/point/af/list.nhn?&page="

for (i in 1:50){
  craw_url = paste(url_b, i,sep='')
  #craw_url = gsub("unknown","",craw_url)
  
  t_css = ".color_b"
  g_css = ".list_netizen_score"
  title_part = read_html(craw_url, encoding = 'CP949')%>%
    html_nodes(t_css)%>%
    html_text()
  
  grade_part = read_html(craw_url, encoding = 'CP949')%>%
    html_nodes(g_css)%>%
    html_text()
  g_part = str_sub(grade_part, -5, end = -5)
  g_part = ifelse(g_part == '0', '10', g_part)
  title = c(title, title_part)
  grade = c(grade, g_part)
}
grade
movies = data.frame(title,grade)
View(movies)
