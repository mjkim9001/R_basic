setwd("c:/Rdata")

library(dplyr)
library(ggplot2)

df = data(package = "ggplot2")

df$results

mpg = as.data.frame(ggplot2::mpg)
head(mpg)
table(mpg$drv)

df_g = mpg %>%
  group_by(drv)%>%
  summarise(mean_cty = round(mean(cty),1))

df_g

ggplot(data = df_g, aes(x=reorder(drv, -mean_cty), y=mean_cty))+
  geom_col(fill = c('red', 'blue','orange'))+
  geom_text(aes(label = df_g$mean_cty), hjust = -0.2, col='red')+ #vjust:막대그래프로 글자의 간격
  coord_flip()+
  xlab("구동타입")+ylab("도시연비")

## seed 함수
runif(3)
rnorm(3) #-1과 1사이의 숫자
rnorm(3, mean=0, sd=1)
set.seed(1234)
runif(15) # set.seed() 실행하면 똑같은 랜덤한 숫자가 나옴
