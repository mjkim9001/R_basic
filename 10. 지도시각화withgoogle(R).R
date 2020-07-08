setwd("c:/Rdata")

library(dplyr)
install.packages("ggmap")
library(ggmap)
library(stringr)
library(rvest)

register_google(key = "________________")

tt = get_map(location = "충북 청주시 서원구 분평동",
        zoom = 15,
        maptype = "roadmap",
        source = "google")
ggmap(tt)

qmap(location = "충북 청주시 서원구 분평동",
        zoom = 15,
        maptype = "satellite", #hybrid,satellite, terrian,...
        source = "google")

plot.new()
frame()

#위도 경고 알려주는것
geocodeQueryCheck()

geocode(location = "충북 청주시 서원구 분평동",
        output = "latlon",
        source = "google")

geocode(location = enc2utf8(x = "충북 청주시 서원구 분평동$language=ko"),
        output = "latlon",
        source = "google")

myloc = geocode(location = '대전광역시 동구 대전로 171',
                output = 'latlon',
                source = 'google')
myloc

center = c(myloc$lon, myloc$lat)

qmap(location = center,
     zoom = 18,
     maptype='hybrid',
     source = 'google')+
  geom_point(data = myloc,
             mapping=aes(x=lon, y=lat),
             shape='o',
             color='red',
             stroke=18,size=10)

## 서울특별시의 대학목록추출하기

url ="https://namu.wiki/w/%EC%84%9C%EC%9A%B8%ED%8A%B9%EB%B3%84%EC%8B%9C%EC%9D%98%20%EB%8C%80%ED%95%99%EA%B5%90%20%EB%AA%A9%EB%A1%9D"
hdoc = read_html(url, encoding = 'UTF-8')

df = hdoc%>%
  html_nodes('.wiki-link-external , .wiki-link-internal')%>%
  html_text()
head(df)
univ= ifelse(str_detect(df, pattern="대학교"), df,"")
univ
kk = univ %>%
  data.frame()
kk=Filter(function(x){nchar(x)>=5}, univ)
kk

univName = kk[2:28]
univName

univCord = geocode(location = univName, 
                   output = 'latlon',
                   source = 'google')
univDf = data.frame(univ = univName,
                    lon=univCord$lon,
                    lat=univCord$lat)
head(univDf)
univDf = na.omit(univDf)
univDf

center = c(mean(x=univDf$lon),
          mean(x=univDf$lat))
center
qmap(location = center,
     zoom = 12,
     maptype = 'satellite',
     source = 'google')+
  geom_point(data = univDf,
             aes(x=lon, y=lat),
             shape='*',
             color='red',
             size=6)+
  geom_text(data = univDf,
            aes(x=lon, y=lat, label=univ),
            color='green',hjust=0.5,
            vjust = -0.1, size = 3,
            fontface = 'bold',
            family='gulim')

