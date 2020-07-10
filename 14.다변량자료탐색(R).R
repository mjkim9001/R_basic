#별그림(star chart, spider chart) & 나이팅게일 차트
crime = read.csv("http://datasets.flowingdata.com/crimeRatesByState-formatted.csv")

head(crime)

rownames(crime)

rownames(crime) = crime[, 1]
rownames(crime)
stars(crime[,2:8], flip.labels = FALSE, key.loc = c(15,1, 5), draw.segments = TRUE)

#체르노프 페이스
install.packages('aplpack')
library(aplpack)
faces(crime[,2:8])

#평행좌표플롯(parellel coordinate plot)
education = read.csv("http://datasets.flowingdata.com/education.csv")
library(lattice)
parallel(education[,2:7], horizontal.axis=FALSE, col=1)
