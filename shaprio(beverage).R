data = read.csv('beverage.csv')
hist(data$QTY, probability = T)
lines(density(data$QTY), col=2, type='h')
shapiro.test(data$QTY)
