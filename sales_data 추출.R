library(dplyr)
setwd('c:/Rdata')

beverage = read.csv('sales_data.csv')

beverage_vision = beverage %>% filter(CATEGORY =='스포츠,이온음료' 
                                      | CATEGORY == '에너지음료'
                                      |CATEGORY == '차음료')

beverage_vision
write.csv(beverage_vision, 'beverage.csv')

beverage = read.csv('beverage.csv')
beverage2 = read.csv('beverage.csv')

beverage_test = beverage %>% filter( MAXTEMP =='스포츠,이온음료' 
                                      | CATEGORY == '에너지음료'
                                      |CATEGORY == '차음료')
