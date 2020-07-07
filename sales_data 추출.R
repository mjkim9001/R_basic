setwd('c:/Rdata')

beverage = read.csv('sales_data.csv')

beverage_vision = beverage %>% filter(CATEGORY =='스포츠,이온음료' 
                                      | CATEGORY == '에너지음료'
                                      |CATEGORY == '차음료')

beverage_vision
write.csv(beverage_vision, 'beverage.csv')
