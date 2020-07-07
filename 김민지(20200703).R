# 1
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

#2
#Q1
mpg = as.data.frame(ggplot2::mpg)
df = mpg %>% select(class, cty)
head(df)

#Q2
df_suv = df %>% filter(class == 'suv')
df_compact = df %>%filter(class == 'compact')
mean(df_suv$cty)
mean(df_compact$cty)

#3
mpq = as.data.frame(ggplot2::mpg)
mpg %>% filter(manufacturer == 'audi') %>%
  arrange(desc(hwy)) %>%
  head(5)

#4
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
mpg %>%
  mutate(total = cty + hwy,
         mean = total /2) %>%
  arrange(desc(mean)) %>%
  head(3)

# 5
#Q1
mpg %>%
  group_by(class) %>%
  summarise(mean_cty = mean(cty))

#Q2
mpg %>%
  group_by(class) %>%
  summarise(mean_cty = mean(cty)) %>%
  arrange(desc(mean_cty)) %>%
  head(3)

#Q3
mpg %>%
  group_by(manufacturer) %>%
  summarise(mean_hwy = mean(hwy)) %>%
  arrange(desc(mean_hwy)) %>%
  head(3)

#Q4
mpg %>%
  filter(class == 'compact') %>%
  group_by(manufacturer) %>%
  summarise(count= n()) %>%
  arrange(desc(count))

#6
fuel = data.frame(fl = c("c", "d", "e", "p", "r"),
                   price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22),
                   stringsAsFactors = F)
#Q1
mpg = as.data.frame(ggplot2::mpg)
mpg = left_join(mpg, fuel, by = 'fl')

#Q2
mpg %>%
  select(model, fl, price_fl) %>%
  head(5)

#분석 도전
midwest = as.data.frame(ggplot2::midwest)

#Q1
midwest = midwest %>%
  mutate(ratio_child = (poptotal-popadults)/poptotal*100)
midwest

#Q2
midwest %>%
  arrange(desc(ratio_child)) %>%
  select(county, ratio_child) %>%
  head(5)

#Q3
midwest = midwest %>%
  mutate(grade = ifelse(ratio_child >= 40, 'large',
                        ifelse(ratio_child >=30, 'middle','small')))

table(midwest$grade)

#Q4
midwest %>%
  mutate(ratio_asian = (popasian/poptotal)*100) %>%
  arrange(ratio_asian) %>%
  select(state, county, ratio_asian)%>%
  head(10)
