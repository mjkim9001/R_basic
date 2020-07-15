setwd('c:/Rdata')
install.packages("arules")
install.packages("arulesViz")

require(arules)
library(arulesViz)

data("Groceries")

data(package = "arules")

Groceries

inspect(Groceries[1:10])

summary(Groceries)

head(sort(itemFrequency(Groceries, type="absolute"), decreasing = T),10)
head(sort(itemFrequency(Groceries, type="relative"), decreasing = T),10)

par(mfrow = c(1,2))
itemFrequencyPlot(Groceries, topN = 10, type="absolute")
itemFrequencyPlot(Groceries, topN = 10, type="relative")

apriori(Groceries)
9385*0.1/30

result_rules = apriori(Groceries, parameter = list(support=0.005, confidence=0.5, minlen=2))

summary(result_rules)

inspect(result_rules[1:5])

result_lift = sort(result_rules, by="lift", decreasing = T)
inspect(result_lift[1:5])

result_conf = sort(result_rules, by = "confidence", decreasing = T)
inspect(result_conf[1:5])

milk_rule = subset(result_lift, items %in% "whole milk")
milk_rule
inspect(milk_rule[1:5])

rhs.milk_rule = subset(result_lift, rhs %in% "whole milk")
rhs.milk_rule
inspect(rhs.milk_rule[1:5])

wholemilk_rule = apriori(Groceries, parameter = list(support=0.005,
                                                     confidence=0.5,
                                                     minlen=2), 
                         appearance = list(default='lhs', rhs='whole milk'))
wholemilk_rule = sort(wholemilk_rule, by='life', decreasing = T)
inspect(wholemilk_rule[1:5])

library(arulesViz)
plot(wholemilk_rule[1:10], method="graph", measure = "lift", shading="confidence")
par(mfrow=c(1,1))
