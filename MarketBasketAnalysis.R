# initiate necessary packages
library(arules); library(arulesViz); library(tidyverse)

#load the dataset (this dataset exist by default in arules package)
data(Groceries)

#explore the data
Groceries # 9835 transaction with 169 items
glimpse(Groceries)
Groceries = data("Groceries")

# observing top items
itemFrequencyPlot(Groceries, support = 0.0, cex.names=0.8, 
                  type = "relative", horiz = TRUE, col = "steelblue2", las = 1, topN=5,
                  xlab = paste("Proportion of Market Baskets Containing Item"))

# market basket analysis with support 0.001 confidence 0.001 (just to show the impact of scales)
rules2 = apriori(Groceries,parameter = list(support = 0.001, confidence = 0.001))
summary(rules2) # 41100 rules generated

# I am gonna move forward with more prudent subset selections
# market basket analysis with support 0.01 confidence 0.01
rules1 = apriori(Groceries,parameter = list(support = 0.01, confidence = 0.01))
summary(rules1) # 610 rules generated

# observe results
x = inspect(head(sort(rules1,by ="lift"),10))
x = x[x$count!=0,]
head(x[order(x$lift,x$support, decreasing = T),])

x = inspect(rules1[1:100])
x[order(x$lift,x$support, decreasing = T),]


# conducting same analysis with (minlen=2)
rules3 = apriori(Groceries,parameter = list(support = 0.01, confidence = 0.01
                                            ,minlen=2))
summary(rules3)
x = inspect(rules1[1:300])

# evaluate results based on lift support and confidence metrics
head(x[order(x$lift,x$support, decreasing = T),])
head(x[order(x$confidence, decreasing = T),])

# analyzing specific pairs = let's assess soda and whole milk!
rules4 = apriori(Groceries,parameter = list(support = 0.01, confidence = 0.01 ,minlen=2, maxlen=2)
                ,appearance = list(lhs='soda',rhs='whole milk'))
inspect(rules4)

# analyzing most likely preference for a specific product
# this time I also added the maxlen to fix the search with only pairs
rules5=apriori(Groceries,parameter=list(support=0.01,confidence=0.01, minlen=2, maxlen=2),
               appearance = list(lhs= 'yogurt'))
inspect(head(sort(rules5,by ="confidence"),10))
inspect(head(sort(rules5,by ="lift"),10))

# visualisation of association rules
library(RColorBrewer)
plot(rules3,control=list(jitter=2, col = rev(brewer.pal(9, 
                  "Greens")[4:9])),shading = "lift")

# lets assume you are the trade marketing manager and try to measure your 
 # product's best associates
# for the sake of simplicity I move forward with yogurt example
rules6=apriori(Groceries,parameter=list(support=0.01,confidence=0.01, minlen=2, maxlen=2))

yogurt_only = subset(rules6,subset=rhs %pin% 'yogurt')
inspect(yogurt_only)

yogurt_sorted = head(sort(yogurt_only,decreasing=T,by='lift'),10)
inspect(yogurt_sorted)

# plotting the relationships with yogurt
plot(yogurt_sorted, method="graph", shading = "lift")









