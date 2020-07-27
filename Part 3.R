## Association rules

# load in the dataset

supermarket1 <- read.transactions("C:/Users/leonb/Downloads/Supermarket_Sales_Dataset II.csv", sep = ",", header = TRUE)
head(supermarket1)

transactions <- read.transactions("C:/Users/leonb/Downloads/Supermarket_Sales_Dataset II.csv", sep = ",", header = TRUE)
head(transactions)
# We first we install the required arules library 
#
install.packages("arules")

# Loading the arules library
#
library(arules)


class(supermarket1)


# Previewing our first 5 transactions
#
inspect(transactions)

items <- as.data.frame(itemLabels(transactions))
colnames(items) <- "Item"
head(items, 10)


summary(transactions)


itemFrequency(supermarket1[, 8:10],type = "absolute")
round(itemFrequency(supermarket1[, 8:10],type = "relative")*100,2)

# Displaying top 10 most common items in the transactions dataset 
# and the items whose relative importance is at least 10%
# 
par(mfrow = c(1, 2))

# plot the frequency of items
itemFrequencyPlot(transactions, topN = 10,col="darkgreen")
itemFrequencyPlot(transactions, support = 0.1,col="darkred")

# Building a model based on association rules 
# using the apriori function 
# ---
# We use Min Support as 0.001 and confidence as 0.8
# ---
# 
rules <- apriori (transactions, parameter = list(supp = 0.001, conf = 0.8))
rules

# We use measures of significance and interest on the rules, 
# determining which ones are interesting and which to discard.
# ---
# However since we built the model using 0.001 Min support 
# and confidence as 0.8 we obtained 73 rules.
# However, in order to illustrate the sensitivity of the model to these two parameters, 
# we will see what happens if we increase the support or lower the confidence level
# 

# Building a apriori model with Min Support as 0.002 and confidence as 0.8.
rules2 <- apriori (transactions,parameter = list(supp = 0.002, conf = 0.8)) 

# Building apriori model with Min Support as 0.002 and confidence as 0.6.
rules3 <- apriori (transactions, parameter = list(supp = 0.001, conf = 0.6)) 

rules2

rules3

# In our first example, we increased the minimum support of 0.001 to 0.002 and model 
#rules went from 73 to only 2 This would lead us to understand that using a high 
#level of support can make the model lose interesting rules. In the second example, 
#we decreased the minimum confidence level to 0.6 and the number of model rules went
#from 73 to 543 This would mean that using a low confidence level increases the 
#number of rules to quite an extent and many will not be useful.

summary(rules)

# Observing rules built in our model i.e. first 5 model rules
# ---
# 
head(rules)


rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1:5])

milk <- subset(rules, subset = rhs %pin% "milk")

# Then order by confidence
milk <- sort(milk, by="confidence", decreasing=TRUE)
inspect(milk[1:5])

# What if we wanted to determine items that customers might buy 
# who have previously bought milk?
# ---
# 
# Subset the rules
milk <- subset(rules, subset = lhs %pin% "milk")

# Order by confidence
milk<-sort(milk, by="confidence", decreasing=TRUE)

# inspect top 5
inspect(milk[15:19])


