# Load required packages
require(pacman)
p_load(dplyr, foreign, RWeka, arules, arulesViz)

setwd('~/Documents/Martin/Syracuse University/IST707/IST707/Week3')

# Load the titanic-train-final.arff file
titanicDat <- read.arff('titanic-train-final.arff')
for(i in 1:ncol(titanicDat)){
  if (class(titanicDat[[i]]) == "numeric"){
    titanicDat[[i]] <- factor(titanicDat[[i]])
  }
}

# 1. Using Apriori
# Set confidence (C) to 0.8 and measurement (T) to Lift (1)
titanicRules <- Apriori(titanicDat, control = Weka_control(C = 0.8, T = 1))

# RWeka Apriori documentation. 
# http://www.dbs.ifi.lmu.de/~zimek/diplomathesis/implementations/EHNDs/doc/weka/associations/Apriori.html

# 2. Market basket analysis
# Load the groceries data set. 
data("Groceries")

# Explore the top 20 items through an item frequency plot. 
itemFrequencyPlot(Groceries, topN = 20, type = "absolute")

# Passing some rules: support = 0.001, confidence = 0.8, top 5 rules.
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))
options(digits = 2)
rules <- sort(rules, decreasing = TRUE, by = 'count')
inspect(rules[1:5])

summary(rules)

# Now to the same with the retail.csv file
retail <- read.transactions('retail.csv', format = 'basket', sep = ',')

# Explore the data set. 
itemFrequencyPlot(retail, topN = 20, type = 'absolute')

rules <- apriori(retail, parameter = list(supp = 0.001, conf = 0.8, maxlen = 3))
rules <- sort(rules, decreasing = TRUE, by = c('lift', 'confidence'))
inspect(rules[1:5])

summary(rules)
