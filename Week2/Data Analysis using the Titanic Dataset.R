# Working on the Kaggel Titanic Dataset

# Set working directory to file where data is held and load. 
setwd("C:/Users/malon/Documents/Syracuse University/IST 707 Data Analytics/Week2")
titanic <- read.csv("train.csv", stringsAsFactors = FALSE)

# Convert integer data into nominal and ordinal data. 
titanic$PassengerId <- factor(titanic$PassengerId)
titanic$Survived <- factor(titanic$Survived)
titanic$Pclass <- ordered(titanic$Pclass)
titanic$Embarked <- factor(titanic$Embarked)

# Loop over the columns to check for NAs. 
for(i in 1:ncol(titanic)){
  column <- colnames(titanic)[i]
  result <- sum(!complete.cases(titanic[[i]]))
  print(paste0(column, ": ", result))
}

# Convert NAs to mean Age to fill missing values. 
titanic$Age[is.na(titanic$Age)] <- mean(titanic$Age, na.rm = TRUE)

# Review number of passengers per class. 
require(dplyr)
titanic2 <- titanic %>% group_by(Pclass) %>% summarize(n = n())
titanic2

max(titanic2$n) - min(titanic2$n)

# Create visualizations.
require(ggplot2)
require(gridExtra)
boxplot <- ggplot(titanic %>% filter(Pclass == 3), aes(y = Fare)) + geom_boxplot()
histogram <- ggplot(titanic %>% filter(Pclass == 3), aes(x = Fare)) + geom_histogram()
grid.arrange(boxplot, histogram, ncol = 2)

# Cross tab survival with point of embarkment.
titanicClean <- titanic %>% filter(Embarked != "") 
crossTab <- table(titanicClean$Embarked, titanicClean$Survived)[2:4, ]

# Aggregate average fare for men and women
titanic %>% group_by(Sex) %>% summarize(avgFare = mean(Fare))

# Data transformation using Fare variable 
# 1. Discretization
fareCut <- cut(titanic$Fare, breaks = c(0, 50, 100, 150, 250, 500), 
    labels = c('cheapest', 'cheap', 'moderate', 'expensive', 'veryExpensive'))
table(fareCut)

# 2. log
ggplot(titanic, aes(Fare, log(Fare))) + geom_point()

# 3. Z-score
titanic %>% mutate(zScore = (Fare-mean(Fare))/sd(Fare)) %>% select(Fare, zScore) %>% head()

# 4. min_max
titanic %>% mutate(minMax = (Fare-min(Fare))/(min(Fare)-max(Fare))) %>% select(Fare, minMax) %>% head()

# Random and systematic sampling
randomSample <- titanic[sample(1:nrow(titanic), 100, replace = FALSE), ]
systemSample <- titanic[seq(1, nrow(titanic), 8.91), ]
