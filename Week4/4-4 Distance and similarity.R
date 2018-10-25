# Calculate the Euclidean distance, Manhattan distance, and cosine similarity between the two tuples. 
a <- c(22, 1, 42, 10)
b <- c(20, 0, 36, 8)

# Euclidean distance
dist(rbind(a, b))
eucl.dist <- function(a, b) sqrt(sum((a - b ) ^ 2))
eucl.dist(a, b)

# Manhattan distance
manh.dist <- function(a, b){
  dist <- abs(a - b)
  dist <- sum(dist)
  return(dist)
}
manh.dist(a, b)

# Cosine similarity
library(tcR)
cosine.similarity(a, b)
