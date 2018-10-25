# K-means and clustering
setwd('~/Documents/Martin/Syracuse University/IST707/IST707/Week4')
require(stats)

# Work on the zoo data
zoo <- read.csv('zoo.csv')
cleanZoo <- zoo[, 2:18]

# Run k-means three times with a different centroid each time. 
set.seed(1)
k1 <- kmeans(cleanZoo, 3)
plotcluster(cleanZoo, k1$cluster)

# Calculate the SSE per number of clusters
wss = kmeans(cleanZoo, centers = 1)$tot.withinss
for(i in 2:17){
  wss[i] = kmeans(cleanZoo, centers = i)$tot.withinss
}
sse = data.frame(c(1:17), c(wss))
colnames(sse) <- c('Clusters', 'SSE')

# Visualize the 'elbow' plot to maximize # of clusters while minimizing SSE 
sse %>% ggvis(~Clusters, ~SSE) %>% layer_points(fill := 'blue') %>% layer_lines()

# Create cluster and plot. 
k2 <- kmeans(cleanZoo, 9)
clusplot(cleanZoo, k1$cluster, color = T, shade = F, labels = 0, lines = 0)
