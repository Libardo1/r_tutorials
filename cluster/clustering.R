# clustering
# k-Means clustering
data(iris)
iris2 <- iris
str(iris2)
# remove species from the data to cluster
iris2$Species <- NULL
str(iris2)
# Apply kmeans() to iris2, and store the clustering result in kc. The cluster number is set to 3.
(kmeans.result <- kmeans(iris2, 3))
# Compare the Species label with the clustering result
table(iris$Species, kmeans.result$cluster)
plot(iris2[c("Sepal.Length", "Sepal.Width")], col = kmeans.result$cluster)
# k-Medoids clustering
# The k-medoids clustering is very similar to k-means, and the major difference between them is that: while a cluster is represented with its center 
# in the k-means algorithm, it is represented with the object closest to the center of the cluster in the k-medoids clustering
library(fpc)
pamk.result <- pamk(iris2)
# number of clusters
pamk.result$nc
# check clustering against real species
table(pamk.result$pamobject$clustering, iris$Species)
layout(matrix(c(1,2),1,2)) # 2 graphs per page
plot(pamk.result$pamobject)
pam.result <- pam(iris2, 3)
table(pam.result$clustering, iris$Species)
layout(matrix(c(1,2),1,2)) # 2 graphs per page
plot(pam.result)
layout(matrix(1)) # change back to one graph per page
# Hierarchical clustering
# Draw a sample of 40 records from iris data, and remove variable Species
idx <- sample(1:dim(iris)[1], 40)
irisSample <- iris[idx,]
irisSample$Species <- NULL
str(irisSample)
# Hierarchical clustering
hc <- hclust(dist(irisSample), method="ave")
plot(hc, hang = -1, labels=iris$Species[idx])
# Density-based clustering
# The idea of density-based clustering is to group objects into one cluster 
# if they are connected to one another by densely populated area.
library(fpc)
iris2 <- iris[-5] # remove class tags
ds <- dbscan(iris2, eps=0.42, MinPts=5)
# compare clusters with original class labels
table(ds$cluster, iris$Species)
plot(ds, iris2)
plot(ds, iris2[c(1,4)])
plotcluster(iris2, ds$cluster)
# create a new dataset for labeling
set.seed(435)
idx <- sample(1:nrow(iris), 10)
newData <- iris[idx,-5]
newData <- newData + matrix(runif(10*4, min=0, max=0.2), nrow=10, ncol=4)
# label new data
myPred <- predict(ds, iris2, newData)
# check the labels assigned to new data
plot(iris2[c(1,4)], col=1+ds$cluster)
points(newData[c(1,4)], pch="*", col=1+myPred, cex=3)
# check cluster labels
table(myPred, iris$Species[idx])