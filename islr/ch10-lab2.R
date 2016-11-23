###############################################################################
# 10.5 Lab 2: Clustering
###############################################################################

library(ISLR)

# The tree library is used to construct classification and regression trees
library(tree)

###############################################################################
# 10.5.1 K-Means Clustering
###############################################################################

# simple simulated example in which there truly are two clusters in the
# data: the first 25 observations have a mean shift relative to the next 25
# observations

set.seed(2) # Set a random seed so initial cluster assignments are reproducible
x = matrix(rnorm (50*2), ncol = 2)
x[1:25,1] = x[1:25,1] + 3
x[1:25,2] = x[1:25,2] - 4

# Cluster into two groups
km.out = kmeans(x, 2, nstart = 20)

# Cluster assignments for each cluster
km.out$cluster

# Plot data with each observation colored according to its cluster assignment
plot(x, col = (km.out$cluster + 1) , main = "K-Means Clustering Results with K=2", xlab = "", ylab = "", pch = 20, cex = 2)

# Peform clustering with K= 3
set.seed(4)
km.out = kmeans(x, 3, nstart = 20)
km.out

# Use kmeans(0) with multiple initial cluster assignments
set.seed(3)
km.out = kmeans (x, 3, nstart = 1)
km.out$tot.withinss # total within-cluster sum of squares, seek to minimize with clustering
km.out = kmeans (x, 3, nstart = 20)
km.out$tot.withinss

###############################################################################
# 10.5.2 Hierarchical Clustering
###############################################################################

hc.complete = hclust (dist(x), method = "complete")
hc.average = hclust (dist(x), method = "average")
hc.single = hclust (dist(x), method = "single")

# Plot dendrograms
par(mfrow =c(1,3))
plot(hc.complete, main = "Complete Linkage", xlab= "", sub = "", cex = .9)
plot(hc.average, main = "Average Linkage", xlab= "", sub = "", cex = .9)
plot(hc.single, main = "Single Linkage", xlab = "", sub = "", cex = .9)

# Determine the cluster labels for each observation associated with a given cut of the dendrogram
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
cutree(hc.single, 4)

# Scale the variables before performing hierarchical clustering of the observations
xsc = scale(x)
plot(hclust(dist(xsc), method = "complete"), main = "Hierarchical Clustering with Scaled Features")

x = matrix(rnorm (30*3), ncol =3)
dd = as.dist(1- cor(t(x)))
plot(hclust(dd, method = "complete"), main = "Complete Linkage with Correlation -Based Distance", xlab = "", sub = "")
