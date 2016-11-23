###############################################################################
# 10.6 Lab 3: NCI60 Example
###############################################################################

library(ISLR)
# NCI is microarray data, with 6830 measurements on 64 cell lines
nci.labs = NCI60$labs
nci.data = NCI60$data

# Examine the cancer types for the cell lines
nci.labs[1:4]
table(nci.labs)

###############################################################################
# 10.6.1 PCI on the NCI60 Data
###############################################################################

pr.out = prcomp(nci.data, scale = TRUE)

# Assign a distinct color to each element of a numeric vector
# Rainbow takes positive integer and returns vector of distinct colors
Cols = function(vec) {
  cols = rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

# Plot the principal component vectors
# Cell lines corresponding to a single cancer type tend to have similar values on
# the first few principal component score vectors. This indicates that cell lines
# of the same type have similar gene expression levels
par(mfrow = c(1,2))
plot(pr.out$x[, 1:2], col = Cols(nci.labs), pch = 19, xlab = "Z1", ylab = "Z2")
plot(pr.out$x[, 1:3], col = Cols(nci.labs), pch = 19, xlab = "Z1", ylab = "Z3")

# Obtain summary of proportion of variance explained 
summary(pr.out)
plot(pr.out)

# Plot PVE of each component and cumulative PVE
# The first 7 components explain 40% of variance
# The "elbow" after the 7th component suggests this is a good cutoff
pve = 100 * pr.out$sdev ^ 2 / sum(pr.out$sdev^2)
par(mfrow = c(1,2))
plot(pve, type = 'o', ylab = "PVE", xlab = "Principal Component", col = "blue")
plot(cumsum(pve), type = 'o', ylab = "Cumulative PVE", xlab = "Principal Component", col = "brown3")

###############################################################################
# 10.6.2 Clustering the NCI60 Data
###############################################################################

# Standardize the data to have mean 0 and s.d. 1
sd.data = scale(nci.data)

# Hierarchical clustering using complete, single, and average linkages
# Choice of linkages affects results. Single linkage tens to yield "trailing" clusters
# Very large clusters where individual observations attach one-by one
par(mfrow = c(3,1))
data.dist = dist(sd.data)
plot(hclust(data.dist), labels = nci.labs, main = "Complete Linkage", xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method = "average"), labels = nci.labs, main = "Average Linkage", xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method = "single"), labels = nci.labs, main = "Single Linkage", xlab = "", sub = "", ylab = "")

# Cut the dendrogram at the height that will yield four clusters
hc.out = hclust(dist(sd.data))
hc.clusters = cutree(hc.out, 4)
table(hc.clusters, nci.labs)
par(mfrow = c(1,1))
plot(hc.out, labels = nci.labs)
abline(h = 139, col = "red")

# Summarize the object
hc.out

# Perform K-Means clustering with K = 4
set.seed(2)
km.out = kmeans(sd.data, 4, nstart = 20)
km.clusters = km.out$cluster
table(km.clusters, hc.clusters)

# Perform hierarchical clustering on first pca score vectors
hc.out = hclust(dist(pr.out$x[,1:5]))
plot(hc.out, labels = nci.labs, main = "Hier. Clust on First Five Score Vectors")
table(cutree(hc.out, 4), nci.labs)