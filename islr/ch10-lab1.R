###############################################################################
# 10. Lab 1: Principal Components Analysis
###############################################################################

states = row.names(USArrests)
names(USArrests)

# Variables have vastly different means
apply(USArrests, 2, mean)
apply(USArrests, 2, var)

# Need to scale variables before peforming PCA, because Assualt has largest mean and variance
# pr comp centers variables to have 0 mean. Scale ensures standard deviation = 1
pr.out = prcomp(USArrests, scale = TRUE)

# Inspect outputs. Center and scale correspond to mean and sd, prior to scaling
names(pr.out)
pr.out$center
pr.out$scale

# Rotation matrix includes principal component loadings. Each colum corresponds to vector
# Four distinct principal components. In general, there are min(n-1, p) components in an n X p dataset
pr.out$rotation

# Columns have principal component score vectors
dim(pr.out$x)

# Plot the first two principal components
biplot(pr.out, scale=0)

# Reproduce figure 10.1
pr.out$rotaion = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale=0)

# Access standard deviation of each compnent and calculate variance
pr.out$sdev
pr.var = pr.out$sdev ^ 2
pr.var

# Compute proportion of variance explained by each component
pve = pr.var / sum(pr.var)

# Plot PVE explained by each component as well as cumulative PVE
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b')
plot(cumsum(pve), xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b')