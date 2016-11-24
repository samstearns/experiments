###############################################################################
# 9.6 Lab: Support Vector Machines
###############################################################################

# Generate two-dimensional example data
set.seed(1)
x = matrix(rnorm(20*2), ncol = 2)
y = c(rep(-1,10), rep(1,10))
x[y==1,] = x[y==1,] + 1

# Check whether the classes are linearly separable
plot(x, col=(3-y))

# Fit the SVM classifier. Encode the response as a factor, to enable classification
dat  = data.frame(x = x, y = as.factor(y))
library(e1071)
svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)

# Plot the classifier
plot(svmfit, dat)

# find identities of support vectors
svmfit$index
summary(svmfit)

# Fit using a smaller value of the cost parameter
svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 0.1, scale = FALSE)
plot(svmfit, dat)
summary(svmfit)