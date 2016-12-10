###############################################################################
# 9.6 Lab: Support Vector Machines
###############################################################################

###############################################################################
# 9.6.1 Support Vector Classifier
###############################################################################

# Generate two-dimensional example data
set.seed(1)
x = matrix(rnorm(20*2), ncol = 2)
y = c(rep(-1,10), rep(1,10))
x[y==1,] = x[y==1,] + 1

# Check whether the classes are linearly separable
plot(x, col=(3-y))

# Fit the SVM classifier. Encode the response as a factor, to enable classification
# instead of regression
# Create a data frame with the response coded as a factor
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

# A smaller cost parameter generates more support vectors
svmfit$index
summary(svmfit)

# Use tune function to perform cross-validation
set.seed(1)
tune.out = tune(svm, y~., data = dat, kernel = "linear",
                ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

# access best model
bestmod = tune.out$best.model
summary(bestmod)

# Use predict function to predict class label on a set of test observations
# Use best model identified in cross-validation
xtest = matrix(rnorm(20*2), ncol=2)
ytest = sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,] = xtest[ytest==1,] + 1
testdat = data.frame(x = xtest, y = as.factor(ytest))

ypred = predict(bestmod, testdat)
table(predict = ypred, truth = testdat$y)

# Repredict with different cost function. One additional observation is misclassified
svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = .01, scale = FALSE)
ypred = predict(svmfit, testdat)
table(predict = ypred, truth = testdat$y)

# Consider situation where classes are linearly separable
# Find a separating hyperplane using the SVM function. First
# Futher separate the two classes so they are linearly separable
x[y==1,] = x[y==1,] + 0.5
plot(x, col = (y+5)/2, pch = 19)

# Fit the classifer using large cost value to prevent mis-classifications
# No training errors were made and only three support vectors were used
dat = data.frame(x=x, y = as.factor(y))
svmfit = svm(y ~ ., data = dat, kernel="linear", cost = 1e5)
summary(svmfit)
plot(svmfit, dat)

# Use a smaller value of cost
# One training observation is misclassified. But the margin is greater
svmfit = svm(y ~ ., data = dat, kernel="linear", cost = 1)
summary(svmfit)
plot(svmfit, dat)

###############################################################################
# 9.6.2 Support Vector Machine
###############################################################################

# First generate data with nonlinear class boundary
set.seed(1)
x = matrix(rnorm(200*2), ncol = 2)
x[1:100,] = x[1:100,] + 2
x[101:150,] = x[101:150,] - 2
y = c(rep(1, 150), rep(2, 50))
dat = data.frame(x=x, y = as.factor(y))
plot(x, col=y)

# Randomly split into training and test groups
train = sample(200, 100)
svmfit = svm(y ~ ., data = dat[train,], kernel = "radial", gamma = 1, cost = 1)
plot(svmfit, dat[train,])
summary(svmfit)

# Increase cost to reduce training errors. 
# But more irregular boundary may be at risk of overfitting
svmfit = svm(y ~ ., data = dat[train,], kernel = "radial", gamma = 1, cost = 1e5)
plot(svmfit, dat[train,])

# Perform cross validation to select best choice of gamma and cost
set.seed(1)
tune.out = tune(svm, y ~ ., 
                data = dat[train,], 
                kernel = "radial",
                ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                              gamma = c(0.5, 1, 2, 3, 4)))

# Best performance involves cost = 1 and gamma = 2
summary(tune.out)

# Apply predictions using best parameters
# Subset the data frame using -train as an index set
# TODO: This does not work. Error in predict method
table(true = dat[-train, "y"], pred = predict(tune.out$bestmodel, newdata = dat[-train,]))

###############################################################################
# 9.6.3 ROC Curves
###############################################################################

library(ROCR)

# Function to plot ROC curve given vector with numerical score pred and class labels truth
rocplot <- function(pred, truth, ...) {
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf, ...)
}

svmfit.opt = svm(y ~ ., data = dat[train,], kernel = "radial", gamma = 2, cost = 1, decision.values = T)
predict(svmfit.opt, dat[train,])

fitted = attributes(predict(svmfit.opt, dat[train,], decision.values = TRUE))$decision.values

# Produce the ROC plot
par(mfrow=c(1,2))
rocplot(fitted, dat[train, "y"], main = "Training Data")

# Increase gamma to produce more flexible fit and increase accuracy
svmfit.flex = svm(y ~ ., data = dat[train,], kernel = "radial", gamma = 2, cost = 50, decision.values = T)
fitted = attributes(predict(svmfit.flex, dat[train,], decision.values = TRUE))$decision.values
rocplot(fitted, dat[train, "y"], add = T, col="red")

# Compute ROC curves on the test data
fitted = attributes(predict(svmfit.opt, dat[-train,], decision.values = T))$decision.values
rocplot(fitted, dat[-train, "y"], main = "Test Data")
fitted = attributes(predict(svmfit.flex, dat[-train,], decision.values = T))$decision.values
rocplot(fitted, dat[-train, "y"], add = T, col="red")

###############################################################################
# 9.6.4 SVM with Multiple Classes
###############################################################################

# If the response has > 2 levels, svm() will perform multi-class classification
# Generate a third class of observations
set.seed(1)
x = rbind(x, matrix(rnorm(50*2), ncol = 2))
y = c(y, rep(0, 50))
x[y==0, 2] = x[y==0, 2] + 2
dat = data.frame(x=x, y = as.factor(y))
par(mfrow = c(1,1 ))
plot(x, col = (y+1))

# Fit an SVM to the data
svmfit = svm(y ~ ., data = dat, kernel = "radial", cost = 10, gamma = 1)
plot(svmfit, dat)

###############################################################################
# 9.6.5 Application to Gene Expression Data
###############################################################################

# Examine the Khan data set, which consists of tissue samples for four types of 
# small round blue cell tumors. Gene expression measurements are available
# Examine the datasets. Measurements for 2,308 genes. Training = 63 observations.
# Test = 20 observations
library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)

# Create support vector approach to predict Cancer subtype
# There is a large # of features relative to observations. Use a linear kernel
# because additional flexibility from polynomial, radial kernel is unnecessary
dat = data.frame(x = Khan$xtrain, y = as.factor(Khan$ytrain))
out = svm(y ~ ., data = dat, kernel = "linear", cost = 10)

# There are no training errors. This is not suprising, given the large # of variables
# vs observations makes it easier to find hyperplanes that separate the classes
summary(out)

# Run on test observations. Cost = 10 leads to two test set errors
dat.te = data.frame(x = Khan$xtest, y = as.factor(Khan$ytest))
pred.te = predict(out, newdata = dat.te)
table(pred.te, dat.te$y)