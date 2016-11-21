###############################################################################
# Lab 1 from Chapter 6 - Subset Selection Methods
###############################################################################

#  Apply the best subset selection approach to the Hitters data. 
# We wish to predict a baseball player’s Salary on the basis of various statistics 
# associated with performance in the previous year.
library(ISLR)
fix(Hitters)
names(Hitters)

# Remove rows with missing salaries. 
dim(Hitters)
sum(is.na(Hitters$Salary))

Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

# regsubsets() function (part of the leaps library) performs best subset 
# selection by identifying the best model that contains a given number of predictors, 
# where best is quantified using RSS
library(leaps)
regfit.full = regsubsets(Salary ~. , Hitters)

# Output best set of variables for each model size
summary(regfit.full)

# Up to eight variables are used by default. Fit up to 10 variable model
regfit.full = regsubsets(Salary ~. , data=Hitters, nvmax=19)
reg.summary = summary(regfit.full)
names(reg.summary)

# R-sq increases monotonically as more variables are added
reg.summary$rsq

# Plot results to help determine which model to select
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of Variables", ylab="RSS", type="l")
plot(reg.summary$adjr2,xlab="Number of Variables", ylab="Adjusted RSq", type="l")

# Plot red dot to indicate model with largest adjusted R-Sq
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col="red", cex=2, pch=20)

# plot the Cp and BIC statistics, and indicate the models with the smallest statistic using which.min().
plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col="red", cex=2, pch=20)
which.min(reg.summary$bic)
plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
points(6, reg.summary$bic[6], col="red", cex=2, pch=20)

# regsubsets() function has a built-in plot() command which can be used 
# to display the selected variables for the best model with a given number 
# of predictors, ranked according to the BIC, Cp, adjusted R2, or AIC
# To find out more about this function, type ?plot.regsubsets.
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
coef(regfit.full, 6)

###############################################################################
# 6.5.2 Forward and Backward Stepwise Selection
###############################################################################

# Use the regsubsets() function to perform forward stepwise or backward stepwise 
# selection, using the argument method="forward" or method="backward".

regfit.fwd = regsubsets( Salary ~ ., data = Hitters, nvmax=19, method = "forward")
summary(regfit.fwd)
regfit.bwd = regsubsets( Salary ~ ., data = Hitters, nvmax=19, method = "backward")
summary(regfit.bwd)

###############################################################################
# Choosing Among Models Using the Validation Set Approach and Cross-Validation
###############################################################################

# Create training and test set
set.seed (1)
train = sample(c(TRUE,FALSE), nrow(Hitters), rep=TRUE)
test =(!train)

# Perform best subset selection on the training dataset
regfit.best = regsubsets( Salary ~ ., data = Hitters[train,], nvmax=19)

# Compute the validation set error for the best model of each model size. 
# First make a model matrix from the test data.
test.mat = model.matrix(Salary ~ ., data=Hitters[test,])

val.errors = rep(NA, 19)
for(i in 1:19) {
  # Extract the coefficients from regfit.best for the best model of that size
  coefi = coef(regfit.best, id = i)
  # Multiply coefficients into the appropriate columns of the test model matrix to form the predictions
  pred = test.mat[, names(coefi)] %*% coefi
  # Compute the test MSE
  val.errors[i] = mean((Hitters$Salary[test] - pred) ^2)
}

# The best model (lowest MSE) has 10 variables
which.min(val.errors)
coef(regfit.best, 10)

# Capture previous steps in a function to be reused

predict.regsubsets <- function (object, newdata, id, ...) {
  form = as.formula(object$call [[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id=id)
  xvars = names(coefi)
  mat[, xvars] %*% coefi
}
  
# Perform best subset selection on the full data set, and select the best ten-variable model 
# Use full dataset to obtain more accurate coefficients
regfit.best = regsubsets(Salary ~ ., data = Hitters, nvmax = 19)

# Choose among the models of different sizes using cross-validation. 
# Perform best subset selection within each of the k training sets. 
k = 10
set.seed(1)
# Allocate each observation to one of the folds
folds = sample(1:k, nrow(Hitters), replace=TRUE)

# Create matrix to store the results
cv.errors = matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

for (j in 1:k) {
  best.fit = regsubsets(Salary ~ ., data = Hitters[folds != j, ], nvmax = 19)
  for (i in 1:19) {
    pred = predict(best.fit, Hitters[folds==j,], id=i)
    cv.errors[j,i] = mean((Hitters$Salary[folds == j] - pred) ^2)
  }
}

# Average over the columns in order to obtain a vector for which 
# the jth element is the cross-validation error for the j-variable model.
mean.cv.errors = apply(cv.errors, 2, mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors, type="b")

# Cross validation selects an 11-variable model
# Perform best subset selection on the full data set in order to obtain the 11-variable model
reg.best = regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
coef(reg.best, 11)