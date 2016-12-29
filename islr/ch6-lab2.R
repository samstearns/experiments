###############################################################################
# Lab 2: Ridge Regression and the Lasso
###############################################################################

library(ISLR)
library(glmnet)

# Ensure that missing values have been removed from the data
Hitters = na.omit(Hitters)

# Use ridge regression and the lasso to predict salary on the hitters data
x = model.matrix(Salary ~ ., Hitters)[,-1]
y = Hitters$Salary

###############################################################################
# 6.6.1: Ridge Regression
###############################################################################

# First fit a ridge regression model. Alpha parameter chooses between ridge 
# and the lasso
# Implement fuction over range from 10^10 to 10^-2
grid = 10 ^ seq(10, -2, length = 100)
ridge.mod = glmnet(x, y, alpha = 0, lambda = grid)

# Vector of ridge regression coefficients, stored in a 20 x 100 matrix
dim(coef(ridge.mod))

# Expect coefficients to be smaller when a large value of lambda is used
ridge.mod$lambda[50]
coef(ridge.mod)[,50]

# In contrast, larger norm of coefficients with smaller value for lambda
ridge.mod$lambda[60]
coef(ridge.mod)[,60]

# Use predict to obstain ridge regressoin coefficients for new value of lambda
predict(ridge.mod, s = 50, type = "coefficients")[1:20,]

# Split samples into training and test set to estimate test error of
# ridge regression and the lasso
set.seed(1)
train = sample(1:nrow(x), nrow(x) / 2)
test = (-train)
y.test = y[test]

# Fit a ridge regression model on training set, and evaluate MSE on the test set
ridge.mod = glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred = predict(ridge.mod, s = 4, newx = x[test,])
mean((ridge.pred - y.test) ^ 2)

# If we had instead simply fit a midel with just an intercept, each test observation
# Could be predicted with the mean of the training observations
mean((mean(y[train]) - y.test) ^ 2)

# Can get same result with ridge regression with very large value of lambda
ridge.pred = predict(ridge.mod, s = 1e10, newx = x[test,])
mean((ridge.pred - y.test) ^ 2)

# Check benefit of performing ridge regression with lambda = 4 vs. 
# Least squares regression
ridge.pred = predict(ridge.mod, s = 0, newx = x[test,], exact = T)
mean((ridge.pred - y.test) ^ 2)
lm(y ~ x, subset = train)
predict(ridge.mod, s = 0, exact = T, type = "coefficients")[1:20,]

# Use cross validation to choose tuning parameter lambda
set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam

# Find test MSE associated with lambda with smallest cross-validation error
ridge.pred = predict(ridge.mod, s = bestlam, newx = x[test,])
mean((ridge.pred - y.test) ^ 2)

# Refit ridge regression on full dataset, using lambda chosen by cross validationp
# As expected, none of the coefficients are zero. Ridge does not do variable selection
out = glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20,]

###############################################################################
# 6.6.2: The Lasso
###############################################################################
lasso.mod = glmnet(x[train,], y[train], alpha = 1, lambda = grid)
# The plot shows that some coefficients will equal zero, depending on the parameter
plot(lasso.mod)

# Perform cross validation and compute associated test error
set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)
bestlam = cv.out$lambda.min
lasso.pred = predict(lasso.mod, s = bestlam, newx = x[test,])

# MSE is similar to ridge regression with lambda chosen by cross validation
mean((lasso.pred - y.test) ^ 2)

out = glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef = predict(out, type = "coefficients", s = bestlam)[1:20, ]

# Lasso model contains only seven variables
lasso.coef