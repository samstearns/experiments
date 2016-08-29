###############################################################################
# R Lab from Chapter 5 - Resampling Methods
###############################################################################
library(ISLR)

###############################################################################
# 5.3.1 The Validation Set Approach
###############################################################################
set.seed (1)

# Use the sample() function to split the set of observations into two halves, 
# by selecting a random subset of 196 observations out of the original 392 observations
train = sample(392, 196)

# Use the subset option in lm() to fit a linear regression using only the 
# observations corresponding to the training set.
lm.fit = lm(mpg ~ horsepower, data = Auto, subset=train)

# Use the predict() function to estimate the response for all 392 observations, 
# and we use the mean() function to calculate the MSE of the 196 observations 
# in the validation set.
attach(Auto)
mean((mpg - predict(lm.fit, Auto))[-train]^2)

# Use the poly() function to estimate the test error for the polynomial and cubic regressions
lm.fit2 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset=train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)

lm.fit3 = lm(mpg ~ poly(horsepower, 3), data = Auto, subset=train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

# If we choose a different training set instead, then we will obtain somewhat 
# different errors on the validation set.
set.seed(2)
train = sample(392,196)

lm.fit = lm(mpg ~ horsepower, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train]^2)

lm.fit2 = lm(mpg ~ poly(horsepower, 2),data=Auto,subset=train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)

lm.fit3 = lm(mpg ~ poly(horsepower, 3), data = Auto, subset=train)
# Results are similar: quadratic function beats linear and cubic functions
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

###############################################################################
# 5.3.2 Leave-One-Out Cross-Validation
###############################################################################

# Use glm to perform linear regression in order to use the cv.glm() function
library(boot) # contains the cv funciton

# glm performs linear regression if the family function is omitted
glm.fit = glm(mpg ~ horsepower, data=Auto)
coef(glm.fit)

cv.err = cv.glm(Auto, glm.fit)
cv.err$delta

# Repeat this procedure for increasingly complex polynomial fits
# Compute cross-validation error for 1-5th order polynomials
cv.error = rep(0,5)
for (i in 1:5) {
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] = cv.glm(Auto,glm.fit)$delta[1]
}

# Results show drop in MSE between linear and quadratic, with limited incremental improvement
cv.error

###############################################################################
# 5.3.3 k-Fold Cross-Validation
###############################################################################

# Use cv.glm to implement k-fold CV
set.seed(17)
cv.error.10 = rep(0,10)

for (i in 1:10) {
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K = 10)$delta[1]
}

#Notice that the computation time is much shorter than that of LOOCV.
cv.error.10

###############################################################################
# 5.3.4 The Bootstrap
###############################################################################

# Returnsan estimate for α based on applying to the observations indexed 
# by the argument index. 
alpha.fn = function(data, index) {
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y) - cov(X,Y)) / (var(X) + var(Y) -2 * cov(X, Y)))
}  

# Estimate alpha using all 100 observations in the Portfolio variable
alpha.fn(Portfolio, 1:100)

# Randomly select 100 observations from the range 1 to 100, with replacement
set.seed (1)
alpha.fn(Portfolio, sample(100, 100, replace=T))

boot(Portfolio, alpha.fn, R=1000)

# Estimating the accuracy of a linear regression model
boot.fn = function(data,index) {
  return(coef(lm(mpg ~ horsepower,data = data, subset = index)))
}

boot.fn(Auto, 1:392)

# Create bootstrap estimates for the intercept and slope terms by randomly 
# sampling from among the observations with replacement
set.seed (1)
boot.fn(Auto, sample(392, 392, replace=T))
boot.fn(Auto, sample(392, 392, replace=T))

# Compute the standard errors of 1,000 bootstrap estimates for the intercept and slope terms.
boot(Auto, boot.fn, 1000)

# Compute the standard errors for the regression coefficients in a linear model.
summary(lm(mpg ~ horsepower, data = Auto))$coef

# Compute the bootstrap standard error estimates and the standard linear 
# regression estimates that result from fitting the quadratic model to the data
# Since this model provides a good fit to the data (Figure 3.8), there is now 
# a better correspondence between the bootstrap estimates and the standard estimates
boot.fn = function(data, index) {
  coefficients(lm(mpg ~ horsepower + I(horsepower^2), data = data, subset=index))
}
set.seed (1)
boot(Auto, boot.fn, 1000)
summary(lm(mpg ~ horsepower + I(horsepower^2), data=Auto))$coef