###############################################################################
# Lab 3: PCR and PLS Regression
###############################################################################

library(pls)
set.seed(2)

###############################################################################
# 6.7.1: Principal Components Regression
###############################################################################

# Apply PCR to hitters data to predict salary
# Predictors are standardized. Performs ten-fold cross-validation error
pcr.fit = pcr(Salary ~ ., data = Hitters, scale = TRUE, validation = "CV")

summary(pcr.fit)

# Smallest cross validation error occurs when M = 16 components are used
validationplot(pcr.fit, val.type = "MSEP")

# Perform PCR on training data and evaluate test set performance
set.seed(1)
pcr.fit = pcr(Salary ~ ., data = Hitters, subset = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")

# Lowest cross-validation error occurs when M = 7 components are used.
# Compute Test MSE
pcr.pred = predict(pcr.fit, x[test,], ncomp = 7 )
mean((pcr.pred = y.test) ^ 2)

# Fit PCR on the full data set, using M = 7 components identified by cross-validation
pcr.fit = pcr(y ~ x, scale = TRUE, ncomp = 7)
summary(pcr.fit)

###############################################################################
# 6.7.2: Partial Least Squares
###############################################################################
set.seed(1)
# Lowest cross-validation occurs when M = 2 partial least squares directions are used
pls.fit = plsr(Salary ~ ., data = Hitters, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)

# Evaluate corresponding test set MSE
pls.pred = predict(pls.fit, x[test,], ncomp = 2)

# Perform PLS using full dataset, M = 2
# The % of variance explained by the 2 component PLS fit is almost as much as that explained
# by the final seven-component PCR fit
pls.fit = plsr(Salary ~ ., data = Hitters, scale = TRUE, ncomp = 2)
summary(pls.fit)