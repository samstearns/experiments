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