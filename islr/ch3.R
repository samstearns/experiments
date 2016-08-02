###############################################################################
# R Lab from Chapter 3 - Linear Regression
###############################################################################

library(ISLR)
library(MASS)

# Load Boston dataset, which records medv (median house value) for 506 neighborhoods around Boston
fix(Boston)
names(Boston)
attach(Boston)

###############################################################################
# 3.6.2 Simple Linear Regression
###############################################################################

# Regress median value on percent of HHs with low socioeconomic status
lm.fit=lm(medv ~ lstat)
summary(lm.fit)

# Obtain confidence interval around coefficient estimates
confint(lm.fit)

# Produce confidence intervals and prediction intervals for medv given lstat
predict(lm.fit, data.frame(lstat=(c(5,10,15))), interval ="confidence")
predict(lm.fit, data.frame(lstat=(c(5,10,15))), interval ="prediction")

# Plot variables with least squares regression line
plot(lstat, medv)
abline(lm.fit)

# Plotting options
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red") > plot(lstat,medv,col="red")
plot(lstat,medv,pch=20) 
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)

# diagnostic plots
par(mfrow=c(2,2))
plot(lm.fit)

# Residual plots show some evidence of non-linearity
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

# Compute leverage statistics
plot(hatvalues (lm.fit))
which.max(hatvalues (lm.fit))
  
###############################################################################
# 3.6.3 Multiple Regression
###############################################################################

# Fit multiple regression model using least squares
lm.fit=lm(medv ~ lstat + age,data=Boston)
summary(lm.fit)

# Regress on all 13 predictors
lm.fit=lm(medv ~ . ,data=Boston)

# Access components of the summary lm.fit object
?summary.lm
summary(lm.fit)$r.sq
summary(lm.fit)$sigma

# Use car package to calculate variance inflation factors
library(car)
vif(lm.fit)

# Perform regression using all variables but one
lm.fit1=lm(medv ~.-age,data=Boston)
summary(lm.fit1)
lm.fit2=lm(medv ~.-age -indus,data=Boston)
summary(lm.fit1)$r.sq
summary(lm.fit2)$r.sq

# Alternatively, use the update function to remove a factor
lm.fit1=update(lm.fit, ~ .-age)

###############################################################################
# 3.6.4 Interaction terms
###############################################################################

# Shorthand for lstat+age+lstat:age.
summary(lm(medv ~ lstat:black,data=Boston))

# Shorthand for lstat+age+lstat:age.
summary(lm(medv ~ lstat*age,data=Boston))

###############################################################################
# 3.6.5 Nonlinean transformations of the predictors
###############################################################################

# Square lstat. The "I" function is used becuase ^ has a special meaning in formuals
lm.fit2=lm(medv ~ lstat+I(lstat^2))
summary(lm.fit2)
lm.fit=lm(medv ~ lstat)

# Use anova to quantify extent to which quadratic fit is superior to linear fit

# anova performs a hypothesis test comparing the two models. 
# The null hypothesis is that the two models fit the data equally well, and 
# the alternative hypothesis is that the full model is superior. 
# Here the F-statistic is 135 and the associated p-value is virtually zero. 
# This provides very clear evidence that the model containing the predictors lstat and lstat2 is 
# far superior to the model that only contains the predictor lstat. 
anova(lm.fit ,lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)

# Use poly function to create fifth order fit
lm.fit5=lm(medv ~ poly(lstat ,5))
summary(lm.fit5)

# Log transformation
summary(lm(medv ~ log(rm),data=Boston))

###############################################################################
# 3.6.6 Qualitative Predictors
###############################################################################
library(ISLR)
names(Carseats)

# Build multiple regression model that includes all values and some interaction terms
# Qualitative variables like ShelveLoc are automatically converted to dummy variables
lm.fit=lm(Sales ~ . + Income:Advertising + Price:Age,data=Carseats)
summary(lm.fit)

# Contrast function returns coding used for the dummy variables
attach(Carseats)
contrasts (ShelveLoc)

###############################################################################
# 3.6.7 Writing functions
###############################################################################
LoadLibraries = function (){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.") 
}

LoadLibraries()