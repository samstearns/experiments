###############################################################################
# 7.8 Lab: Non-linear modeling
###############################################################################

library(ISLR)
attach(Wage)

###############################################################################
# 7.8.1 Polynomial Regression and Step Functions
###############################################################################
fit = lm(wage ~ poly(age, 4), data = Wage)
coef(summary(fit))

# Obtain age coefficients directly
fit2 = lm(wage ~ poly(age, 4, raw = T), data = Wage)
coef(summary(fit2))

# Equivalent ways of fitting the model. I() is a wrapper function to expace "^" 
# in formulas. cbind() also serves as a wrapper
fit2a = lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data = Wage)
coef(fit2a)

fit2b = lm(wage ~ cbind(age, age^2, age^3, age^4), data = Wage)

# Create grid of age values for predictions
agelims = range(age)
age.grid = seq(from = agelims[1], to = agelims[2])
preds = predict(fit, newdata = list(age = age.grid), se = TRUE)
se.bands = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2 * preds$se.fit)

# Plot the data and add the fit from the degree 4 polynomial
par(mfrow = c(1,2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(age, wage, xlim = agelims, cex = 0.5, col = "darkgrey")
title("Degree-4 Polynomial", outer = T)
lines(age.grid, pres$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

# Fitted values obtained in either case are identical
preds2 = predict(fit2, newdata = list(age = age.grid), se = TRUE)
max(abs(preds$fit - preds2$fit))

# Determining which degree polynomial to use, via anova / F tests
# Fit five models and sequentially compare the simpler model to the more complex one
fit.1 = lm(wage ~ age, data = Wage)
fit.2 = lm(wage ~ poly(age, 2), data = Wage)
fit.3 = lm(wage ~ poly(age, 3), data = Wage)
fit.4 = lm(wage ~ poly(age, 4), data = Wage)
fit.5 = lm(wage ~ poly(age, 5), data = Wage)

# P-values indicate that cubic or quartic polynomial provides reasonable fit
# Low p-values indicate that more complex model is better fit
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

coef(summary(fit.5))

# Anova also can be used to compare other terms in the models
# Cross-validation provides another choice for choosing the polynomial degree
fit.1 = lm(wage ~ education + age, data = Wage)
fit.2 = lm(wage ~ education + poly(age, 2), data = Wage)
fit.3 = lm(wage ~ education + poly(age, 3), data = Wage)
anova(fit.1, fit.2, fit.3)

# Predict whether an  individual earns more than $250K per year
# Wrapper function I() used to create binary response on the fly
fit = glm(I(wage > 250) ~ poly(age, 4), data = Wage, family = binomial)
preds = predict(fit, newdata = list(age = age.grid), se = T)

# Use a transformation to get confidence intervals
pfit = exp(preds$fit) / (1 + exp(preds$fit))
se.bands.logit = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
se.bands = exp(se.bands.logit) / (1 + exp(se.bands.logit))
preds = predict(fit, newdata = list(age = age.grid), type = "response", se = T)

# Plot the right hand side. This shows age values with $250K salary as marks on to
# of the plot, and below 250 as marks on the bottom of the plot
plot(age, I(wage>250), xlim = agelims, type = "n", ylim = c(0, 0.2))
points(jitter(age), I((wage > 250) / 5), cex = 0.5, pch = "|", col = "darkgrey")
lines(age.grid, pfit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

# Use the cut() function to fit a step function
table(cut(age, 4))
fit = lm(wage ~ cut(age, 4), data = Wage)
coef(summary(fit))

###############################################################################
# 7.8.2 Splines
###############################################################################

library(splines)

# Fit age to wage using a regression spine. Three knots produce spline with
# Six basis funcitons
fit = lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)
pred = predict(fit, newdata = list(age = age.grid), se = T)
plot(age, wage, col = "gray")
lines(age.grid, pred$fit, lwd = 2)
lines(age.grid, pred$fit + 2 * pred$se, lty = "dashed")
lines(age.grid, pred$fit - 2 * pred$se, lty = "dashed")

# Use df option to produce spline with knots at uniform quantiles of data
dim(bs(age, knots = c(25, 40, 60)))
dim(bs(age, df = 6))
# Knots correspond to 25, 50, and 75th percentile of age
attr(bs(age, df = 6), "knots")

# Use the ns() function to fit a natural spline.
fit2 = lm(wage ~ ns(age, df = 4), data = Wage)
pred2  = predict(fit2, newdata = list(age = age.grid), se = T)
lines(age.grid, pred2$fit, col = "red", lwd = 2)

# Fit a smoothing spline using smooth.spline(). Figure 7.8
plot(age, wage, xlim = agelims, cex = 0.5, col = "darkgrey")
title("Smoothing Spline")
# Specify degrees of freedom
fit = smooth.spline(age, wage, df = 16)
# Select smoothness via cross-validation. This yields 6.8 degrees of freedom
fit2 = smooth.spline(age, wage, cv = TRUE)
fit2$df
lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "6.8 DF"), col = c("red", "blue"), lty = 1, lwd = 2, cex = 0.8)

# Use loess() function to perform local regression
plot(age, wage, xlim = agelims, cex = 0.5, col = "darkgrey")
title("Local Regression")
fit = loess(wage ~ age, span = 0.2, data = Wage)
fit2 = loess(wage ~ age, span = 0.5, data = Wage)

lines(age.grid, predict(fit, data.frame(age = age.grid)), col = "red", lwd = 2)
lines(age.grid, predict(fit2, data.frame(age = age.grid)), col = "red", lwd = 2)
legend("topright", legend = c("Span = 0.2", "Span = 0.8"), col = c("red", "blue"), lty = 1, lwd = 2, cex = 0.8)

###############################################################################
# 7.8.3 GAMs
###############################################################################

library(gam)

# Fit a GAM to predict wage, using natural splines of year and age, with education as
# a qualitative predictor. This can be modeled as a linear regression
gam1 = lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = Wage)

# Fit the model using smoothing splines (vs. natural splines)
gam.m3 = gam(wage ~ s(year,4) + s(age,5) + education, data = Wage)

# Produce figure 7.12. Plot recognizes gam.m3 as a gam object, and plots accordingly
par(mfrow = c(1,3))
plot(gam.m3, se = TRUE, col = "blue")

# the plot.gam function can still be used on the generic linear model
plot.gam(gam1, se = TRUE, col = "red")

# The function of year looks linear. Run anova tests to choose the best model
# M2 is the best model based on the results of the anova tests
gam.m1 = gam(wage ~ s(age, 5) + education, data = Wage)
gam.m2 = gam(wage ~ year + s(age, 5) + education, data = Wage)

summary(gam.m3)
anova(gam.m1, gam.m2, gam.m3, test = "F")

# Make predictions on the training set
preds = predict(gam.m2, newdata = Wage)

# Use local regression fits as a building block
gam.lo = gam(wage ~ s(year, df = 4) + lo(age, span = 0.7) + education, data = Wage)
plot.gam(gam.lo, se = TRUE, col = "green")

# Use lo() to create interactions before calling the gam() function
gam.lo.i = gam(wage ~ lo(year, age, span = 0.5) + education, data = Wage)

# plot resulting 2D surface using the akima package
library(akima)
plot(gam.lo.i)

# Fit a logistic regresion GAM using the I() function to create the binary response
gam.lr = gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, data = Wage)
par(mfrow = c(1,3))
plot(gam.lr, se = T, col = "green")

# Check # of high earners by education
table(education, I(wage>250))

# Fit logistic regression excluding the <HS category. These results are more sensible 
# (particularly the chart on the left)
gam.lr.s = gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, data = Wage, subset = (education != "1. < HS Grad"))
plot(gam.lr.s, se = T, col = "green")