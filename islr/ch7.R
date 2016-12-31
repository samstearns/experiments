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

###############################################################################
# 7.8.1 GAMs
###############################################################################