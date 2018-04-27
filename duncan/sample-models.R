###############################################################################################
# Risk Adjustment Benchmark models
# This script uses the test dataset accompanying Healthcare Risk Adjustment and Predictive Modeling, by Ian Duncan
# The test dataset contains 10,000 randomly selected patients from a commercial population
# The 10,000 patients are continuously enrolled for two years
###############################################################################################

library(tree)

# Data Setup ----------------------------------------------------------------------------------
duncan.data <- read.csv("modeling_sample_dataset.csv")
summary(duncan.data)

# Helper functions ----------------------------------------------------------------------------
# mean((yhat-hcc.prospective.costs)^2)
CalculateMSE <- function (model, test.data, actual.values) {
  yhat = predict(model, test.data)
  mse <- mean((yhat - actual.values)^2)
}

# Concurrent models ----------------------------------------------------------------------------
hcc.current.data <- duncan.data[, c(2, 21, 26, 51:133)]
hcc.concurrent.fit <- lm(allow_current_total ~ ., data = hcc.current.data)
summary(hcc.concurrent.fit)
foo <- CalculateMSE(hcc.concurrent.fit, hcc.current.data, hcc.current.data$allow_current_total)

# Prospective models ----------------------------------------------------------------------------
age.gender.prospective.fit <- lm(allow_future_total ~ gender + age, data = duncan.data)
summary(age.gender.prospective.fit)

prior.cost.prospective.fit <- lm(allow_future_total ~ allow_current_total, data = duncan.data)
summary(prior.cost.prospective.fit)

prior.cost.prospective.2.fit <- lm(allow_future_total ~ allow_current_total + non_claimant, data = duncan.data)
summary(prior.cost.prospective.2.fit)

# Simple Age/Gender + HCCs
hcc.prospective.data <- duncan.data[, c(2, 21, 32, 51:133)]
hcc.prospective.fit <- lm(allow_future_total ~ ., data = hcc.prospective.data)
summary(hcc.prospective.fit)

hcc.prior.cost.data <- duncan.data[, c(2, 21, 26, 32, 51:133)]
hcc.prior.cost.fit <- lm(allow_future_total ~ ., data = hcc.prior.cost.data)
summary(hcc.prior.cost.fit)

tree.prospective =tree(allow_future_total ~ ., data = hcc.prospective.data)
summary (tree.prospective)

# Prospective Admissions models ----------------------------------------------------------------------------
hcc.prospective.admit.data <- duncan.data[, c(2, 21, 35, 51:133)]
hcc.prospective.admit.fit <- lm(admit_cnt_future ~ ., data = hcc.prospective.admit.data)
summary(hcc.prospective.admit.fit)