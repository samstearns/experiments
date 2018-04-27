###############################################################################################
# Risk Adjustment Benchmark models
# This script uses the test dataset accompanying Healthcare Risk Adjustment and Predictive Modeling, by Ian Duncan
# The test dataset contains 10,000 randomly selected patients from a commercial population
# The 10,000 patients are continuously enrolled for two years
###############################################################################################

library(tree)

# Data Setup ----------------------------------------------------------------------------------
duncan_data <- read.csv("modeling_sample_dataset.csv")
summary(duncan_data)

# Helper functions ----------------------------------------------------------------------------
# mean((yhat-hcc.prospective.costs)^2)
calculate_mse <- function (model, test_data, actual_values) {
  yhat = predict(model, test_data)
  mse <- mean((yhat - actual_values)^2)
}

# Concurrent models ----------------------------------------------------------------------------
hcc_current_data <- duncan_data[, c(2, 21, 26, 51:133)]
hcc_concurrent_fit <- lm(allow_current_total ~ ., data = hcc_current_data)
summary(hcc_concurrent_fit)
foo <- calculate_mse(hcc_concurrent_fit, hcc_current_data, hcc_current_data$allow_current_total)

# Prospective models ----------------------------------------------------------------------------
age_gender_prospective_fit <- lm(allow_future_total ~ gender + age, data = duncan_data)
summary(age_gender_prospective_fit)

prior_cost_prospective_fit <- lm(allow_future_total ~ allow_current_total, data = duncan_data)
summary(prior_cost_prospective_fit)

prior_cost_prospective_2_fit <- lm(allow_future_total ~ allow_current_total + non_claimant, data = duncan_data)
summary(prior_cost_prospective_2_fit)

# Simple Age/Gender + HCCs
hcc_prospective_data <- duncan_data[, c(2, 21, 32, 51:133)]
hcc_prospective_fit <- lm(allow_future_total ~ ., data = hcc_prospective_data)
summary(hcc_prospective_fit)

hcc_prior_cost_data <- duncan_data[, c(2, 21, 26, 32, 51:133)]
hcc_prior_cost_fit <- lm(allow_future_total ~ ., data = hcc_prior_cost_data)
summary(hcc_prior_cost_fit)

tree_prospective = tree(allow_future_total ~ ., data = hcc_prospective_data)
summary (tree_prospective)

# Prospective Admissions models ----------------------------------------------------------------------------
hcc_prospective_admit_data <- duncan_data[, c(2, 21, 35, 51:133)]
hcc_prospective_admit_fit <- lm(admit_cnt_future ~ ., data = hcc_prospective_admit_data)
summary(hcc_prospective_admit_fit)