###############################################################################################
# Examples from Healthcare Risk Adjustment and Predictive Modeling, by Ian Duncan
# This script uses the test dataset accompanying the book, from www.actexmadriver.com
# The test dataset contains 10,000 randomly selected patients from a commercial population
# The 10,000 patients are continuously enrolled for two years
###############################################################################################

# Data Setup ----------------------------------------------------------------------------------
duncan_data <- read.csv("modeling_sample_dataset.csv")
attach(duncan.data)
summary(duncan.data)

###############################################################################################
# Chapter 8: Linear Regression Models
###############################################################################################

# 8.7: Example of linear regressions ----------------------------------------------------------
hist(allow_future_total)

###############################################################################################
# Chapter 9: The Generalized Linear Model
###############################################################################################

###############################################################################################
# Chapter 10: Logistic Regression Models
###############################################################################################

# 10.3: Example of Logistic Regression to predict likelihood of hospitalization ---------------
admission_table <- table(duncan_data$admit_flg_future, duncan_data$admit_flg_current)
admission_table

loh_fit = glm(admit_flg_future ~ admit_flg_current + gender + A_OVER64 + Er_visit_flg_current + pcp_visit_cnt_current, data=duncan_data, family=binomial)
summary(loh_fit)

###############################################################################################
# Chapter 11: Tree-Based Methods
###############################################################################################
hcc_prospective_data <- duncan_data[, c(2, 21, 32, 51:133)]
hcc_prospective_fit <- lm(allow_future_total ~ ., data = hcc_prospective_data)
summary(hcc_prospective_fit)

hcc_prospective_admit_data <- duncan_data[, c(2, 21, 35, 51:133)]
hcc_prospective_admit_fit <- lm(admit_cnt_future ~ ., data = hcc_prospective_admit_data)
summary(hcc_prospective_admit_fit)

hcc_prior_cost_data <- duncan_data[, c(2, 21, 26, 32, 51:133)]
hcc_prior_cost_fit <- lm(allow_future_total ~ ., data = hcc_prior_cost_data)
summary(hcc_prior_cost_fit)

# Regression trees to predict prospective cost
library(MASS)
library(tree)

set.seed(1)
train = sample(1:nrow(hcc_prospective_data), nrow(hcc_prospective_data)/2)
tree_prospective = tree(allow_future_total ~., hcc_prospective_data, subset=train)
summary(tree_prospective)

# Use unpruned tree to make predictions on the test set
yhat = predict(tree_prospective, newdata = hcc_prospective_data[-train ,])
hcc_prospective_costs = hcc_prospective_data[-train ,"allowed_future_cost"]
plot(yhat, hcc_prospective_costs)
abline(0,1)