###############################################################################################
# Examples from Healthcare Risk Adjustment and Predictive Modeling, by Ian Duncan
# This script uses the test dataset accompanying the book, from www.actexmadriver.com
# The test dataset contains 10,000 randomly selected patients from a commercial population
# The 10,000 patients are continuously enrolled for two years
###############################################################################################

# Data Setup ----------------------------------------------------------------------------------
duncan.data <- read.csv("modeling_sample_dataset.csv")
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
admission.table <- table(duncan.data$admit_flg_future, duncan.data$admit_flg_current)
admission.table

loh.fit = glm(admit_flg_future ~ admit_flg_current + gender + A_OVER64 + Er_visit_flg_current + pcp_visit_cnt_current, data=duncan.data, family=binomial)
summary(loh.fit)

###############################################################################################
# Chapter 11: Tree-Based Methods
###############################################################################################
hcc.prospective.data <- duncan.data[, c(2, 21, 32, 51:133)]
hcc.prospective.fit <- lm(allow_future_total ~ ., data = hcc.prospective.data)
summary(hcc.prospective.fit)

hcc.prospective.admit.data <- duncan.data[, c(2, 21, 35, 51:133)]
hcc.prospective.admit.fit <- lm(admit_cnt_future ~ ., data = hcc.prospective.admit.data)
summary(hcc.prospective.admit.fit)

hcc.prior.cost.data <- duncan.data[, c(2, 21, 26, 32, 51:133)]
hcc.prior.cost.fit <- lm(allow_future_total ~ ., data = hcc.prior.cost.data)

# Regression trees to predict prospective cost
library(MASS)
library(tree)

set.seed(1)
train = sample(1:nrow(hcc.prospective.data), nrow(hcc.prospective.data)/2)
tree.prospective = tree(allow_future_total ~., hcc.prospective.data, subset=train)
summary(tree.prospective)

# Use unpruned tree to make predictions on the test set
yhat = predict(tree.prospective, newdata = hcc.prospective.data[-train ,])
hcc.prospective.costs = hcc.prospective.data[-train ,"allowed_future_cost"]
plot(yhat, boston.test)
abline(0,1)

# Test set MSE associated with the regression tree is 25.05. 
# The square root of the MSE is therefore around 5.005, 
# indicating that this model leads to test predictions that 
# are within around $5,005 of the true median home value for the suburb.
mean((yhat-hcc.prospective.costs)^2)
