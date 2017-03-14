###############################################################################################
# Examples from Healthcare Risk Adjustment and Predictive Modeling, by Ian Duncan
# This script uses the test dataset accompanying the book, from www.actexmadriver.com
# The test dataset contains 10,000 randomly selected patients from a commercial population
# The 10,000 patients are continuously enrolled for two years
###############################################################################################

# Data Setup ----------------------------------------------------------------------------------
setwd("/Users/sjs/dev/git/experiments/duncan")
duncan.data <- read.csv("modeling_sample_dataset.csv")
summary(duncan.data)

# 8.7: Example of linear regressions ----------------------------------------------------------
hist(allow_future_total)

age.gender.prospective.fit <- lm(allow_future_total ~ gender + age, data = data)
summary(age.gender.prospective.fit)

prior.cost.prospective.fit <- lm(allow_future_total ~ allow_current_total, data = data)
summary(prior.cost.prospective.fit)

prior.cost.prospective.2.fit <- lm(allow_future_total ~ allow_current_total + non_claimant, data = data)
summary(prior.cost.prospective.2.fit)

# Simple Age/Gender + HCCs. 
hcc.current.data <- duncan.data[, c(2, 21, 26, 51:133)]
hcc.concurrent.fit <- lm(allow_current_total ~ ., data = hcc.current.data)
summary(hcc.concurrent.fit)

hcc.prospective.data <- duncan.data[, c(2, 21, 32, 51:133)]
hcc.prospective.fit <- lm(allow_future_total ~ ., data = hcc.prospective.data)
summary(hcc.prospective.fit)

hcc.prospective.admit.data <- duncan.data[, c(2, 21, 35, 51:133)]
hcc.prospective.admit.fit <- lm(admit_cnt_future ~ ., data = hcc.prospective.admit.data)
summary(hcc.prospective.admit.fit)


hcc.prior.cost.data <- duncan.data[, c(2, 21, 26, 32, 51:133)]
hcc.prior.cost.fit <- lm(allow_future_total ~ ., data = hcc.prior.cost.data)
summary(hcc.prior.cost.fit)

# 10.3: Example of Logistic Regression to predict likelihood of hospitalization ---------------
admission.table <- table(data$admit_flg_future, data$admit_flg_current)
admission.table

attach(data)

loh.fit = glm(admit_flg_future ~ admit_flg_current + gender + A_OVER64 + Er_visit_flg_current + pcp_visit_cnt_current, data=data, family=binomial)
summary(loh.fit)