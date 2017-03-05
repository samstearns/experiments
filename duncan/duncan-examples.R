###############################################################################################
# Examples from Healthcare Risk Adjustment and Predictive Modeling, by Ian Duncan
# This script uses the test dataset accompanying the book, from www.actexmadriver.com
# The test dataset contains 10,000 randomly selected patients from a commercial population
# The 10,000 patients are continuously enrolled for two years
###############################################################################################

# Data Setup ----------------------------------------------------------------------------------
setwd("/Users/sjs/Dropbox/dev/git/experiments/duncan")
duncan.data <- read.csv("modeling_sample_dataset.csv")
summary(duncan.data)

# 8.7: Example of linear regressions ----------------------------------------------------------
hist(allow_future_total)

age.gender.fit <- lm(allow_future_total ~ gender + age, data = data)
summary(age.gender.fit)

prior.cost.fit <- lm(allow_future_total ~ allow_current_total, data = data)
summary(prior.cost.fit)

prior.cost.2.fit <- lm(allow_future_total ~ allow_current_total + non_claimant, data = data)
summary(prior.cost.2.fit)

# Simple Age/Gender model based on HCCs. 
hcc.data <- duncan.data[, c(2, 21, 26, 51:133)]
hcc.fit <- lm(allow_future_total ~ ., data = hcc.data)
summary(hcc.fit)

# 10.3: Example of Logistic Regression to predict likelihood of hospitalization ---------------
admission.table <- table(data$admit_flg_future, data$admit_flg_current)
admission.table

attach(data)

loh.fit = glm(admit_flg_future ~ admit_flg_current + gender + A_OVER64 + Er_visit_flg_current + pcp_visit_cnt_current, data=data, family=binomial)
summary(loh.fit)