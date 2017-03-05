###############################################################################################
# Examples from Healthcare Risk Adjustment and Predictive Modeling, by Ian Duncan
###############################################################################################

# Data Setup ----------------------------------------------------------------------------------
setwd("/Users/sjs/Dropbox/dev/git/experiments/duncan")
data <- read.csv("modeling_sample_dataset.csv")
summary(data)

# 10.3: Example of Logistic Regression to predict likelihood of hospitalization ---------------
admission.table <- table(data$admit_flg_future, data$admit_flg_current)
admission.table

attach(data)

loh.fit = glm(admit_flg_future ~ admit_flg_current + gender + A_OVER64 + Er_visit_flg_current + pcp_visit_cnt_current, data=data, family=binomial)
summary(loh.fit)