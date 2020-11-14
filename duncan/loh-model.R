###############################################################################################
# LOH Models
# This script uses the test dataset accompanying Healthcare Risk Adjustment and Predictive Modeling, by Ian Duncan
# The test dataset contains 10,000 randomly selected patients from a commercial population
# The 10,000 patients are continuously enrolled for two years
###############################################################################################

# Source libraries
library(tree)
library(ROCR)

# Data Setup ----------------------------------------------------------------------------------
setwd("/Users/sjs/dev/git/experiments/duncan")
duncan.data <- read.csv("modeling_sample_dataset.csv")
summary(duncan.data)

# admit_flg_future = column 37
prospective.loh.data <- duncan.data[, c(2, 20, 21, 26, 37, 51:133)]

write.csv(prospective.loh.data, "testfile.csv", row.names = FALSE)

# Logistic Regression Models ------------------------------------------------------------------
prospective.age.gender.fit <- glm(admit_flg_future ~ age + gender, data = prospective.loh.data, family = 'binomial')
prospective.age.gender.dx.fit <- glm(admit_flg_future ~.-allow_current_total -plan_type, data = prospective.loh.data, family = 'binomial')
prospective.age.gender.dx.plantype.fit <- glm(admit_flg_future ~.-allow_current_total , data = prospective.loh.data, family = 'binomial')
prospective.age.gender.dx.cost.fit <- glm(admit_flg_future ~ allow_current_total, data = prospective.loh.data, family = 'binomial')

age.gender.predictions <- predict(prospective.age.gender.fit, prospective.loh.data)
age.gender.dx.predictions <- predict(prospective.age.gender.dx.fit, prospective.loh.data)
age.gender.dx.plantype.predictions <- predict(prospective.age.gender.dx.plantype.fit, prospective.loh.data)
age.gender.dx.cost.predictions <- predict(prospective.age.gender.dx.cost.fit, prospective.loh.data)

# Create the ROC Plot
age.gender.pred <- prediction(age.gender.predictions, prospective.loh.data$admit_flg_future)
age.gender.perf <- performance(age.gender.pred, 'tpr', 'fpr')
age.gender.auc <- performance(age.gender.pred, 'auc')

age.gender.dx.pred <- prediction(age.gender.dx.predictions, prospective.loh.data$admit_flg_future)
age.gender.dx.perf <- performance(age.gender.dx.pred, 'tpr', 'fpr')
age.gender.dx.auc <- performance(age.gender.dx.pred, 'auc')

age.gender.dx.plantype.pred <- prediction(age.gender.dx.plantype.predictions, prospective.loh.data$admit_flg_future)
age.gender.dx.plantype.perf <- performance(age.gender.dx.plantype.pred, 'tpr', 'fpr')
age.gender.dx.plantype.auc <- performance(age.gender.dx.plantype.pred, 'auc')

age.gender.dx.cost.pred <- prediction(age.gender.dx.cost.predictions, prospective.loh.data$admit_flg_future)
age.gender.dx.cost.perf <- performance(age.gender.dx.cost.pred, 'tpr', 'fpr')
age.gender.dx.cost.auc <- performance(age.gender.dx.cost.pred, 'auc')

par(mfrow=c(1,1))

plot(age.gender.perf, lty=3, col="red", main="Likelihood of Hospitalization Models")
plot(age.gender.dx.perf, lty=3, col="blue", add=TRUE)
plot(age.gender.dx.plantype.perf, lty=3, col="purple", add=TRUE)
plot(age.gender.dx.cost.perf, lty=3, col="green", add=TRUE)
abline(a=0, b= 1)
legend(0.25, 0.25, 
       c('Age/Gender', 'Age/Gender/Dx', 'Age/Gender/Dx/Plantype','Age/Gender/Prior Cost'),
       col=c('red','blue', 'purple', 'green'),lwd=3)

# Plot the AUC values
auc.values <- c(unlist(slot(age.gender.auc, "y.values")), 
         unlist(slot(age.gender.dx.auc, "y.values")),
         unlist(slot(age.gender.dx.plantype.auc, "y.values")),
         unlist(slot(age.gender.dx.cost.auc, "y.values")))

barplot(auc.values, main = "AUC", 
        horiz = FALSE, names.arg = c('Age/ Gender', 
                                     'Age / Gender \n / Dx', 
                                     'Age / Gender\n / Dx /\n Plantype', 
                                     'Age / Gender\n / Prior Cost'), cex.names = 0.6)

# Decision Tree Models ------------------------------------------------------------------
# Recode sales as a binary variable High, with value of Yess if Sales > 8
HasAdmit = ifelse(prospective.loh.data$admit_flg_future < 1 , "No", "Yes")

# Merge with rest of of data
prospective.loh.data = data.frame(prospective.loh.data, HasAdmit)

treemodel = tree(HasAdmit ~.-allow_current_total -admit_flg_future, data = prospective.loh.data)
summary (treemodel)

tree.predictions = predict(treemodel, prospective.loh.data, type = "class")

pred.tree <- prediction(tree.predictions, prospective.loh.data$HasAdmit)
perf.age.gender <- performance(pred.age.gender, 'tpr', 'fpr')


# Amazon ML results ------------------------------------------------------------------
amzn <- read.csv("/Users/sjs/dev/git/vp-application/amazon-ml.csv")

amzn.pred <- prediction(amzn$bestAnswer, amzn$trueLabel)
amzn.perf <- performance(amzn.pred, 'tpr', 'fpr')
amzn.auc <- performance(amzn.pred, 'auc')

plot(age.gender.dx.perf, lty=3, col="red", main="Likelihood of Hospitalization Models")
plot(amzn.perf, lty=3, col="blue", add=TRUE)
abline(a=0, b= 1)
legend(0.25, 0.25, 
       c('Age/Gender/Dx', 'Amazon ML'),
       col=c('red', 'blue'), lwd=3)