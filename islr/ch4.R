###############################################################################
# R Lab from Chapter 4: Logistic Regression, LDA, QDA, and KNN
###############################################################################

###############################################################################
# 4.6.1 The Stock Market Data
###############################################################################

# Data set consists of percentage returns for the S&P 500 stock index over 
# 1,250 days, from the beginning of 2001 until the end of 2005. For each date, 
# we have recorded the percentage returns for each of the five previous 
# trading days, Lag1 through Lag5. 
# We have also recorded Volume (the number of shares traded Today 
# (the percentage return on the date in question) and Direction 
# (whether the market was Up or Down on this date).
library(ISLR)
names(Smarket)
summary(Smarket)

# Return pairwise correlations among predictors in the dataset.
# Note: the last column is removed column which is not an integer
cor(Smarket[,-9])

attach(Smarket)
plot(Volume)

###############################################################################
# 4.6.2 Logistic Regression
###############################################################################

# Fit a logistic regression model in order to predict direction. Pass binomial
# to run logistic regression (vs. other types of glm)
glm.fit = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)

# P values do not show association between lag and direction!
summary(glm.fit)

# Access coefficients for the model
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,4]

# Predict probability that the market will go up, given predictors
glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]

# Contrasts shows that R has created dummy variable where 1 = up
contrasts (Direction)

# Convert these predicted probabilities into class labels, Up or Down
# Create a vector of class predictions based on whether the predicted 
# probability of a market increase is greater than or less than 0.5.
glm.pred=rep("Down",1250)
glm.pred[glm.probs >.5]="Up"
table(glm.pred,Direction)
mean(glm.pred==Direction )

# Create hold out set of observations for 2005
train=(Year < 2005)
Smarket.2005= Smarket [! train ,]
dim(Smarket.2005)
Direction.2005=Direction[!train]

# Train and test model on two different datasets: 2001-2004 and 2005
glm.fit=glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume , data=Smarket ,family=binomial, subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")

# Compute predictions for 2005 and compare to actual
glm.pred=rep("Down",252)
glm.pred[glm.probs >.5]="Up"

table(glm.pred, Direction.2005)

mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)

# Refit model with strongest predictors in original model
glm.fit=glm(Direction ~ Lag1+Lag2,data=Smarket ,family=binomial, subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs >.5]="Up"
table(glm.pred,Direction.2005)

# Results increase to 58% success
mean(glm.pred==Direction.2005)

# Predict returns associated with specific values
predict(glm.fit,newdata=data.frame(Lag1=c(1.2,1.5), Lag2=c(1.1,-0.8)),type="response")

###############################################################################
# 4.6.3 Linear Discriminant Analysis
###############################################################################
library(MASS)
lda.fit=lda(Direction ~ Lag1+Lag2,data=Smarket, subset=train)
lda.fit

lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)

# LDA and logistic regression predictions are almost identical
lda.class=lda.pred$class
table(lda.class ,Direction.2005)
mean(lda.class==Direction.2005)

# Recreate the predictions contained in lda.pred$class.
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)

lda.pred$posterior[1:20,1]
lda.class[1:20]

# Find days where posterior probability is at least 90%
sum(lda.pred$posterior[,1]>.9)

###############################################################################
# 4.6.4 Quadratic Discriminant Analysis
###############################################################################

# qda is also contained in the MASS library. The syntax is equivalent
qda.fit=qda(Direction ~ Lag1+Lag2,data=Smarket ,subset=train)
qda.fit

qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)

###############################################################################
# 4.6.5 K-Nearest Neighbors
###############################################################################
library(class)

# Set the four inputs for KNN
train.X=cbind(Lag1 ,Lag2)[train ,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction =Direction [train]
set.seed (1)

knn.pred=knn(train.X,test.X,train.Direction ,k=1)

# Results in 50% correct predictions
table(knn.pred,Direction.2005)

# Adjust K to get less flexible fit to the data
knn.pred=knn(train.X,test.X,train.Direction ,k=3)

# Slight improvement to 0.53
table(knn.pred,Direction.2005)

###############################################################################
# 4.6.6 An Application to Caravan Insurance Data
###############################################################################

dim(Caravan)
attach(Caravan)

summary(Purchase)

# standardize the data so every column has stdev of 1 and mean of zero
standardized.X=scale(Caravan [,-86])

# Compare before and after
var ( Caravan [ ,1])
var ( Caravan [ ,2])
var(standardized.X[,1])
var(standardized.X[,2])

# split the observations into a test set, containing the first 1,000 
# observations, and a training set, containing the remaining observations.
test =1:1000
train.X=standardized.X[-test ,]
test.X=standardized.X[test ,]
train.Y=Purchase [-test]
test.Y=Purchase [test]
set.seed (1)
knn.pred=knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")

# knn does better than random among customers predicted to buy insurance:
# 12% (9 of 59) vs 6%
table(knn.pred,test.Y)

# Increasing k to 5 improves success rate to almost 27%
knn.pred=knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)
knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)

# Fit a logistic regression model to the data
glm.fit=glm(Purchase ~.,data=Caravan ,family=binomial, subset=-test)
glm.probs=predict(glm.fit,Caravan[test,],type="response")
glm.pred=rep("No",1000)

# Using a cutoff of 0.5 results in only seven candidates, all wrong
glm.pred[glm.probs >.5]="Yes"
table(glm.pred,test.Y)

# Lower threshold to prediction of 25% - correct predictions = 33%, 5x lift vs random
glm.pred=rep("No",1000)
glm.pred[glm.probs >.25]=" Yes"
table(glm.pred,test.Y)