###############################################################################
# 8.3 - Lab: Decision Trees
###############################################################################

# The tree library is used to construct classification and regression trees
library(tree)
library(ISLR)

###############################################################################
# 8.3.1 - Fitting Classification Trees
###############################################################################

attach(Carseats)

# Recode sales as a binary variable High, with value of Yess if Sales > 8
High = ifelse(Sales <= 8,"No", "Yes")

# Merge High with rest of of carseats data
Carseats = data.frame(Carseats, High)

# Fit a classification tree in order to predict High using all variables but sales
tree.carseats =tree(High~.-Sales, Carseats)

# Training error rate = misclassification error rate
summary (tree.carseats)

# Plot tree and use text function to label the nodes
# Pretty=0 to include category names for qualitative predictors, instead of a letter
plot(tree.carseats)
text(tree.carseats, pretty=0) 

# Print output for each branch of the tree
tree.carseats

# Estimate  the test error to evaluate the performance
# Create training set of 200 rows
set.seed(2)
train = sample(1:nrow(Carseats), 200) 
Carseats.test = Carseats[-train,]
High.test=High[-train]

# Create tree using training set
tree.carseats =tree(High ~ .-Sales, Carseats, subset = train)

tree.pred = predict(tree.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)

# Consider whether pruning the tree might lead to improved results
# cv.tree() performs cross-validation in order to determine the optimal level of tree complexity
set.seed(3)
cv.carseats = cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)

# We plot the error rate as a function of both size and k.
par(mfrow = c(1,2))
plot(cv.carseats$size ,cv.carseats$dev ,type = "b")
plot(cv.carseats$k ,cv.carseats$dev ,type = "b")

# Apply apply the prune.misclass() function in order to prune the tree
# to obtain the nine-node tree
prune.carseats = prune.misclass(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats,pretty =0)

# Evaluate performance of pruned tree on test data set
tree.pred = predict(prune.carseats, Carseats.test, type =" class ")
table(tree.pred,High.test)

# increase the value of best, we obtain a larger pruned tree with lower
# classification accuracy:
prune.carseats = prune.misclass(tree.carseats, best = 15)
plot(prune.carseats)
text(prune.carseats,pretty = 0)
tree.pred = predict (prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)

###############################################################################
# 8.3.2 - Fitting Regression Trees
###############################################################################

# First create traiing set and fit a regression tree
# Classification vs. regression selected automatically based on target variable
library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston = tree(medv ~., Boston, subset=train)

# Show results. Only 3 variables used to create tree
# Deviance = sum of squared errors
summary(tree.boston)

plot(tree.boston)
text(tree.boston, pretty = 0)

# Use CV to determine if pruning the tree will improve performance
# Plot deviance as a function of tree size to review results
cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type='b')

# The most complex tree had the lowest deviance. 
prune.boston = prune.tree(tree.boston, best=5)
plot(prune.boston)
text(prune.boston, pretty = 0)

# Use unpruned tree to make predictions on the test set
yhat = predict(tree.boston, newdata = Boston[-train ,])
boston.test = Boston[-train ,"medv"]
plot(yhat, boston.test)
abline(0,1)

# Test set MSE associated with the regression tree is 25.05. 
# The square root of the MSE is therefore around 5.005, 
# indicating that this model leads to test predictions that 
# are within around $5,005 of the true median home value for the suburb.
mean((yhat-boston.test)^2)

###############################################################################
# 8.3.3 - Bagging and Random Forests
###############################################################################
library(randomForest)
set.seed(1)
bag.boston = randomForest(medv ~., data = Boston, subset = train, mtry = 13, importance = TRUE)
bag.boston

# Review performance of bagged model on the test set
yhat.bag = predict(bag.boston, newdata = Boston[-train ,])
plot(yhat.bag, boston.test)
abline(0,1)

# Test set MSE associated with the bagged regression tree is 13.16, almost
# half that obtained using an optimally-pruned single tree
mean((yhat.bag - boston.test)^2)

# Change number of trees grown by randomForest() using the ntree argument
bag.boston = randomForest(medv ~., data = Boston, subset = train, mtry = 13, ntree = 25)
yhat.bag = predict(bag.boston, newdata = Boston[-train ,])
mean((yhat.bag- boston.test)^2)

# Use smaller value of mtry argument, to shift to random forests vs. bagging
# MSE of 11.31 indicates that random forest yielded improvement over bagging
set.seed(1)
rf.boston =randomForest(medv ???., data=Boston, subset =train, mtry=6, importance =TRUE)
yhat.rf = predict(rf.boston, newdata =Boston[-train ,])
mean((yhat.rf - boston.test)^2)

# View the importance of each variable
importance(rf.boston)
varImpPlot(rf.boston)

###############################################################################
# 8.3.4 - Boosting
###############################################################################
library(gbm)
set.seed(1)
# Fit boosted regression trees to the dataset. 5000 trees with max depth of 4
boost.boston = gbm(medv ~., data=Boston[train,], distribution ="gaussian", n.trees=5000, interaction.depth = 4)
summary(boost.boston)

# Produce partial dependence plots to illustrate marginal dependence of lstat and rm
# after integrating out the other variables
par(mfrow = c(1,2))
plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")

# Use boosted model to predict medv on the test set
# The test MSE obtained is 11.8; similar to the test MSE for random forests
# and superior to that for bagging
yhat.boost = predict(boost.boston, newdata = Boston[-train,], n.trees = 5000)
mean((yhat.boost - boston.test)^2)

# Perform boosting with different value of shrinkage parameter
boost.boston = gbm(medv ~., data = Boston[train,], distribution = "gaussian",
                   n.trees = 5000, interaction.depth = 4, shrinkage = 0.2, verbose = F)

yhat.boost = predict(boost.boston, newdata = Boston[-train,], n.trees =5000)
mean((yhat.boost -boston.test)^2)
