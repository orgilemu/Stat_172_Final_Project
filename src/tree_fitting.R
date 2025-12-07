#this file fits a classification tree

rm(list = ls())
library(rpart) # for classification trees 
library(rpart.plot) # to get attractive plots of trees
library(pROC)


#source clean data
source("src/data_exploration_and_cleaning.R")


### --- TREE FITTING -----
set.seed(2025)
tree <- rpart(Outcome ~ .,
              data = train_predictive, 
              method = 'class') # creating the tree

rpart.plot(tree) # plotting the tree
tree # textual output for large data sets 

### ----- TUNNING THE TREE ------

printcp(tree) # sub tree splits 
# The smallest x-error is in the biggest tree (last row) tried, 0.90261
# This indicates R did not grow the tree to it's fullest extent 
set.seed(2025)
tree <- rpart(Outcome ~ .,
              data = train_predictive, 
              method = 'class', 
              control = rpart.control(cp = 0.0001, minsplit = 1))
rpart.plot(tree) # plot tree
printcp(tree) # sub trees 

# Find optimal CP using minimum xerror (automate)
optimalCP <- tree$cptable[which.min(tree$cptable[,"xerror"]) , "CP"] 

tree2 <- rpart::prune(tree, cp = optimalCP) # automatized CP
rpart.plot(tree2) # plot tree2
tree2
# tree2 is our tuned tree - it is the final tree we will use for predicting 


# ------- MODEL VALIDATION ------
# Need to make a column of predictions 
# Need to do that on the TESTING data 

test.df$result_pred <- predict(tree2, test.df, type = "class")

# Make a confusion matrix 
table(test.df$result_pred, test.df$Outcome)

pi_hat <- predict(tree2, test.df, type = "prob")[ , "Favorable"]

# "Favorable" is out positive event 
rocCurve <- roc(response = test.df$Outcome, #supply our truth (in test set)
                predictor = pi_hat, # supply predicted PROBABILITIES of positive case
                levels = c("Unfavorable", "Favorable")) # (negative, positive)

# Plot ROC curve
plot(rocCurve, print.thres = TRUE, print.auc = TRUE)

# Interpretations: 

# If we set pi* = 0.162, we can achieve specificity of 0.912
# and sensitivity of 0.321

# That is, we'll predict an unfavorable outcome 91.2% of the time when 
# an unfavorable outcome actually happens.  

# That is, we'll predict a favorable outcome 32.1% of the time when 
# a favorable outcome actually happens.   
# Area under the curve is 0.621


# Obtain predictions consistent with the above promises: 
pi_star <- coords(rocCurve, "best", ret = "threshold")$threshold[1]
test.df$result_pred <- as.factor(ifelse(pi_hat > pi_star, "Favorable", "Unfavorable"))
