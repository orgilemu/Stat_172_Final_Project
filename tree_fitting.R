rm(list = ls())
library(rpart) # for classification trees 
library(rpart.plot) # to get attractive plots of trees


# Read in clean data RDS that we created in the cleaning file
model_data <- readRDS("clean_data.rds")
View(model_data)



# Setting the seed
RNGkind(sample.kind = "default")
set.seed(2025)

# Creating a vector of randomly selected rows that will go into training data set 
train.idx <- sample(x = 1:nrow(model_data), 0.7*nrow(model_data))
head(train.idx)

# Split data into train/test

train.df <- model_data[train.idx, ]
test.df <- model_data[-train.idx, ] 

dim(train.df)
dim(test.df)


### --- TREE FITTING -----
set.seed(2025)
tree <- rpart(Outcome ~ Fiscal.Year + Not.Timely + Not.Jurisdictional + Processor + Housing + Employment + 
                Public.Accommodations + Education + Credit + Race.Type + Disability + Age + 
                Sex.Type + Pregnancy + National.Origin.Type + Familial.Status + Marital.Status + Religion.Type +
                Creed + Color + Sexual.Orientation + Gender.Identity + Retaliation + Processing.Days, 
              data = train.df, 
              method = 'class') # creating the tree

rpart.plot(tree) # plotting the tree
tree # textual output for large data sets 

### ----- TUNNING THE TREE ------

printcp(tree) # sub tree splits 
# The smallest x-error is in the biggest tree (last row) tried, 0.90261
# This indicates R did not grow the tree to it's fullest extent 
set.seed(2025)
tree <- rpart(Outcome ~ Fiscal.Year + Not.Timely + Not.Jurisdictional + Processor + Housing + Employment + 
                Public.Accommodations + Education + Credit + Race.Type + Disability + Age + 
                Sex.Type + Pregnancy + National.Origin.Type + Familial.Status + Marital.Status + Religion.Type +
                Creed + Color + Sexual.Orientation + Gender.Identity + Retaliation + Processing.Days, 
              data = train.df, 
              method = 'class', 
              control = rpart.control(cp = 0.0001, minsplit = 1))
rpart.plot(tree) # plot tree
printcp(tree) # sub trees 
which.min(tree$cptable[,"xerror"]) # optimal split based on min xerror
# The smallest xerror is at 20 splits with xerror of 0.82043 

# Find optimal CP using minimum xerror (automate)
optimalCP <- tree$cptable[which.min(tree$cptable[,"xerror"]) , "CP"] 

tree2 <- prune(tree, cp = optimalCP) # automatized CP
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

# If we set pi* = 0.131, we can achieve specificity of 0.810
# and sensitivity of 0.654

# That is, we'll predict an unfavorable outcome 81.0% of the time when 
# an unfavorable outcome actually happens.  

# That is, we'll predict a favorable outcome 65.4% of the time when 
# a favorable outcome actually happens.   
# Area under the curve is 0.784


# Obtain predictions consistent with the above promises: 
pi_star <- coords(rocCurve, "best", ret = "threshold")[1]
# Pi* is equal to 0.1306307
test.df$result_pred <- as.factor(ifelse(pi_hat > pi_star, "Unfavorable", "Favorable"))
