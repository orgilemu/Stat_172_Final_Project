rm(list = ls())
library(rpart) # for classification trees 
library(rpart.plot) # to get attractive plots of trees


#read in clean data RDS that we created in the cleaning file
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
              method = 'class')

rpart.plot(tree) # plotting the tree
ctree # textual output for large data sets 

### ----- TUNNING THE TREE ------