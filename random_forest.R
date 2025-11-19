#random forest
rm(list = ls())
library(randomForest)
library(ggplot2)
library(pROC)
library(tidymodels)

model_data <- readRDS("clean_data.rds")
RNGkind(sample.kind = "default")
set.seed(2025)

train.idx <- sample(x=1:nrow(model_data), size = .7*nrow(model_data))
train.df <- model_data[train.idx, ]    
test.df <- model_data[-train.idx, ]
#remove Closure.Description because we cleaned this to create binary Outcome variable
test.df <- test.df %>% subset(select = -c(Closure.Description)) 
train.df <- train.df %>% subset(select = -c(Closure.Description))


#just used to see if 1000 separate trees takes too long
# myforest <- randomForest(Outcome ~Fiscal.Year + Not.Timely + Not.Jurisdictional + Processor + Housing + Employment + 
#                            Public.Accommodations + Education + Credit + Race.Type + Disability + Age + 
#                            Sex.Type + Pregnancy + National.Origin.Type + Familial.Status + Marital.Status + Religion.Type +
#                            Creed + Color + Sexual.Orientation + Gender.Identity + Retaliation + Processing.Days, 
#                          data = train.df, #only use training data
#                          ntree = 1000, #B = 1000 separate trees
#                          mtry = 4, # m = 4, is sqrt(# of X's which is 24), so 4 or 5
#                          importance = TRUE) #helps us identify important predictions
# myforest

#------results--------
#OOB estimate of  error rate: 11.65%
#Accuracy is 1-.1165 = 88.35%

#Confusion matrix:
              #Unfavorable Favorable class.error
#Unfavorable       14081       125   0.008799099
#Favorable          1798       502   0.781739130

#-------------------TUNING FOREST-------------------
#step 1: define the model with mtry as parameter
rf_model <-rand_forest(mtry = tune(),#tells it to tune mtry only
                       trees = 1000) %>% #fix B to as big of a number as we can afford
  set_mode("classification") %>% #not regression
  set_engine("randomForest") #there are multiple packages that run RF's


#step 2 - create a recipe
rf_rec <- recipe(Outcome ~Fiscal.Year + Not.Timely + Not.Jurisdictional + Processor + Housing + Employment + 
                                               Public.Accommodations + Education + Credit + Race.Type + Disability + Age + 
                                               Sex.Type + Pregnancy + National.Origin.Type + Familial.Status + Marital.Status + Religion.Type +
                                               Creed + Color + Sexual.Orientation + Gender.Identity + Retaliation + Processing.Days, data = train.df) #use training data set here

#step 3 - create the workflow 
rf_wf <- workflow() %>% 
  add_model(rf_model) %>% #from step 1
  add_recipe(rf_rec) #from step 2

#step 4 - create folds for cross validation
set.seed(2025)
folds <- vfold_cv(train.df, v=5) #splits training data into 5 folds

#step 5 - tune random forest
rf_tuned <- tune_grid(
  rf_wf, #workflow from step 3
  resamples = folds, #folds created in step 4
  grid = tibble(mtry = c(4, 2, 8, 16)), #can only select between 1 and 24 x's, takes way too long, so only doing four
  metrics = metric_set(roc_auc) #could add "accuracy" if want to do oob, but roc_auc is better
)

#step 6 - extract AUC (or OOB)
rf_results <- rf_tuned %>% 
  collect_metrics()

#View(rf_results)

#this shows that our best m-try value is 8
# ggplot(data = rf_results)+
#   geom_line(aes(x=mtry,y=mean))+ #mean is mean AUC from cross validation
#   geom_point(aes(x=mtry,y=mean))+ #makes it clearer where break points are
#   labs(x = "m (mtry) value", y = "Area Under the Curve (AUC)")+
#   theme_bw()



best_params <-select_best(rf_tuned, metric = "roc_auc")
final_forest <- final_forest<- randomForest(Outcome ~Fiscal.Year + Not.Timely + Not.Jurisdictional + Processor + Housing + Employment + 
                                              Public.Accommodations + Education + Credit + Race.Type + Disability + Age + 
                                              Sex.Type + Pregnancy + National.Origin.Type + Familial.Status + Marital.Status + Religion.Type +
                                              Creed + Color + Sexual.Orientation + Gender.Identity + Retaliation + Processing.Days,
                                            data = train.df, #only use training data
                                            ntree = 1000, #B = 1000 separate trees
                                            mtry = best_params %>% pull(mtry), #m=whatever we found was best earlier
                                            importance = TRUE) #helps us identify important predictions
#----prediction----------
pi_hat <- predict(final_forest, test.df, type = "prob")[,"Favorable"]
rocCurve <-roc(response = test.df$Outcome,
               predictor = pi_hat,
               levels = c("Unfavorable","Favorable")) # always negative, positive
plot(rocCurve, print.thres = TRUE, print.auc = TRUE)

#if we set pi* = .152 (threshold that we set for prediction), we are estimated to get a 
#specificity of .843 and sensitivity of 0.731
#that is, we will predict an unfavorable outcome 84% of the time when the outcome is actually unfavorable
#further, we will predict a favorable outcome 73% of the time when the outcome is actually favorable

#make a column of preds in our test data
pi_star <- coords(rocCurve, "best", ret = "threshold")$threshold[1]
test.df$forest_pred <- ifelse(pi_hat>pi_star, "Favorable","Unfavorable") %>% as.factor()
head(test.df)

#-----------interpretation------------
varImpPlot(final_forest, type = 1)
model_data$Outcome_bin <-ifelse(model_data$Outcome == "Favorable", 1,0)


m1 <- glm(Outcome_bin ~Processing.Days., #started with best predictor (according to model)
          data = model_data,
          family = binomial(link="logit")) 

