#this model is for fitting our random forest, and fitting the predictive model using the variable importance plot 
rm(list = ls())
library(randomForest)
library(ggplot2)
library(pROC)
library(tidymodels)

#source clean data
source("src/data_exploration_and_cleaning.R")
RNGkind(sample.kind = "default")
set.seed(2025)

#-------------------TUNING FOREST-------------------
#step 1: define the model with mtry as parameter
rf_model <-rand_forest(mtry = tune(),#tells it to tune mtry only
                       trees = 1000) %>% #fix B to as big of a number as we can afford
  set_mode("classification") %>% #not regression
  set_engine("randomForest") #there are multiple packages that run RF's


#step 2 - create a recipe
rf_rec <- recipe(Outcome ~., data = train_predictive) #use training data set here

#step 3 - create the workflow 
rf_wf <- workflow() %>% 
  add_model(rf_model) %>% #from step 1
  add_recipe(rf_rec) #from step 2

#step 4 - create folds for cross validation
set.seed(2025)
folds <- vfold_cv(train_predictive, v=5) #splits training data into 5 folds


ncol(train_predictive) #there are 21 columns, 20 x vars

#step 5 - tune random forest
rf_tuned <- tune_grid(
  rf_wf, #workflow from step 3
  resamples = folds, #folds created in step 4
  grid = tibble(mtry = c(4, 2, 8, 16)), #can only select between 1 and 20 x's, takes way too long, so only doing four
  metrics = metric_set(roc_auc) #could add "accuracy" if want to do oob, but roc_auc is better
)

#step 6 - extract AUC (or OOB)
rf_results <- rf_tuned %>% 
  collect_metrics()

#View(rf_results)

#this shows our best m try value
# ggplot(data = rf_results)+
#   geom_line(aes(x=mtry,y=mean))+ #mean is mean AUC from cross validation
#   geom_point(aes(x=mtry,y=mean))+ #makes it clearer where break points are
#   labs(x = "m (mtry) value", y = "Area Under the Curve (AUC)")+
#   theme_bw()



best_params <-select_best(rf_tuned, metric = "roc_auc")
final_forest <- final_forest<- randomForest(Outcome ~.,
                                            data = train_predictive, #only use training data
                                            ntree = 1000, #B = 1000 separate trees
                                            mtry = best_params %>% pull(mtry), #m=whatever we found was best earlier
                                            importance = TRUE) #helps us identify important predictions
#----prediction----------
pi_hat <- predict(final_forest, test.df, type = "prob")[,"Favorable"]
rocCurve <-roc(response = test.df$Outcome,
               predictor = pi_hat,
               levels = c("Unfavorable","Favorable")) # always negative, positive
plot(rocCurve, print.thres = TRUE, print.auc = TRUE)

#if we set pi* = .021 (threshold that we set for prediction), we are estimated to get a 
#specificity of .927 and sensitivity of 0.281
#that is, we will predict an unfavorable outcome 93% of the time when the outcome is actually unfavorable
#further, we will predict a favorable outcome 28% of the time when the outcome is actually favorable

#make a column of preds in our test data
pi_star <- coords(rocCurve, "best", ret = "threshold")$threshold[1]
test.df$forest_pred <- ifelse(pi_hat>pi_star, "Favorable","Unfavorable") %>% as.factor()
head(test.df)

#-----------interpretation------------
varImpPlot(final_forest, type = 1)
model_data$Outcome_bin <-ifelse(model_data$Outcome == "Favorable", 1,0)

#we decided to not make a descriptive model using this variable importance plot because the AUC was so much lower than 
#using lasso
