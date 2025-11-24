#random forest
rm(list = ls())
library(randomForest)
library(ggplot2)
library(pROC)
library(tidymodels)

#source clean data
source("src/data_exploration_and_cleaning.R")
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


m1 <- glm(Outcome_bin ~Processing.Days, #started with best predictor (according to model)
          data = model_data,
          family = binomial(link="logit")) 
AIC(m1) #19155.66


m2 <- glm(Outcome_bin ~Processing.Days+Fiscal.Year,
          data = model_data,
          family = binomial(link="logit")) 
AIC(m2) #19028.98


m3 <- glm(Outcome_bin ~Processing.Days+Fiscal.Year+Processor,
          data = model_data,
          family = binomial(link="logit")) 
AIC(m3) #18847.73


m4 <- glm(Outcome_bin ~Processing.Days+Fiscal.Year+Processor+Not.Timely,
          data = model_data,
          family = binomial(link="logit")) 
AIC(m4) #18711.17


m5 <- glm(Outcome_bin ~Processing.Days+Fiscal.Year+Processor+Not.Timely+Housing,
          data = model_data,
          family = binomial(link="logit")) 
AIC(m5) #17673.8


m6 <- glm(Outcome_bin ~Processing.Days+Fiscal.Year+Processor+Not.Timely+Housing+Not.Jurisdictional,
          data = model_data,
          family = binomial(link="logit")) 
AIC(m6) #17572.95

m7 <- glm(Outcome_bin ~Processing.Days+Fiscal.Year+Processor+Not.Timely+Housing+Not.Jurisdictional
          +Race.Type,
          data = model_data,
          family = binomial(link="logit")) 
AIC(m7) #17327.52

m8 <- glm(Outcome_bin ~Processing.Days+Fiscal.Year+Processor+Not.Timely+Housing+Not.Jurisdictional
          +Race.Type+Disability,
          data = model_data,
          family = binomial(link="logit")) 
AIC(m8) #17323.12


m9 <- glm(Outcome_bin ~Processing.Days+Fiscal.Year+Processor+Not.Timely+Housing+Not.Jurisdictional
          +Race.Type+Disability+Public.Accommodations,
          data = model_data,
          family = binomial(link="logit")) 
AIC(m9) #17236.73

m10 <- glm(Outcome_bin ~Processing.Days+Fiscal.Year+Processor+Not.Timely+Housing+Not.Jurisdictional
           +Race.Type+Disability+Public.Accommodations+Sex.Type,
           data = model_data,
           family = binomial(link="logit")) 
AIC(m10) #17202.23


m11 <- glm(Outcome_bin ~Processing.Days+Fiscal.Year+Processor+Not.Timely+Housing+Not.Jurisdictional
           +Race.Type+Disability+Public.Accommodations+Sex.Type+Employment,
           data = model_data,
           family = binomial(link="logit")) 
AIC(m11) #17203.33

#-------IT GOT WORSE AFTER M11, SO M10 IS OUR BEST MODEL HERE ----------
#THE AIC IS 17202.23
summary(m10)
coef(m10)
coef(m10) %>% exp()
confint(m10)
#baseline for Processor is "EEOC"
#baseline for Not.Timely is "No"
#baseline for Housing is "No"
#baseline for Not.Jurisdictional is "No"
#baseline for Race.Type is "American Indian"
#baseline for Disability is "No"
#baseline for Public.Accommodations is "No"
#baseline for Sex.Type is "female"


#All other variables held constant, the odds of a favorable outcome for cases that are not timely 
#(the complaint was NOT received within 300 days of the alleged incident) are 6.2 times the odds of cases that are timely. 
#We are 95% confident that the true change in odds is between e^1.6 and e^2.04, or 
#4.95 and 7.69 times higher than timely cases.


#All other variables held constant, for each additional month (30 days) that the case is being processed,
#the odds of a favorable outcome change by a factor of e^30*.00034 = 1.010308, or a 1% increase in odds.
#We are 95% confident that the true change in odds are between e^30*0.00018 and e^30*.0005 or a 0.5% increase
#to a 1.5% increase.

#All other variables held constant, 

