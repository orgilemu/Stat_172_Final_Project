#This file is where we show which model we think is best for predictions/interpretations and why
rm(list = ls())
library(tidyverse)
library(glmnet) 

#source clean data
source("src/data_exploration_and_cleaning.R")

#-----------------------------PREDICTIONS-------------------------------
#the best AUC here was the regular MLE. However, for predictions, we will use our random forest model from 
#random_forest.R because its AUC is .858


# --------------------------- INTERPRETATIONS -----------------------------------------------------
# Our best model from the lasso & ridge.R file was just the regular MLE (only by .001), so let's compare it to
#the best model from the result of our variable importance plot in our random_forest.R file (this was m10)
forest_model <- glm(Outcome_bin ~Processing.Days+Fiscal.Year+Processor+Not.Timely+Housing+Not.Jurisdictional
                    +Race.Type+Disability+Public.Accommodations+Sex.Type,
                    data = model_data,
                    family = binomial(link="logit")) 
AIC(forest_model) #17202.23
summary(forest_model) #we do not think there is complete separation here because all of the 
#standard errors are small


#regular mle that was the best model in this file, but using our full dataset instead of only train
lr_mle_best <- glm(Outcome_bin ~ Fiscal.Year + Not.Timely + Not.Jurisdictional + Processor + Housing + Employment + 
                     Public.Accommodations + Education + Credit + Race.Type + Disability + Age + 
                     Sex.Type + Pregnancy + National.Origin.Type + Familial.Status + Marital.Status + Religion.Type +
                     Creed + Color + Sexual.Orientation + Gender.Identity + Retaliation + Processing.Days, 
                   data = model_data, 
                   family = binomial(link = "logit"))
AIC(lr_mle_best) #17080.46
summary(lr_mle_best) #there are some huge standard errors here which implies we likely have complete separation
#Religion.Type levels have 187+ for standard error, which is way too big

#Although the lr_mle_best model has a slightly lower AIC,
#We are leaning towards using the forest_model instead because it does not suffer from complete separation, and 
#there are less x variables. This should make the model simpler to interpret and give recommendations to our client.
#Just to check, we calculated the BICs
BIC(forest_model)#17339.39
BIC(lr_mle_best)#17419.32
#as we thought, the forest_model is simpler. Since the BIC is lower for the forest_model, we feel comfortable
#choosing it as the best model for our interpretations.

summary(forest_model)
coef(forest_model)
coef(forest_model) %>% exp()
confint(forest_model)
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




