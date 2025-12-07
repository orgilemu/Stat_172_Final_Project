#This file is where we show which model we think is best for predictions/interpretations and why
rm(list = ls())
library(tidyverse)
library(glmnet) 
library(logistf) #for firths

#source clean data
source("src/data_exploration_and_cleaning.R")

#-----------------------------PREDICTIONS-------------------------------
#the best AUC here was using penalized regression (lasso).

#(this is just pasted from the lasso & ridge file)
#if we set pi* = .162 (threshold that we set for prediction), we are estimated to get a 
#specificity of .693 and sensitivity of 0.601
#that is, we will predict an unfavorable outcome 69% of the time when the outcome is actually unfavorable
#further, we will predict a favorable outcome 60% of the time when the outcome is actually favorable


# --------------------------- INTERPRETATIONS -----------------------------------------------------

#here are the exponentiated coefficients from our final_lasso model. 

# > exp(coef(final_lasso))
# 38 x 1 Matrix of class "dgeMatrix"
# s0
# (Intercept)                                                    0.02138909
# ProcessorICRC                                                  2.50821191
# ProcessorLocal                                                 3.05060354
# HousingYes                                                     3.69069375
# EmploymentYes                                                  1.00000000
# Public.AccommodationsYes                                       0.48836607
# EducationYes                                                   0.84065525
# CreditYes                                                      0.66358081
# Race.TypeAsian or Pacific Islander                             1.01194271
# Race.TypeBlack                                                 0.98644464
# Race.TypeOther                                                 1.22003503
# Race.TypeWhite                                                 0.96089210
# Race.TypeUnapplicable                                          1.64615839
# DisabilityYes                                                  1.20418058
# AgeYes                                                         0.75018029
# Sex.TypeMale                                                   0.69039833
# Sex.TypeUnapplicable                                           0.82178685
# PregnancyYes                                                   1.37927739
# National.Origin.TypeEast Indian                                0.25724713
# National.Origin.TypeHispanic                                   0.75649825
# National.Origin.TypeMexican                                    0.83356044
# National.Origin.TypeOther National Origin                      1.00000000
# National.Origin.TypePerceived as Afghani, Arab, Middle Eastern 1.00000000
# National.Origin.TypeUnapplicable                               1.11515658
# Familial.StatusYes                                             1.00000000
# Marital.StatusYes                                              0.22614247
# Religion.TypeCatholic                                          1.00000000
# Religion.TypeJewish                                            1.00000000
# Religion.TypeMuslim                                            2.14434815
# Religion.TypeOther                                             0.87568329
# Religion.TypeProtestant                                        0.50303237
# Religion.TypeSikhs                                             1.00000000
# Religion.TypeUnapplicable                                      1.98830596
# CreedYes                                                       1.28193515
# ColorYes                                                       0.99593258
# Sexual.OrientationYes                                          1.08248734
# Gender.IdentityYes                                             1.00000000
# RetaliationYes                                                 1.00000000


#from this, we can gain insights about variable importance, and we can interpret the coefficients.
#in order to quantify uncertainty around our interpretations, we need to make a standard MLE using only the variables 
#that lasso decided were important. These variables are those with an estimate of 0, or 1.00 above (because it was exponentiated)


model_data <- model_data %>% 
  mutate(Outcome_bin = ifelse(Outcome == "Favorable", 1, 0)
  )

final_descriptive_glm <- glm(Outcome_bin ~ Processor + Housing + 
                               Public.Accommodations + Education + Credit +
                               Race.Type + Disability + Age + Sex.Type +
                               Pregnancy + National.Origin.Type + Marital.Status + 
                               Religion.Type + Creed + Color + Sexual.Orientation,
                             data = model_data, 
                             family = binomial(link = "logit"))

summary(final_descriptive_glm) 
#there is definitely some complete separation here, there are some huge standard errors (Religion type all over 100)
#so, we need to use firths (logistf)

#We also want to force "Unapplicable" to be the reference level for all Type columns, to make it much easier to interpret
# The relevel() function moves a specific level to the front (baseline) - AI USAGE HERE

model_data$Race.Type <- relevel(model_data$Race.Type, ref = "Unapplicable")
model_data$Sex.Type  <- relevel(model_data$Sex.Type, ref = "Unapplicable")
model_data$National.Origin.Type <- relevel(model_data$National.Origin.Type, ref = "Unapplicable")
model_data$Religion.Type <- relevel(model_data$Religion.Type, ref = "Unapplicable")

final_descriptive_glm <- logistf(Outcome_bin ~ Processor + Housing + 
                               Public.Accommodations + Education + Credit +
                               Race.Type + Disability + Age + Sex.Type +
                               Pregnancy + National.Origin.Type + Marital.Status + 
                               Religion.Type + Creed + Color + Sexual.Orientation,
                             data = model_data, 
                             family = binomial(link = "logit"))

# summary(final_descriptive_glm)
# 
# Model fitted by Penalized ML
# Coefficients:
#                                                                       coef   se(coef)  lower 0.95  upper 0.95       Chisq            p method
# (Intercept)                                                    -3.01962819 0.13075879 -3.28519796 -2.77099332         Inf 0.000000e+00      2
# ProcessorICRC                                                   1.19921054 0.12851352  0.95526522  1.46059684         Inf 0.000000e+00      2
# ProcessorLocal                                                  1.40113351 0.13821256  1.13673099  1.68019603         Inf 0.000000e+00      2
# HousingYes                                                      1.35149891 0.05038112  1.25259326  1.45034512         Inf 0.000000e+00      2
# Public.AccommodationsYes                                       -0.77778814 0.07929394 -0.93643685 -0.62440232         Inf 0.000000e+00      2
# EducationYes                                                   -0.34434353 0.16135033 -0.67528837 -0.03899378  4.93345424 2.634161e-02      2
# CreditYes                                                      -0.66086998 0.49564854 -1.79615374  0.21591005  2.06239880 1.509723e-01      2
# Race.TypeAmerican Indian                                       -0.83498743 0.34538528 -1.58113762 -0.20796882  7.17731696 7.383104e-03      2
# Race.TypeAsian or Pacific Islander                             -0.27826129 0.21624476 -0.72351544  0.13142056  1.72913676 1.885212e-01      2
# Race.TypeBlack                                                 -0.54118428 0.06362959 -0.66687314 -0.41690274         Inf 0.000000e+00      2
# Race.TypeOther                                                 -0.28532384 0.10988287 -0.50489390 -0.07210619  6.94358207 8.412049e-03      2
# Race.TypeWhite                                                 -0.65105628 0.14557520 -0.94721414 -0.37366548 22.99201682 1.626755e-06      2
# DisabilityYes                                                   0.18106387 0.04467979  0.09321376  0.26876530 16.28231082 5.457102e-05      2
# AgeYes                                                         -0.27537858 0.05817246 -0.39062088 -0.16207702 23.21913596 1.445518e-06      2
# Sex.TypeFemale                                                  0.23703160 0.05170897  0.13507719  0.33818554 20.52976097 5.871127e-06      2
# Sex.TypeMale                                                   -0.19535318 0.08892463 -0.37319187 -0.02337321  4.97833512 2.566667e-02      2
# PregnancyYes                                                    0.38083501 0.10065938  0.18035659  0.57556953 13.47690054 2.415184e-04      2
# National.Origin.TypeAfghani, Arab, Middle Eastern              -0.30953540 0.38176174 -1.11904793  0.40010983  0.68478841 4.079427e-01      2
# National.Origin.TypeEast Indian                                -1.86955151 1.44220160 -6.72900865  0.17687872  3.02617864 8.193077e-02      2
# National.Origin.TypeHispanic                                   -0.45490436 0.15116888 -0.76138612 -0.16631813  9.85814295 1.690817e-03      2
# National.Origin.TypeMexican                                    -0.44960536 0.17347065 -0.80283690 -0.11946785  7.28704938 6.945345e-03      2
# National.Origin.TypeOther National Origin                      -0.05339528 0.09029548 -0.23354900  0.12192643  0.34986526 5.541894e-01      2
# National.Origin.TypePerceived as Afghani, Arab, Middle Eastern  0.07834070 0.88838390 -2.17227106  1.57529322  0.00755832 9.307203e-01      2
# Marital.StatusYes                                              -2.07765796 0.82737537 -4.27045136 -0.73801066 11.64272230 6.445421e-04      2
# Religion.Type7th Day Adventist                                 -0.52109384 1.47208941 -5.39865497  1.64785340  0.14403944 7.042978e-01      2
# Religion.TypeCatholic                                          -0.42522663 0.49338407 -1.54984894  0.43730171  0.82846548 3.627166e-01      2
# Religion.TypeJewish                                            -1.20557455 0.83763737 -3.39757925  0.13159425  2.99439851 8.355293e-02      2
# Religion.TypeMuslim                                             0.24081883 0.22499203 -0.22186687  0.66773489  1.08450699 2.976915e-01      2
# Religion.TypeOther                                             -0.95224178 0.16404731 -1.28977023 -0.64244882 43.11135363 5.171130e-11      2
# Religion.TypeProtestant                                        -1.64797239 0.54653782 -2.93448948 -0.71774748 15.21180537 9.610077e-05      2
# Religion.TypeSikhs                                              1.68398430 1.63838938 -3.30345272  4.62580568  0.77081653 3.799646e-01      2
# CreedYes                                                        0.32264063 0.39660839 -0.54208787  1.04489265  0.60610806 4.362570e-01      2
# ColorYes                                                        0.06075982 0.07449792 -0.08623115  0.20686552  0.65894703 4.169319e-01      2
# Sexual.OrientationYes                                           0.07392136 0.09338351 -0.11274472  0.25457141  0.61488644 4.329539e-01      2
# 
# Method: 1-Wald, 2-Profile penalized log-likelihood, 3-None
# 
# Likelihood ratio test=1705.027 on 33 df, p=0, n=23580
# Wald test = 8469.497 on 33 df, p = 0

#------------------------------INTERPRETATIONS-------------------------------------------

#the model as a whole is significant - both likelihood and wald give it a p val of 0.

#where you file your case matters - 
#all else held constant, filing a claim locally has odds of achieving a positive outcome over 4 times greater than those filed 
#at the EEOC (federal level). We are 95% confident that the true change in odds is between a factor of 3.1 and 5.4.

# housing cases seem to be successful, so we say you should file -
#all else held constant, the odds of a positive outcome for claims filed for housing complaints are 3.9 times higher than 
#non-housing claims. We are 95% confident that the true change in odds is between a factor of 3.5 and 4.3. 

#race-based discrimination cases are not successful
#all else held constant, the odds of a positive outcome for claims based on race are between 
#24-57% less than non-race discrimination cases. Race type of asian and pacific islander seems to have the 
#least difference in odds out of all race types, at 24% less.

#marital status claims are not successful -
#all else held constant, the odds of a positive outcome for complaints on the basis of marital status are 
#77% less than non marital status based claims. We are 95% confident that the true value is between 
#52% and 99% less.

#disability cases are successful - 
#all else held constant, the odds for a positive outcome for 
#claims filed on the basis of the complainant's disability are 20% more than non-disability. We are 95%
#confident that the true value is between 10% and 31% more.

#age cases are not successful -
#all else held constant, the odds for a positive outcome for 
#claims filed on the basis of the complainant's age are 25% less than non-age based claims. We are 95%
#confident that the true value is between 15% and 33% less.

#pregnancy cases are successful -
#all else held constant, the odds for a positive outcome for 
#claims filed on the basis of the complainant's pregnancy are 46% more than non-pregnancy based claims. We are 95%
#confident that the true value is between 20% and 78% more.

#the odds of a positive outcome for complaints filed based on creed, color, credit and sexual orientation are not significantly 
#different than not filing on those basis.


#public accommodation cases are not successful -
#all else held constant, the odds for a positive outcome for 
#complaints associated with public accommodations (public services and buildings) 
#are 54% less than non-public accomodation based claims. We are 95%
#confident that the true value is between 47% and 60% less.


#education cases are not successful (but a very big range)-
#all else held constant, the odds for a positive outcome for 
#complaints associated with education are 54% less than non-education based claims. We are 95%
#confident that the true value is between 4% and 49% less.


#not much success in filing outside of catholic, jewish, muslim, protestant, or 7th day adventist - 
#all else held constant, the only religion type that has a statistically significant difference in odds
#of a positive outcome compared to not filing for religion based discrimination is type "other". The odds of a positive outcome
#for religion types that are NOT catholic, jewish, muslim, protestant, or 7th day adventist are 61% less than 
#non-religion based cases. We are 95% confident that the true value is between 47% and 72% less.

#all else held constant, the only two national origin types that have a statistically significant 
#difference in odds of a positive outcome compared to not filing for national origin based discrimination
#are types "Mexican" and "Hispanic". The odds for these both are around 36% less than not filing for national origin based discrimination. 
#we are 95% confident that the true value is between 11% and 55% less.