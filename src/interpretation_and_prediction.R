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

final_descriptive_glm <- logistf(Outcome_bin ~ Processor + Housing + 
                               Public.Accommodations + Education + Credit +
                               Race.Type + Disability + Age + Sex.Type +
                               Pregnancy + National.Origin.Type + Marital.Status + 
                               Religion.Type + Creed + Color + Sexual.Orientation,
                             data = model_data, 
                             family = binomial(link = "logit"))

summary(final_descriptive_glm)

# Model fitted by Penalized ML
# Coefficients:
#   coef   se(coef)  lower 0.95  upper 0.95        Chisq            p
# (Intercept)                                                    -4.44821326 1.56461068 -9.39710000 -2.00780916 15.983204500 6.390693e-05
# ProcessorICRC                                                   1.19921054 0.12851352  0.95526522  1.46059684          Inf 0.000000e+00
# ProcessorLocal                                                  1.40113351 0.13821256  1.13673099  1.68019603          Inf 0.000000e+00
# HousingYes                                                      1.35149891 0.05038112  1.25259326  1.45034512          Inf 0.000000e+00
# Public.AccommodationsYes                                       -0.77778814 0.07929394 -0.93643685 -0.62440232          Inf 0.000000e+00
# EducationYes                                                   -0.34434353 0.16135033 -0.67528837 -0.03899378  4.933454238 2.634161e-02
# CreditYes                                                      -0.66086998 0.49564854 -1.79615374  0.21591005  2.062398798 1.509723e-01
# Race.TypeAsian or Pacific Islander                              0.55672614 0.40271452 -0.20454587  1.39589458  2.016157503 1.556325e-01
# Race.TypeBlack                                                  0.29380315 0.34618538 -0.33502984  1.04124132  0.770822052 3.799629e-01
# Race.TypeOther                                                  0.54966360 0.35669519 -0.10355468  1.31421143  2.660775029 1.028506e-01
# Race.TypeWhite                                                  0.18393115 0.37106930 -0.50298053  0.97155029  0.252459772 6.153484e-01
# Race.TypeUnapplicable                                           0.83498743 0.34538528  0.20796882  1.58113762  7.177316956 7.383104e-03
# DisabilityYes                                                   0.18106387 0.04467979  0.09321376  0.26876530 16.282310819 5.457102e-05
# AgeYes                                                         -0.27537858 0.05817246 -0.39062088 -0.16207702 23.219135956 1.445518e-06
# Sex.TypeMale                                                   -0.43238478 0.09498818 -0.62161316 -0.24801032 21.808295621 3.012946e-06
# Sex.TypeUnapplicable                                           -0.23703160 0.05170897 -0.33818554 -0.13507719 20.529760974 5.871127e-06
# PregnancyYes                                                    0.38083501 0.10065938  0.18035659  0.57556953 13.476900544 2.415184e-04
# National.Origin.TypeEast Indian                                -1.56001611 1.48992739 -6.45418309  0.66132887  1.624691054 2.024389e-01
# National.Origin.TypeHispanic                                   -0.14536896 0.40700954 -0.91121307  0.70637981  0.123725958 7.250280e-01
# National.Origin.TypeMexican                                    -0.14006996 0.41515201 -0.92424557  0.72530584  0.110690994 7.393588e-01
# National.Origin.TypeOther National Origin                       0.25614012 0.38534694 -0.46168230  1.07149650  0.455254736 4.998501e-01
# National.Origin.TypePerceived as Afghani, Arab, Middle Eastern  0.38787610 0.96257256 -1.95061835  2.08456843  0.150542264 6.980177e-01
# National.Origin.TypeUnapplicable                                0.30953540 0.38176174 -0.40010983  1.11904793  0.684788406 4.079427e-01
# Marital.StatusYes                                              -2.07765796 0.82737537 -4.27045136 -0.73801066 11.642722300 6.445421e-04
# Religion.TypeCatholic                                           0.09586721 1.55230801 -2.35533225  5.03210653  0.003868968 9.504027e-01
# Religion.TypeJewish                                            -0.68448071 1.69314855 -3.75494454  4.35182844  0.146037408 7.023513e-01
# Religion.TypeMuslim                                             0.76191267 1.48818374 -1.46009559  5.65136791  0.318842611 5.723040e-01
# Religion.TypeOther                                             -0.43114794 1.48069043 -2.62842376  4.45274802  0.074990060 7.842052e-01
# Religion.TypeProtestant                                        -1.12687856 1.57002335 -3.65202764  3.82201612  0.401063675 5.265404e-01
# Religion.TypeSikhs                                              2.20507814 2.20217477 -3.21099870  7.65632176  0.906100983 3.411511e-01
# Religion.TypeUnapplicable                                       0.52109384 1.47208941 -1.64785340  5.39865497  0.144039438 7.042978e-01
# CreedYes                                                        0.32264063 0.39660839 -0.54208787  1.04489265  0.606108061 4.362570e-01
# ColorYes                                                        0.06075982 0.07449792 -0.08623115  0.20686552  0.658947026 4.169319e-01
# Sexual.OrientationYes                                           0.07392136 0.09338351 -0.11274472  0.25457141  0.614886441 4.329539e-01
# method
# (Intercept)                                                         2
# ProcessorICRC                                                       2
# ProcessorLocal                                                      2
# HousingYes                                                          2
# Public.AccommodationsYes                                            2
# EducationYes                                                        2
# CreditYes                                                           2
# Race.TypeAsian or Pacific Islander                                  2
# Race.TypeBlack                                                      2
# Race.TypeOther                                                      2
# Race.TypeWhite                                                      2
# Race.TypeUnapplicable                                               2
# DisabilityYes                                                       2
# AgeYes                                                              2
# Sex.TypeMale                                                        2
# Sex.TypeUnapplicable                                                2
# PregnancyYes                                                        2
# National.Origin.TypeEast Indian                                     2
# National.Origin.TypeHispanic                                        2
# National.Origin.TypeMexican                                         2
# National.Origin.TypeOther National Origin                           2
# National.Origin.TypePerceived as Afghani, Arab, Middle Eastern      2
# National.Origin.TypeUnapplicable                                    2
# Marital.StatusYes                                                   2
# Religion.TypeCatholic                                               2
# Religion.TypeJewish                                                 2
# Religion.TypeMuslim                                                 2
# Religion.TypeOther                                                  2
# Religion.TypeProtestant                                             2
# Religion.TypeSikhs                                                  2
# Religion.TypeUnapplicable                                           2
# CreedYes                                                            2
# ColorYes                                                            2
# Sexual.OrientationYes                                               2
# 
# Method: 1-Wald, 2-Profile penalized log-likelihood, 3-None
# 
# Likelihood ratio test=1705.027 on 33 df, p=0, n=23580
# Wald test = 8469.497 on 33 df, p = 0


#where you file your case matters - 
#all else held constant, filing a claim locally has odds of achieving a positive outcome over 4 times greater than those filed 
#at the EEOC (federal level). We are 95% confident that the true change in odds is between a factor of 3.1 and 5.4.

#exp(1.40113351)
#exp(1.13673099)
#exp(1.68019603)
