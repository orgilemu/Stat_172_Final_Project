#This file is where we show which model we think is best for predictions/interpretations and why
rm(list = ls())
library(tidyverse)
library(glmnet) 
library(logistf) #for firths
library(ggplot2)
library(dplyr)

#source clean data
source("src/data_exploration_and_cleaning.R")

#-----------------------------PREDICTIONS-------------------------------
#the best AUC here was using penalized regression (lasso).

#(this is just pasted from the lasso & ridge file)
#if we set pi* = .162 (threshold that we set for prediction), we are estimated to get a 
#specificity of .693 and sensitivity of 0.601
#that is, we will predict an unfavorable outcome 69% of the time when the outcome is actually unfavorable
#further, we will predict a favorable outcome 60% of the time when the outcome is actually favorable
# ---------------------- PLOT AUC's---------------
#AI USAGE - GEMINI
model_performance <- data.frame(
  Model = c("Classification Tree", 
            "Random Forest", 
            "Regular MLE", 
            "Lasso", 
            "Ridge"),
  
  AUC = c(0.621,  # Replace with Tree AUC
          0.605,  # Replace with Forest AUC
          0.722,  # Replace with MLE AUC
          0.722,  # Replace with Lasso AUC
          0.720)  # Replace with Ridge AUC
)

auc_plot <- ggplot(model_performance, aes(x = reorder(Model, AUC), y = AUC, fill = Model)) +
  
  geom_col(width = 0.7, show.legend = FALSE) +
  
  geom_text(aes(label = round(AUC, 3)), 
            vjust = -0.5, 
            size = 5, 
            fontface = "bold") +
  #colorblind-friendly colors
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  
  labs(
    title = "Model Performance Comparison",
    subtitle = "Area Under the Curve (AUC) on Test Data",
    x = "Predictive Model",
    y = "AUC Score (Higher is Better)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major.x = element_blank(), # Removes vertical grid lines
    axis.text.x = element_text(face = "bold", size = 11)
  )
print(auc_plot)

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


# Race-based discrimination cases are not successful.
# All else held constant, claims based on American Indian, Black, White, or 'Other' races
# have significantly lower odds of a positive outcome compared to non-race claims.
# These groups face odds that are between 25% and 57% lower than non-race based claims. The only
# race type that did not have a significant difference from non-race based claims was
#Asian/Pacific Islander.

#marital status claims are not successful -
#all else held constant, the odds of a positive outcome for complaints on the basis of marital status are 
#88% less than non marital status based claims. We are 95% confident that the true value is between 
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


#education cases are not successful -
#all else held constant, the odds for a positive outcome for 
#complaints associated with education are 29% less than non-education based claims. We are 95%
#confident that the true value is between 4% and 49% less.


#not much success in religion, ESPECIALLY protestant- 
#all else held constant, the only religion type that has a statistically significant difference in odds
#of a positive outcome compared to not filing for religion based discrimination is type "other" and "protestant". The odds of a positive outcome
#for religion types that are NOT catholic, jewish, muslim, protestant or 7th day adventist are 61% less than 
#non-religion based cases. We are 95% confident that the true value is between 47% and 72% less.
#However, for protestants, the odds of a positive outcome are 80% less than not filing. We are 95% confident
#that the true value is between 51% and 95% less.

#all else held constant, the only two national origin types that have a statistically significant 
#difference in odds of a positive outcome compared to not filing for national origin based discrimination
#are types "Mexican" and "Hispanic". The odds for these both are around 36% less than not filing for national origin based discrimination. 
#we are 95% confident that the true value is between 11% and 55% less.

#female has significantly better odds, male has significantly lower-
#Female complainants have significantly higher odds of success than the baseline (cases where Sex was not a factor).
#We are 95% confident that being Female increases the odds of a favorable outcome by 14% to 40%.
#Male complainants face a statistically significant disadvantage. We are 95% confident that being Male decreases
#the odds of a favorable outcome by roughly 2% to 31% compared to non-sex-based claims.

#--------------------------------plotting results-----------------------------
#AI USAGE------ USED GEMINI FOR THESE PLOTS

data <- data.frame(
  Factor = c("Local Agency", "Housing", "Pregnancy", "Disability", 
             "Sex: Male", "Age", "Education", "National Origin: Mexican", 
             "Public Accommodations", "Race: Black", "Race: White", "Marital Status"),
  OddsRatio = c(4.06, 3.86, 1.46, 1.20, 
                0.69, 0.76, 0.71, 0.64, 
                0.46, 0.58, 0.52, 0.13),
  LowerCI = c(3.11, 3.49, 1.20, 1.10, 
              0.55, 0.67, 0.51, 0.45, 
              0.39, 0.47, 0.43, 0.01),
  UpperCI = c(5.37, 4.26, 1.78, 1.31, 
              0.85, 0.85, 0.96, 0.90, 
              0.54, 0.72, 0.63, 0.48),
  Type = c("Good", "Good", "Good", "Good", 
           "Bad", "Bad", "Bad", "Bad", 
           "Bad", "Bad", "Bad", "Bad")
)

# PLOT ON ALL FACTORS
plot <- ggplot(data, aes(x = OddsRatio, y = reorder(Factor, OddsRatio), color = Type)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", size = 1) +
  geom_point(size = 4) +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.3, size = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "What Drivers Increase Your Odds of Success?",
    subtitle = "Odds Ratios (95% Confidence Intervals) relative to the Baseline",
    x = "Odds Ratio ( > 1 = Higher Odds of Favorable Outcome)",
    y = "",
    caption = "Baseline: Federal Agency, Non-Housing, Female, Non-Race Case"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    axis.text.y = element_text(face = "bold")
  )

print(plot)

religion_data <- data.frame(
  Type = c("Muslim", "Sikhs", "Jewish", "Catholic", 
           "Protestant", "7th Day Adventist", "Other"),
  OddsRatio = c(1.27, 5.39, 0.30, 0.65, 
                0.19, 0.59, 0.39), # exp(coef)
  LowerCI = c(0.80, 0.04, 0.03, 0.21, 
              0.05, 0.004, 0.27),  # exp(lower 0.95)
  UpperCI = c(1.95, 102.3, 1.14, 1.55, 
              0.49, 5.19, 0.53),   # exp(upper 0.95)
  Significant = c("No", "No", "No", "No", 
                  "Yes", "No", "Yes") # Based on p < 0.05
)

# --- PLOT ON BASIS OF RELIGION ---
plot_religion <- ggplot(religion_data, aes(x = OddsRatio, y = reorder(Type, OddsRatio), color = Significant)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
  
  # Error Bars
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2, size = 1) +
  geom_point(size = 4) +
  
  # Log Scale for X-axis (CRITICAL for wide ranges like Sikhs)
  scale_x_log10(breaks = c(0.1, 0.5, 1, 2, 5, 10, 100)) +
  
  scale_color_brewer(palette = "Set1") +
  
  labs(
    title = "Religion-Based Outcome Odds",
    subtitle = "Odds Ratios relative to Non-Religion Cases (Log Scale)",
    caption = "Note: 'Sikhs' shows high uncertainty (wide bar). 'Protestant' is significantly lower.",
    x = "Odds Ratio (Log Scale)",
    y = "Religion Type"
  ) +
  theme_minimal(base_size = 14)

print(plot_religion)


origin_data <- data.frame(
  Type = c("East Indian", "Hispanic", "Mexican", 
           "Afghani/Arab/Mid-East", "Other"),
  OddsRatio = c(0.15, 0.63, 0.64, 
                1.08, 0.95), 
  LowerCI = c(0.001, 0.47, 0.45, 
              0.11, 0.79), 
  UpperCI = c(1.19, 0.85, 0.89, 
              4.83, 1.13), 
  Significant = c("No", "Yes", "Yes", 
                  "No", "No") 
)

# --- PLOT FOR BASIS OF NATIONAL ORIGIN ---
plot_origin <- ggplot(origin_data, aes(x = OddsRatio, y = reorder(Type, OddsRatio), color = Significant)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2, size = 1) +
  geom_point(size = 4) +
  
  scale_color_brewer(palette="Set1") +
  
  labs(
    title = "National Origin Outcomes",
    subtitle = "Odds Ratios relative to Non-National Origin Cases",
    x = "Odds Ratio (< 1 = Lower Odds of Success)",
    y = "National Origin Group"
  ) +
  theme_minimal(base_size = 14)

print(plot_origin)


sex_data <- data.frame(
  Gender = c("Female", "Male"),
  OddsRatio = c(1.27, 0.82),       # Calculated exp(coef)
  LowerCI = c(1.14, 0.69),         # Calculated exp(lower .95)
  UpperCI = c(1.40, 0.98),         # Calculated exp(upper .95)
  Type = c("Positive", "Negative") # For coloring
)

# ---PLOT FOR BASIS OF SEX ---
plot_sex <- ggplot(sex_data, aes(x = OddsRatio, y = Gender, color = Type)) +
  # Add vertical line at 1 (Neutral/Baseline)
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", size = 1) +
  
  # Add Error Bars (Confidence Intervals)
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2, size = 1) +
  
  # Add Points
  geom_point(size = 5) +
    scale_color_brewer(palette = "Set1") +

  labs(
    title = "Gender Outcomes in Discrimination Cases",
    subtitle = "Odds Ratios relative to Baseline (Cases where Sex was not a factor)",
    caption = "Baseline: 'Unapplicable' (Cases filed on other bases like Race or Housing)",
    x = "Odds Ratio ( > 1 = Higher Odds of Success)",
    y = ""
  ) +
  
  # Professional Theme
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray30"),
    legend.position = "none", # Hide legend since the y-axis labels explain it
    panel.grid.minor = element_blank()
  )

print(plot_sex)


race_data <- data.frame(
  Race = c("American Indian", "White", "Black", "Other", "Asian / Pacific Islander"),
  
  OddsRatio = c(0.43, 0.52, 0.58, 0.75, 0.76), 
  
  LowerCI = c(0.21, 0.39, 0.51, 0.60, 0.48),   
  
  UpperCI = c(0.81, 0.69, 0.66, 0.93, 1.14),   
  
  # Significant? (Based on p < 0.05 in output)
  # Asian was p=0.18 (No), others were p < 0.05 (Yes)
  Significant = c("Yes", "Yes", "Yes", "Yes", "No") 
)

# --- PLOT FOR RACE ------
plot_race <- ggplot(race_data, aes(x = OddsRatio, y = reorder(Race, OddsRatio), color = Significant)) +
  
  # Vertical line at 1.0 (Neutral Baseline)
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
  
  # Error Bars (The Confidence Interval)
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2, size = 1) +
  
  # The Point Estimate
  geom_point(size = 5) +
  
  # color-blind friendly color scheme
  scale_color_brewer(palette = "Set1") +
  
  # Labels
  labs(
    title = "Race Outcomes in Discrimination Cases",
    subtitle = "Odds Ratios relative to Non-Race Claims (Unapplicable)",
    caption = "Note: 'Asian / Pacific Islander' interval crosses 1.0, making the result inconclusive.",
    x = "Odds Ratio ( < 1 indicates lower odds of success)",
    y = "Race Type"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

# Print
print(plot_race)
