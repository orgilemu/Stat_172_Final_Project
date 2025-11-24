rm(list = ls())
library(tidyverse)
library(pROC) 
# new packages
library(glmnet) 
library(lubridate)

model_data <- readRDS("clean_data.rds")
View(model_data)

RNGkind(sample.kind = "default")
set.seed(23591)

model_data <- model_data %>% 
  mutate(Outcome_bin = ifelse(Outcome == "Favorable", 1, 0)
  ) 



train.idx <- sample(x = 1:nrow(model_data), size = 0.7*nrow(model_data))
train.df <- model_data[train.idx,]
test.df <- model_data[-train.idx,]

lr_mle <- glm(Outcome_bin ~ Fiscal.Year + Not.Timely + Not.Jurisdictional + Processor + Housing + Employment + 
                Public.Accommodations + Education + Credit + Race.Type + Disability + Age + 
                Sex.Type + Pregnancy + National.Origin.Type + Familial.Status + Marital.Status + Religion.Type +
                Creed + Color + Sexual.Orientation + Gender.Identity + Retaliation + Processing.Days, 
              data = train.df, 
              family = binomial(link = "logit"))

summary(lr_mle)


lr_ml_coefs <- coef(lr_mle)

# ---- Lasso & Ridge ------


# create matrix which one-hot codes all factor variables
x.train <- model.matrix(Outcome_bin ~ Fiscal.Year + Not.Timely + Not.Jurisdictional + Processor + Housing + Employment + 
                          Public.Accommodations + Education + Credit + Race.Type + Disability + Age + 
                          Sex.Type + Pregnancy + National.Origin.Type + Familial.Status + Marital.Status + Religion.Type +
                          Creed + Color + Sexual.Orientation + Gender.Identity + Retaliation + Processing.Days, data = train.df)[, -1]

x.test <- model.matrix(Outcome_bin ~ Fiscal.Year + Not.Timely + Not.Jurisdictional + Processor + Housing + Employment + 
                         Public.Accommodations + Education + Credit + Race.Type + Disability + Age + 
                         Sex.Type + Pregnancy + National.Origin.Type + Familial.Status + Marital.Status + Religion.Type +
                         Creed + Color + Sexual.Orientation + Gender.Identity + Retaliation + Processing.Days, data = test.df)[, -1]

# create vectors of 0/1 y variable
y.train <- as.vector(train.df$Outcome_bin)
y.test <- as.vector(test.df$Outcome_bin)


lr_lasso_cv <- cv.glmnet(x.train, 
                         y.train, 
                         family = binomial(link = "logit"),
                         alpha = 1) # sets us in lasso mode (not ridge)

lr_ridge_cv <- cv.glmnet(x.train, 
                         y.train, 
                         family = binomial(link = "logit"),
                         alpha = 0) # sets us in ridge mode (not lasso)


# ---- best lasso & ridge ----


best_lasso_lambda <- lr_lasso_cv$lambda.min
best_ridge_lambda <- lr_ridge_cv$lambda.min

lr_ridge_coefs <- coef(lr_ridge_cv, s = "lambda.min") %>% as.matrix()
lr_lasso_coefs <- coef(lr_lasso_cv, s = "lambda.min") %>% as.matrix()

lr_ridge_coefs
lr_lasso_coefs




ggplot() + 
  geom_point(aes(x = lr_ml_coefs, y = lr_ridge_coefs)) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  xlim(c(-10, 10)) + ylim(c(-10,10))

ggplot() + 
  geom_point(aes(x = lr_ml_coefs, y = lr_lasso_coefs)) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  xlim(c(-10, 10)) + ylim(c(-10,10))




final_lasso <- glmnet(x.train, y.train, 
                      family = binomial(link = "logit"),
                      alpha =1,
                      lambda = best_lasso_lambda)

final_ridge <- glmnet(x.train, y.train, 
                      family = binomial(link = "logit"),
                      alpha = 0 ,
                      lambda = best_ridge_lambda)




test.df.preds <- test.df %>%
  mutate(mle_pred = predict(lr_mle, test.df, type = "response"),
         lasso_pred = predict(final_lasso, x.test, type = "response")[,1],
         ridge_pred = predict(final_ridge, x.test, type = "response")[,1])

view(test.df.preds) 





cor(test.df.preds$mle_pred, test.df.preds$lasso_pred)
plot(test.df.preds$mle_pred, test.df.preds$lasso_pred)






mle_rocCurve <- roc(response = as.factor(test.df.preds$Outcome_bin),
                    predictor = test.df.preds$mle_pred,
                    levels = c("0", "1"))

lasso_rocCurve <- roc(response = as.factor(test.df.preds$Outcome_bin),
                      predictor = test.df.preds$lasso_pred,
                      levels = c("0", "1"))

ridge_rocCurve <- roc(response = as.factor(test.df.preds$Outcome_bin),
                      predictor = test.df.preds$ridge_pred,
                      levels = c("0", "1"))



plot(mle_rocCurve)
plot(lasso_rocCurve)
plot(ridge_rocCurve)

#make data frame of MLE ROC info
mle_data <- data.frame(
  Model = "MLE",
  Specificity = mle_rocCurve$specificities,
  Sensitivity = mle_rocCurve$sensitivities,
  AUC = as.numeric(mle_rocCurve$auc)
)
#make data frame of lasso ROC info
lasso_data <- data.frame(
  Model = "Lasso",
  Specificity = lasso_rocCurve$specificities,
  Sensitivity = lasso_rocCurve$sensitivities,
  AUC = lasso_rocCurve$auc %>% as.numeric
)
#make data frame of ridge ROC info
ridge_data <- data.frame(
  Model = "Ridge",
  Specificity = ridge_rocCurve$specificities,
  Sensitivity = ridge_rocCurve$sensitivities,
  AUC = ridge_rocCurve$auc%>% as.numeric
)

# Combine all the data frames
roc_data <- rbind(mle_data, lasso_data, ridge_data)


# Plot the data
ggplot() +
  geom_line(aes(x = 1 - Specificity, y = Sensitivity, color = Model),data = roc_data) +
  geom_text(data = roc_data %>% group_by(Model) %>% slice(1), 
            aes(x = 0.75, y = c(0.75, 0.65, 0.55), colour = Model,
                label = paste0(Model, " AUC = ", round(AUC, 3)))) +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "1 - Specificity", y = "Sensitivity", color = "Model") +
  theme_minimal()


