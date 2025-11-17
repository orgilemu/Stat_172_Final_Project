library(dplyr)
library(ggplot2)

#read in clean data RDS that we created in the cleaning file
model_data <- readRDS("clean_data.rds")
View(model_data)

ggplot(data = model_data) + 
  geom_histogram(aes(x = Processing.Days, fill = Outcome), position ='fill', binwidth = 75) +
  labs(x = "Processing Days", y = "Proportion") + 
  ggtitle(" Case Outcomes over Time") + 
  scale_fill_grey("Case \nOutcome") + 
  theme_bw()
