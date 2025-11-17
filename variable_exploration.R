library(dplyr)
library(ggplot2)

#read in clean data RDS that we created in the cleaning file
model_data <- readRDS("clean_data.rds")
View(model_data)


# Visualizing Y variable (Outcome) over Processing.Days 
ggplot(data = model_data) + 
  geom_histogram(aes(x = Processing.Days, fill = Outcome), position ='fill', binwidth = 75) +
  labs(x = "Processing Days", y = "Proportion") + 
  ggtitle("Case Outcomes over Time") + 
  scale_fill_grey("Case \nOutcome") + 
  theme_bw()

# Visualizing Y variable (Outcome) over Not.Timely cases 
ggplot(data = model_data) + 
  geom_bar(aes(x = Not.Timely, fill = Outcome), position ='fill', binwidth = 75) +
  labs(x = "Timely Case", y = "Proportion", caption = "Note: Not timely cases indicate the case was not recieved within 300 days of the incident") + 
  ggtitle("Case Outcomes over Timely Cases") + 
  scale_fill_grey("Case \nOutcome") + 
  theme_bw() 
  
