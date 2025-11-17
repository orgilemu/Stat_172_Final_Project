rm(list=ls())
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(forcats)  

#read in clean data RDS that we created in the cleaning file
model_data <- readRDS("clean_data.rds")
#View(model_data)


# Visualizing Y variable (Outcome) over Processing.Days 
ggplot(data = model_data) + 
  geom_histogram(aes(x = Processing.Days, fill = Outcome), position ='fill', binwidth = 75) +
  labs(x = "Processing Days", y = "Proportion") + 
  ggtitle("Case Outcomes over Processing Time") + 
  scale_fill_grey("Case \nOutcome") + 
  theme_bw()

# Visualizing Y variable (Outcome) over Fiscal.Year
ggplot(data = model_data) + 
  geom_histogram(aes(x = Fiscal.Year, fill = Outcome), position ='fill', binwidth = 1) +
  labs(x = "Fiscal Year", y = "Proportion") + 
  ggtitle("Case Outcomes over Time (Fiscal Year)") + 
  scale_fill_grey("Case \nOutcome") + 
  theme_bw()

# Visualizing Y variable (Outcome) over Not.Timely cases 
ggplot(data = model_data) + 
  geom_bar(aes(x = Not.Timely, fill = Outcome), position ='fill', binwidth = 75) +
  labs(x = "Timely Case", y = "Proportion", caption = "Note: Not timely cases indicate the case was not recieved within 300 days of the incident") + 
  ggtitle("Case Outcomes over Timely Cases") + 
  scale_fill_grey("Case \nOutcome") + 
  theme_bw() 


# --- 2. Define a Professional Theme ---
#
# We can define a theme to reuse for all our plots
# This makes them look consistent and professional.
theme_clean <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13, color = "gray30"),
    plot.caption = element_text(color = "gray50", face = "italic"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

# Define a color palette for our "Favorable" vs "Unfavorable"
# This is colorblind-friendly and professional
outcome_colors <- c("Favorable" = "#1F77B4", "Unfavorable" = "#D62728")


# --- `Processing.Days` by Outcome ---
#
# We'll use a density plot to see the distribution.
# We'll also filter out extreme long-tail cases (> 730 days)
# to make the main distribution more visible.

plot1 <- model_data %>%
  filter(Processing.Days <= 730) %>% # Focus on cases <= 2 years
  ggplot(aes(x = Processing.Days, fill = Outcome)) +
  geom_density(alpha = 0.7) + # Use alpha for transparency
  scale_fill_manual(values = outcome_colors) +
  labs(
    title = "Favorable Outcomes Tend to Have Shorter Processing Times",
    subtitle = "Distribution of processing days for cases closed within 2 years",
    x = "Processing Days",
    y = "Density",
    fill = "Case Outcome",
    caption = "Data: Iowa Civil Rights Commission"
  ) +
  theme_clean
print(plot1)

basis_data <- model_data %>%
  select(Outcome, Employment, Housing, Public.Accommodations, Education, Credit) %>%
  pivot_longer(
    cols = -Outcome, # Pivot everything except the Outcome
    names_to = "Complaint.Basis",
    values_to = "Value"
  ) %>%
  filter(Value == "Yes") # Keep only rows where this basis was "Yes"


plot2 <- basis_data %>%
  #
  # --- MODIFICATION HERE ---
  #
  # We use fct_reorder() on the x-axis variable.
  # 1. `.f = Complaint.Basis`: The factor we want to reorder.
  # 2. `.x = Outcome == "Favorable"`: What we use to reorder. This creates
  #    a TRUE/FALSE (1/0) vector.
  # 3. `.fun = mean`: We calculate the mean of the 1s and 0s, which
  #    gives us the proportion of "Favorable" cases.
  # 4. `.desc = TRUE`: We sort in descending (decreasing) order.
  #
  ggplot(aes(x = fct_reorder(Complaint.Basis, Outcome == "Favorable",
                             .fun = mean, .desc = TRUE),
             fill = Outcome)) +
  # `position = "fill"` creates the 100% stacked bar
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) + # Show Y axis as %
  scale_fill_manual(values = outcome_colors) +
  labs(
    title = "Favorable Outcome Rates Vary by Complaint Basis",
    subtitle = "Proportion of Favorable vs. Unfavorable outcomes, sorted by Favorable rate",
    x = "Basis of Complaint",
    y = "Proportion",
    fill = "Case Outcome",
    caption = "Data: Iowa Civil Rights Commission"
  ) +
  theme_clean
print(plot2)


# ---Outcome Rates by Complaint Demographics ---

demo_data <- model_data %>%
  select(Outcome, Race, Disability, Age, Sex, National.Origin, Religion) %>%
  pivot_longer(
    cols = -Outcome,
    names_to = "Demographic.Basis",
    values_to = "Value"
  ) %>%
  filter(Value == "Yes")

plot3 <- demo_data %>%
  ggplot(aes(x = fct_reorder(Demographic.Basis, Outcome == "Favorable",
                             .fun = mean, .desc = TRUE),
             fill = Outcome)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = outcome_colors) +
  labs(
    title = "Favorable Outcome Rates by Demographic Basis",
    subtitle = "Proportion of Favorable vs. Unfavorable outcomes, sorted by Favorable rate",
    x = "Demographic Basis",
    y = "Proportion",
    fill = "Case Outcome",
    caption = "Data: Iowa Civil Rights Commission"
  ) +
  theme_clean
print(plot3)

