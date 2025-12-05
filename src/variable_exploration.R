#this model is for exploring our variables before any modeling to see what kind of 
#relationships might exist in our data

rm(list=ls())
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(forcats)  

#source clean data
source("src/data_exploration_and_cleaning.R")

#View(model_data)

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

# Visualizing Y variable (Outcome) over Processing.Days 
ggplot(data = model_data) + 
  geom_histogram(aes(x = Processing.Days, fill = Outcome), position ='fill', binwidth = 75) +
  labs(x = "Processing Days", y = "Proportion") + 
  ggtitle("Case Outcomes over Processing Time") + 
  scale_fill_brewer("Case \nOutcome" , palette = "Paired") + 
  theme_clean

# Visualizing Y variable (Outcome) over Fiscal.Year
ggplot(data = model_data) + 
  geom_histogram(aes(x = Fiscal.Year, fill = Outcome), position ='fill', binwidth = 1) +
  labs(x = "Fiscal Year", y = "Proportion") + 
  ggtitle("Case Outcomes over Time (Fiscal Year)") + 
  scale_fill_brewer("Case \nOutcome", palette = "Paired") + 
  theme_clean

# Visualizing Y variable (Outcome) over Not.Timely cases 
ggplot(data = model_data) + 
  geom_bar(aes(x = Not.Timely, fill = Outcome), position ='fill', binwidth = 75) +
  labs(x = "Timely Case", y = "Proportion", caption = "Note: Not timely cases indicate the case was not recieved within 300 days of the incident") + 
  ggtitle("Case Outcomes over Timely Cases") + 
  scale_fill_brewer("Case \nOutcome", palette = "Paired") + 
  theme_clean


# --- `Processing.Days` by Outcome ---
#
# We'll use a density plot to see the distribution.
# We'll also filter out extreme long-tail cases (> 730 days)
# to make the main distribution more visible.

plot1 <- model_data %>%
  filter(Processing.Days <= 730) %>% # Focus on cases <= 2 years
  ggplot(aes(x = Processing.Days, fill = Outcome)) +
  geom_density(alpha = 0.7) + # Use alpha for transparency
  scale_fill_brewer("Case \nOutcome", palette = "Paired")+
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
  scale_fill_brewer("Case \nOutcome", palette = "Paired") +
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
  scale_fill_brewer("Case \nOutcome", palette = "Paired") +
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


# --- 6. NEW PLOT 4: Outcome Rates by Processor ---
#
# This plot answers the client question:
# "Does it matter which agency handles my complaint?"
#
plot4 <- model_data %>%
  ggplot(aes(x = fct_reorder(Processor, Outcome == "Favorable",
                             .fun = mean, .desc = TRUE),
             fill = Outcome)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer("Case \nOutcome", palette = "Paired") +
  labs(
    title = "Favorable Outcome Rates Vary by Processing Agency",
    subtitle = "Proportion of Favorable vs. Unfavorable outcomes, sorted by Favorable rate",
    x = "Processing Agency",
    y = "Proportion",
    fill = "Case Outcome",
    caption = "Data: Iowa Civil Rights Commission"
  ) +
  theme_clean

# To see the plot:
 print(plot4)


# --- 7. NEW PLOT 5: Outcome Rates by Race.Type ---
#
# This leverages our data cleaning of the 'Type' columns.
# It answers: "For cases where Race *is* a basis,
# do outcomes differ by the specific race type documented?"
#
# --- !!! IMPORTANT !!! ---
# You must run this chunk below to create 'race_type_data'
# BEFORE you can run the 'plot5' chunk.
#
race_type_data <- model_data %>%
  filter(Race == "Yes") # Filter to ONLY cases where Race was a basis

#
# Now that 'race_type_data' exists, you can run this chunk to create plot5
#
plot5 <- race_type_data %>%
  ggplot(aes(x = fct_reorder(Race.Type, Outcome == "Favorable",
                             .fun = mean, .desc = TRUE),
             fill = Outcome)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer("Case \nOutcome", palette = "Paired") +
  labs(
    title = "Favorable Outcome Rates by Race Type",
    subtitle = "Proportion of outcomes for cases where Race was a cited basis, sorted by Favorable rate",
    x = "Race Type Documented",
    y = "Proportion",
    fill = "Case Outcome",
    caption = "Data: Iowa Civil Rights Commission"
  ) +
  theme_clean +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Add angle for readability

# To see the plot:
 print(plot5)


# --- 8. NEW PLOT 6: Multivariate Interaction Plot ---
#
# This is a "stupendous" plot for your client.
# It answers: "Is filing for 'Retaliation' *with* 'Race'
# different than filing for 'Race' alone?"
#
# --- !!! IMPORTANT !!! ---
# You must run this chunk below to create 'interaction_data'
# BEFORE you can run the 'plot6' chunk.
#
interaction_data <- model_data %>%
  # Focus only on cases where one or both of these were a basis
  filter(Race == "Yes" | Retaliation == "Yes") %>%
  mutate(
    Complaint.Profile = case_when(
      Race == "Yes" & Retaliation == "Yes" ~ "Race + Retaliation",
      Race == "Yes" & Retaliation == "No"  ~ "Race Only",
      Race == "No"  & Retaliation == "Yes" ~ "Retaliation Only"
    )
  ) %>%
  # Remove any stray cases (shouldn't be any, but good practice)
  filter(!is.na(Complaint.Profile))

#
# Now that 'interaction_data' exists, you can run this chunk to create plot6
#
plot6 <- interaction_data %>%
  ggplot(aes(x = fct_reorder(Complaint.Profile, Outcome == "Favorable",
                             .fun = mean, .desc = TRUE),
             fill = Outcome)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer("Case \nOutcome", palette = "Paired") +
  labs(
    title = "Favorable Outcome Rates by Complaint Profile",
    subtitle = "Investigating the interaction between Race and Retaliation claims",
    x = "Complaint Profile",
    y = "Proportion",
    fill = "Case Outcome",
    caption = "Data: Iowa Civil Rights Commission"
  ) +
  theme_clean

# To see the plot:
 print(plot6)


