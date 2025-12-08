# Credit to https://corybrunson.github.io/ggalluvial/
# Credit to ChatGBT for help with plot code/errors 

library(ggplot2)
library(ggalluvial)
library(dplyr)

# read in data 
model_data <- readRDS("clean_data.rds")

# Theme consistency 
theme_clean <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13, color = "gray30"),
    plot.caption = element_text(color = "gray50", face = "italic"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

# Processor to Outcome 
# create data frame, filtering out "Other
df_proc <- model_data %>%
  filter(Processor != "Other") %>% 
  count(Processor, Outcome)
# create hammock plot
ggplot(df_proc,
       aes(axis1 = Processor,
           axis2 = Outcome,
           y = n)) +
  geom_alluvium(aes(fill = Processor)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Processor", "Outcome")) +
  labs(title = "Processor to Outcome Hammock",
       y = "Number of Complaints") + 
  scale_fill_manual(values = c("#ece2f0", "#a6bddb", "#1c9099")) +
  theme_clean

# Race Type to Outcome 
# create data frame, filtering out "Other", "Unknown" and "Unapplicable"
df_race <- model_data %>%
  filter(!Race.Type %in% c("Other", "Unknown", "Unapplicable", "")) %>% 
  count(Race.Type, Outcome)

ggplot(df_race,
       aes(axis1 = Race.Type,
           axis2 = Outcome,
           y = n)) +
  geom_alluvium(aes(fill = Race.Type)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Race.Type", "Outcome")) +
  labs(title = "Race Type to Outcome Hammock",
       y = "Number of Complaints") +
  scale_fill_manual(values = c("#f6eff7", "#bdc9e1", "#67a9cf", "#1c9099", "#016c59")) +
  theme_clean

# Discrimination to Outcome 
# Create dataframe 
df_basis <- model_data %>%
  select(Outcome,
         Housing, Employment, Public.Accommodations, Education, Credit,
         Race, Disability, Age, Sex, Pregnancy,
         National.Origin, Familial.Status, Marital.Status,
         Religion, Creed, Color, Sexual.Orientation,
         Gender.Identity, Retaliation) %>%
  pivot_longer(
    cols = -Outcome,
    names_to = "Basis",
    values_to = "Flag"
  ) %>%
  filter(Flag == "Yes") %>%
  count(Basis, Outcome) %>%
  mutate(
    Basis = str_replace_all(Basis, "\\.", " "),
    Basis = fct_reorder(Basis, n, .fun = sum, .desc = TRUE)
  )
# make hammock plot
ggplot(df_basis,
       aes(axis1 = Basis,
           axis2 = Outcome,
           y = n)) +
  geom_alluvium(aes(fill = Basis), alpha = 0.8) +
  geom_stratum(fill = "white", color = "black", size = 0.4) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            size = 3,
            hjust = 0.5,
            lineheight = 0.9) +
  scale_x_discrete(limits = c("Basis", "Outcome"),
                   expand = c(0.15, 0.05)) +
  labs(title = "Discrimination Basis to Outcome Hammock",
       x = NULL,
       y = "Number of Complaints") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 13, face = "bold"),
    plot.title = element_text(size = 18, face = "bold")
  )
