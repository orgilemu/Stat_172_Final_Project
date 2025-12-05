rm(list=ls())
library(dplyr)
library(forcats)
data <- read.csv('raw/Closed_Discrimination_Complaint_Cases_in_Iowa.csv', stringsAsFactors = TRUE)

#explore data
#View(data)
#table(data$Closure.Description)
#summary(data)


# --- Y-VAR CLEANING: Outcome Categories ---
# Based on our client (a potential victim), a "win" is a favorable
# settlement. A "loss" is ANY other final closure that is not a win.


#------------AI USAGE: Gemini helped explain the definition of each outcome, so that we could decide which were considered
#favorable/unfavorable, and which we should exclude because they neither help/hurt our client.
#AI also helped with the code for cleaning this column --------------------


# Favorable" (Win / Y=1): The client received a tangible, positive outcome.
favorable_outcomes <- c(
  "Satisfactory Adjustment",
  "Successful Conciliation",
  "Withdrawal with Satisfactory Adjustment",
  "Local Conciliation",
  "Local SA",
  "Probable Cause",
  "Probable Cause/No Probable Cause"
)

# Unfavorable" (Loss / Y=0): Any procedural or merits-based closure
# that did not result in a tangible win for the client.
unfavorable_outcomes <- c(
  # Merits-Based Losses
  "No Probable Cause",
  "Hearing Officer Dismissal",
  "Not Jurisdictional/No Probable Cause",
  
  # Procedural Losses (From the client's perspective)
  "Administrative Closure",
  "Administrative Closure/No Jurisdiction",
  "EEOC AC Closure",
  "Failure to Cooperate/AC",
  "Failure to Locate",
  "Local AC Closure",
  "No Jurisdiction",
  "Not Timely",
  "Withdrawal" # Simple withdrawal without settlement
)

# Exclude: These are not final "wins" or "losses" for this agency.
# We will filter these cases out entirely.
# Right to Sue: This is NOT a loss. It's a procedural step to
# escalate the case to court. Including it would distort our model.
# Transferred: The case was simply moved to another agency.
# Please Select / Other: Bad data.
# Closed After Public Hearing: Ambiguous outcome, safer to exclude.


# --- Create New 'Outcome' Column and Filter ---

data <- data %>%
  mutate(
    # Use case_when() to create the new binary variable
    Outcome = case_when(
      Closure.Description %in% favorable_outcomes   ~ "Favorable",
      Closure.Description %in% unfavorable_outcomes ~ "Unfavorable",
      
      # All other cases (Right to Sue, Transferred, Other, etc.)
      # will be assigned NA
      TRUE                                          ~ NA_character_
    )
  ) %>%
  # Filter out all the rows where Outcome is NA (our "Exclude" group)
  filter(!is.na(Outcome)) %>%
  
  # Convert the new Outcome column to a factor.
  mutate(
    Outcome = factor(Outcome, levels = c("Unfavorable", "Favorable"))
  )


#check work
#print(table(data$Outcome))
#View(data)
#summary(data)

#since our y var Outcome is just a categorization of Closure.Description, we want to remove this from our data frame and only 
#use Outcome instead.

data <- data %>%
  select(-Closure.Description)

#confirm it worked
#head(data)

#Replace NA's in Race.Type, Sex.Type, National.Origin.Type and Religion.Type into a new column called 
#"Unknown"

#----------AI USAGE: Gemini helped with the code to clean this because there were issues with converting between string/factor
#and removing the na's ---------------------

columns_to_clean_na <- c(
  "Race.Type",
  "Sex.Type",
  "National.Origin.Type",
  "Religion.Type"
)

data <- data %>%
  mutate(
    across(
      all_of(columns_to_clean_na),
      ~ . %>%
        # Use fct_recode() to change factor levels to NULL (NA)
        fct_recode(
          NULL = "",    # Recode empty string "" to NA
          NULL = "NA",  # Recode the string "NA" to NA
          NULL = " "    # Recode a single space " " to NA
        ) %>%
        fct_explicit_na(na_level = "Unapplicable")
    )
  )

#check work
#summary(data)

#there are 5 NAs in Processing.Days, going to check it out
#na_processing_rows <- data %>%
#filter(is.na(Processing.Days))
#View(na_processing_rows)

#these 5 have a Date.Closed, but no Date.Opened. Since there are only 5, we are just going to drop them.
model_data <- data %>%
  filter(!is.na(Processing.Days))


