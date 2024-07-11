# Load necessary libraries
library(dplyr)
library(readr)


# Load the dataset
burnout_data <- readr::read_csv("Burnout_Modified2.csv")

# Clean the dataset and rename columns
cleaned_data <- burnout_data %>%
  # Remove rows with missing values
  tidyr::drop_na() %>%
  # Remove duplicate rows
  distinct() %>%
  # Correct any data inconsistencies if needed (example shown for Gender)
  mutate(Gender = recode(Gender, 'M' = 'Male', 'F' = 'Female')) %>%
  # Rename columns for consistency
  rename(
    patient_id = `Patient ID`,
    year = `Year`,
    age = `Age`,
    gender = `Gender`,
    marital_status = `Marital Status`,
    profession = `Profession`,
    years_in_profession = `Years in Profession`,
    work_life_balance = `Work-Life Balance`,
    job_satisfaction = `Job Satisfaction`,
    burnout_level = `Burnout Level`,
    burnout_category = `Burnout Category`
  )

# Save the cleaned dataset to a new CSV file
readr::write_csv(cleaned_data, "Cleaned_Burnout_Modified2.csv")

# Print the first few rows of the cleaned dataset
print(head(cleaned_data))