install.packages("sf")

# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(rnaturalearth)
library(sf)

# Step 1: Find resistance status columns
resistance_cols2 <- grep("_I$", names(data), value = TRUE)

# Step 2: Keep those with ≤90% missing (i.e., at least 10% have values)
valid_resistance_cols2 <- resistance_cols2[
  colMeans(is.na(data[resistance_cols2])) <= 0.90
]

# Step 3: Select necessary columns
spatial_base <- data %>%
  dplyr::select(Country, Year, Species, dplyr::all_of(valid_resistance_cols2))

# Step 4  Check which columns passed
print(valid_resistance_cols2)

# Step 6 Save the filtered dataset for future use
write.csv(spatial_base, "spatial_resistance_data.csv", row.names = FALSE)

##Reshaping and Summarising Resistance Rates

# Step 1: Pivot wider resistance columns into long format
long_data <- spatial_base %>%
  pivot_longer(
    cols = ends_with("_I"),         # All resistance columns
    names_to = "Drug",              # Put drug name in a column
    values_to = "Resistance_Status" # Put actual values in another
  )

# Step 2: Filter out missing results (keep only tested rows)
long_data_clean <- long_data %>%
  filter(!is.na(Resistance_Status))

# Step 3: Group and count for each Drug-Country-Year combo
resistance_summary <- long_data_clean %>%
  group_by(Country, Year, Drug) %>%
  summarise(
    Resistant_Count = sum(Resistance_Status == "Resistant", na.rm = TRUE),
    Total_Tested = n(),
    Resistance_Rate = Resistant_Count / Total_Tested,
    .groups = "drop"
  )

# Step 4: Preview summary
print(head(resistance_summary))


