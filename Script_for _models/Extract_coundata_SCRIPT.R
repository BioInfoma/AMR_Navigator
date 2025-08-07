
# Ensure necessary packages
library(readr)
library(dplyr)

# Load your full dataset
data <- read_csv("~/AMR NAVIGATOR/2025_03_11_atlas_antibiotics.csv")

# Step 1: Identify all columns ending in "_I"
resistance_cols <- grep("_I$", names(data), value = TRUE)

# Step 2: Coerce those columns to character (to interpret "Resistant", etc.)
data[resistance_cols] <- lapply(data[resistance_cols], as.character)

# Step 3: Keep only those columns where ≤90% of values are missing (i.e., ≥10% non-NA)
missing_fraction <- sapply(data[resistance_cols], function(col) mean(is.na(col)))
valid_resistance_cols <- names(missing_fraction[missing_fraction <= 0.90])

# Step 4: Extract Species, Country, and valid resistance columns
cols_to_extract <- c("Species", "Country", valid_resistance_cols)
model1_columns_only <- data[, cols_to_extract]

View(model1_columns_only)


