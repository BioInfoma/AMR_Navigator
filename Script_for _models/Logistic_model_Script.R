# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(broom)

# ==== STEP 1: Load full dataset and filter relevant columns ====

# Load large original dataset
data_LR <- read_csv("~/AMR NAVIGATOR/2025_03_11_atlas_antibiotics.csv")

# Identify resistance columns ending in "_I" with ≤90% missing
resistance_cols_lr <- grep("_I$", names(data_LR), value = TRUE)
valid_drugs_lr <- resistance_cols_lr[
  colMeans(is.na(data_LR[resistance_cols_lr])) <= 0.90
]

# Define predictor variables
predictors <- c("Species", "Country", "Gender", "Age Group")

# Extract and save smaller dataset
smaller_data <- data_LR %>%
  select(all_of(c(valid_drugs_lr, predictors))) %>%
  drop_na(Species, Country, Gender, `Age Group`)  # Drop rows with missing key predictors

write_csv(smaller_data, "logistic_model_data.csv")
print("✔ Smaller dataset saved as 'logistic_model_data.csv'")

# ==== STEP 2: Reload smaller dataset and run logistic models ====

# Load smaller dataset
small_data <- read_csv("logistic_model_data.csv")

# Storage for model comparisons
model_comparison <- list()

# Loop through each valid drug
for (drug in valid_drugs_lr) {
  message("Processing: ", drug)
  
  # Prepare data for this drug
  df <- small_data %>%
    select(all_of(c(drug, predictors))) %>%
    filter(!is.na(.data[[drug]])) %>%
    mutate(Response = ifelse(.data[[drug]] == "Resistant", 1, 0))
  
  # Skip if not enough data
  if (nrow(df) < 100) next
  
  # Fit logistic models with increasing complexity
  model_A <- glm(Response ~ Species, data = df, family = binomial)
  model_B <- glm(Response ~ Species + Country, data = df, family = binomial)
  model_C <- glm(Response ~ Species + Country + Gender, data = df, family = binomial)
  model_D <- glm(Response ~ Species + Country + Gender + `Age Group`, data = df, family = binomial)
  
  # Store AICs
  comparison <- data.frame(
    Drug = drug,
    Model = c("Species", "Species+Country", "Species+Country+Gender", "Species+Country+Gender+Age"),
    AIC = c(AIC(model_A), AIC(model_B), AIC(model_C), AIC(model_D)),
    N = nrow(df)
  )
  
  model_comparison[[drug]] <- comparison
}

# Combine all results into one data frame
model_results_df <- do.call(rbind, model_comparison)

# Save results
write.csv(model_results_df, "logistic_model_selection.csv", row.names = FALSE)

print("✔ Logistic model comparison complete and saved.")

#checking the best model


# Load results
model_results <- read_csv("logistic_model_selection.csv")

# Get the best model (lowest AIC) per drug
best_models <- model_results %>%
  group_by(Drug) %>%
  filter(AIC == min(AIC)) %>%
  ungroup()

# View it
print(best_models)

# Save it if needed
write_csv(best_models, "best_logistic_models.csv")

### Fitting full logistics regression


# Step 1: Load logistic model data and best model summary
data_LR_final <- read_csv("logistic_model_data.csv")
best_models <- read_csv("best_logistic_models.csv")

# Step 2: Initialize storage
final_model_outputs <- list()

# Step 3: Loop through each drug and fit the selected model
for (i in seq_len(nrow(best_models))) {
  drug <- best_models$Drug[i]
  formula_text <- best_models$Model[i]
  
  message("Fitting final logistic model for: ", drug)
  
  # Prepare data for this drug
  df <- data_LR_final %>%
    select(all_of(c(drug, "Species", "Country", "Gender", "Age Group"))) %>%
    filter(!is.na(.data[[drug]])) %>%
    mutate(Response = ifelse(.data[[drug]] == "Resistant", 1, 0))
  
  if (nrow(df) < 100) next  # skip underpowered models
  
  # Dynamically build formula
  formula_safe <- gsub("Age", "`Age Group`", formula_text)
  model_formula <- as.formula(paste("Response ~", formula_safe))
  
  # Fit model
  model_fit <- glm(model_formula, data = df, family = binomial)
  
  # Extract tidy results
  tidy_model <- broom::tidy(model_fit) %>%
    filter(term != "(Intercept)") %>%
    mutate(Drug = drug, Model = formula_text)
  
  final_model_outputs[[drug]] <- tidy_model
}

# Step 4: Combine and save all tidy model results
final_model_df <- bind_rows(final_model_outputs)

# Save full model summaries
write_csv(final_model_df, "logistic_model_coefficients.csv")

# Optional: save only significant predictors
sig_predictors <- final_model_df %>%
  filter(p.value < 0.05)

write_csv(sig_predictors, "significant_predictors_logistic.csv")

print("✔ Final logistic models fitted and results saved.")



