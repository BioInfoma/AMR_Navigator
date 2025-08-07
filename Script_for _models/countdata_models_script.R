# === COUNT MODEL BY SPECIES ===
library(dplyr)
library(MASS)
library(ggplot2)
library(readr)

# Initialize storage
species_results_list <- list()

# Loop through each drug
for (drug in names(model1_columns_only)[-(1:2)]) {
  
  cat("\n=== Drug (Species Model):", drug, "===\n")
  
  filtered <- model1_columns_only %>%
    filter(.data[[drug]] == "Resistant")
  
  model_data <- filtered %>%
    group_by(Country, Species) %>%
    summarise(Resistant_Count = n(), .groups = "drop")
  
  if (nrow(model_data) < 5) next
  
  model <- glm(Resistant_Count ~ Species, family = poisson, data = model_data)
  dispersion <- sum(residuals(model, type = "pearson")^2) / df.residual(model)
  
  if (dispersion > 1.5) {
    model_nb <- tryCatch(glm.nb(Resistant_Count ~ Species, data = model_data), error = function(e) NULL)
    if (!is.null(model_nb)) {
      model_data$Predicted_Count <- predict(model_nb, type = "response")
    }
  } else {
    model_data$Predicted_Count <- predict(model, type = "response")
  }
  
  species_summary <- model_data %>%
    group_by(Drug = drug, Species) %>%
    summarise(
      Total_Observed = sum(Resistant_Count),
      Total_Predicted = sum(Predicted_Count),
      .groups = "drop"
    )
  
  species_results_list[[drug]] <- species_summary
}

# Combine and save results
species_results <- bind_rows(species_results_list)
write_csv(species_results, "count_model_species.csv")
cat("✅ Species-level count model results saved to 'count_model_species.csv'\n")


# === COUNT MODEL BY COUNTRY ===
library(dplyr)
library(MASS)
library(ggplot2)
library(readr)

# Initialize storage
country_results_list <- list()

# Loop through each drug
for (drug in names(model1_columns_only)[-(1:2)]) {
  
  cat("\n=== Drug (Country Model):", drug, "===\n")
  
  filtered <- model1_columns_only %>%
    filter(.data[[drug]] == "Resistant")
  
  model_data <- filtered %>%
    group_by(Species, Country) %>%
    summarise(Resistant_Count = n(), .groups = "drop")
  
  if (nrow(model_data) < 5) next
  
  model <- glm(Resistant_Count ~ Country, family = poisson, data = model_data)
  dispersion <- sum(residuals(model, type = "pearson")^2) / df.residual(model)
  
  if (dispersion > 1.5) {
    model_nb <- tryCatch(glm.nb(Resistant_Count ~ Country, data = model_data), error = function(e) NULL)
    if (!is.null(model_nb)) {
      model_data$Predicted_Count <- predict(model_nb, type = "response")
    }
  } else {
    model_data$Predicted_Count <- predict(model, type = "response")
  }
  
  country_summary <- model_data %>%
    group_by(Drug = drug, Country) %>%
    summarise(
      Total_Observed = sum(Resistant_Count),
      Total_Predicted = sum(Predicted_Count),
      .groups = "drop"
    )
  
  country_results_list[[drug]] <- country_summary
}

# Combine and save results
country_results <- bind_rows(country_results_list)
write_csv(country_results, "count_model_country.csv")
cat("✅ Country-level count model results saved to 'count_model_country.csv'\n")
