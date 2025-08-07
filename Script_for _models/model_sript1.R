# === LIBRARIES ===
library(readr)
library(dplyr)
library(forcats)
library(MASS)
library(stringr)
library(janitor)

# === LOAD AND CLEAN DATA ===
df <- read_csv("~/AMR NAVIGATOR/2025_03_11_atlas_antibiotics.csv") %>%
  clean_names()

# === DEFINE VALID AGE GROUPS ===
age_levels <- c(
  "0 to 2 Years", "3 to 12 Years", "13 to 18 Years",
  "19 to 64 Years", "65 to 84 Years", "85 and Over"
)

df <- df %>%
  filter(age_group %in% age_levels) %>%
  mutate(
    age_group = factor(age_group, levels = age_levels, ordered = TRUE),
    species = factor(species),
    country = factor(country),
    gender = factor(gender)
  )

# === IDENTIFY DRUG COLUMNS ENDING IN "_i" ===
drug_cols <- names(df)[str_ends(names(df), "_i")]

# === INITIALIZE MODEL STORAGE ===
model_list <- list()

# === LOOP THROUGH DRUGS ===
for (drug in drug_cols) {
  cat("\n=== Modeling:", drug, "===\n")
  
  df_temp <- df %>%
    filter(!is.na(.data[[drug]])) %>%
    mutate(
      resistance_status = factor(.data[[drug]],
                                 levels = c("Susceptible", "Intermediate", "Resistant"),
                                 ordered = TRUE),
      species = fct_lump(species, n = 20),
      country = fct_lump(country, n = 15)
    ) %>%
    droplevels()
  
  # DEBUG: print how many unique levels
  cat("Unique response levels for", drug, ":", levels(df_temp$resistance_status), "\n")
  
  # Only model if there are at least 3 levels in the outcome
  if (nlevels(df_temp$resistance_status) < 3) {
    cat("Skipped", drug, "- only", nlevels(df_temp$resistance_status), "level(s) present.\n")
    next
  }
  
  # Try to fit model
  model <- tryCatch({
    polr(resistance_status ~ age_group + species + country + gender,
         data = df_temp, Hess = TRUE)
  }, error = function(e) {
    cat("Model for", drug, "failed:", e$message, "\n")
    return(NULL)
  })
  
  # Save if successful
  if (!is.null(model)) {
    model_list[[drug]] <- model
    print(summary(model))
  }
}

library(car)
vif(model)

library(nnet)
model_check <- multinom(resistance_status ~ age_group + species + country + gender, data = df_temp)
vif(model_check)


#Binary lofistic  regression

# === LIBRARIES ===
library(readxl)
library(dplyr)
library(stringr)
library(janitor)
library(broom)
library(car)



# === DEFINE AGE GROUPS ===
age_levels <- c(
  "0 to 2 Years", "3 to 12 Years", "13 to 18 Years",
  "19 to 64 Years", "65 to 84 Years", "85 and Over"
)

# === FORMAT VARIABLES ===
df <- df %>%
  filter(age_group %in% age_levels) %>%
  mutate(
    age_group = factor(age_group, levels = age_levels, ordered = TRUE),
    species = factor(species),
    country = factor(country),
    gender = factor(gender)
  )

# === SELECT DRUGS ENDING IN "_i" ===
drug_cols <- names(df)[str_ends(names(df), "_i")]

# === INITIALIZE MODEL RESULTS LIST ===
binary_models <- list()

# === LOOP THROUGH DRUGS ===
for (drug in drug_cols) {
  cat("\n=== Modeling:", drug, "===\n")
  
  df_temp <- df %>%
    filter(!is.na(.data[[drug]])) %>%
    mutate(
      resistance_binary = ifelse(.data[[drug]] == "Resistant", 1, 0)
    )
  
  if (length(unique(df_temp$resistance_binary)) < 2 || nrow(df_temp) < 50) {
    cat("Skipped", drug, "- not enough data or no variation in response.\n")
    next
  }
  
  # Fit logistic regression model
  model_B <- tryCatch({
    glm(resistance_binary ~ age_group + species + country + gender,
        data = df_temp, family = binomial)
  }, error = function(e) {
    cat("Model for", drug, "failed:", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(model)) {
    binary_models[[drug]] <- model
    print(summary(model_B))            # Model summary
    print(exp(coef(model_B)))          # Odds ratios
    print(vif(model))                # Multicollinearity check
  }
}
