# 🧬 AMR Navigator: Interactive Dashboard for Antimicrobial Resistance

**Project Submission for the 2025 Vivli x Pfizer AMR Surveillance Data Challenge**

An interactive R Shiny dashboard to visualize and analyze global antimicrobial resistance (AMR). Built for clinicians, researchers, and policymakers, the dashboard integrates statistical modeling and visual analytics to support data-driven decisions in AMR management.

---

## 🌐 Live Dashboard

👉 [Launch the Dashboard](https://bioinforma.shinyapps.io/amr_deploy/)

---

## 📊 Dashboard Features

- **📍 Resistance Map**: Interactive choropleth map by drug and year
- **📈 Count Models**: Observed vs. predicted resistance by species and country
- **🧪 Logistic Predictors**: View significant predictors of resistance
- **✅ Antibiotic Recommendations**: Top drugs for each species-country pair
- **📅 Trends**: Explore AMR trends across years and countries
- **🔍 General Visualizations**: Heatmaps and bubble plots for resistance patterns

---

## 📁 Data & Methods

### 🔹 Data Source
This project uses the **2025 Pfizer ATLAS AMR dataset**, made available via the [Vivli AMR Surveillance Challenge](https://vivli.org/). It contains global susceptibility test results from clinical isolates with metadata including species, year, country, age group, and gender.

### 🔹 Analytical Methods

**Data Preprocessing**
- Kept isolates with interpretable susceptibility results (columns ending in `_I`)
- Removed antibiotics with >90% missing values
- Resistance status was binarized: "Resistant" = 1, all else = 0

**Modeling Approaches**
- **Count Models**: Poisson or Negative Binomial models to predict resistance counts by species and country
- **Logistic Models**: Multiple logistic regression models per antibiotic using species, country, age, and gender as predictors. Best model selected by AIC.
- **Recommendation Engine**: Uses logistic coefficients to rank drugs by predicted susceptibility probability

**Visualizations**
- Built with `ggplot2`, `plotly`, `sf`, and `rnaturalearth` for maps and trends
- Includes time series plots, maps, bar charts, heatmaps, and interactive tables

📄 *See [Methods Summary](./_Methods_amr.docx) for detailed methodology.*

---

## 🚀 How to Run Locally

```r
# 1. Clone this repository
git clone https://github.com/BioInfoma/AMR_Navigator.git
cd AMR_Navigator

# 2. Install dependencies
install.packages(c("shiny", "dplyr", "ggplot2", "plotly", "scico", "DT", "sf", "readr",
                   "rnaturalearth", "rnaturalearthdata", "shinythemes", "forcats", "tidyr",
                   "broom", "MASS"))

# 3. Run the app
shiny::runApp("app.R")
```
🔎 Make sure the required CSV data files are placed in the same directory as app.R.

👥 Authors
Kolawole Temitope

Onuka Favor

Akiniyin Afeez

Fawole Elijah

Dr. Okewole

📄 License
This project is licensed under the 2025 Vivli Data Challenge.

🙏 Acknowledgments
Vivli & Pfizer – for data access and organizing the AMR Surveillance Challenge

HackBio – for providing the server to run the Analysis

RStudio + Shiny Community – for enabling rapid dashboard development
