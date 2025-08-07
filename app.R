# ============================================================
# 🧬 AMR MULTI-PAGE DASHBOARD - Vivli x AMR Hackathon 2025
# File: app.R
# Authors: Kolawole Temitope, Onuka Favor, Akiniyin Afeez, Fawole Elijah, Dr Okewole.
# Description:
#     An interactive Shiny dashboard to visualize and analyze
#     antimicrobial resistance (AMR) using Vivli's AMR surveillance data.
#     Includes resistance maps, logistic models, country trends,
#     species-specific insights, and a recommendation engine.
# ============================================================

# --- Load Required Libraries ---
library(shiny)           # Core Shiny app framework
library(dplyr)           # Data manipulation
library(readr)           # CSV file reading
library(sf)              # Spatial (geographic) data handling
library(ggplot2)         # Base plotting
library(rnaturalearth)   # Country shapefiles for map
library(rnaturalearthdata)
library(scico)           # Scientific color palettes
library(DT)              # Interactive data tables
library(tidyr)           # Data reshaping
library(forcats)         # Factor manipulation
library(plotly)          # Interactive plots
library(scales)          # Nice axis scales
library(shinythemes)     # Themed UI
library(rsconnect)       # Deployment (optional)


# --- Load Input CSV Files ---
# Ensure these files are in the same directory as app.R
res_data      <- read_csv("spatial_resistance_data.csv")  # Full resistance dataset
log_all       <- read_csv("logistic_model_coefficients.csv")  # All logistic model coefficients
log_sig       <- read_csv("significant_predictors_logistic.csv")  # Significant predictors only
species_count <- read_csv("count_model_species.csv")  # Predicted species counts
country_count <- read_csv("count_model_country.csv")  # Predicted country counts

# Load world shapefile for geographic mapping
world <- ne_countries(scale = "medium", returnclass = "sf")


# --- Preprocessing for Filter Inputs and Visuals ---
drug_cols     <- grep("_I$", names(res_data), value = TRUE)  # Identify drug columns
species_list  <- unique(res_data$Species)
country_list  <- unique(res_data$Country)
years         <- sort(unique(res_data$Year))

# Limit heavy visuals to top species/drugs for performance
top_species <- res_data %>%
  count(Species, sort = TRUE) %>%
  slice_head(n = 20) %>%
  pull(Species)

top_drugs <- res_data %>%
  pivot_longer(cols = all_of(drug_cols), names_to = "Drug", values_to = "Status") %>%
  count(Drug, sort = TRUE) %>%
  slice_head(n = 20) %>%
  pull(Drug)

# Precompute heatmap & bubble data
heatmap_data_all <- res_data %>%
  pivot_longer(cols = all_of(drug_cols), names_to = "Drug", values_to = "Status") %>%
  filter(Status == "Resistant",
         Species %in% top_species,
         Drug %in% top_drugs) %>%
  count(Species, Drug) %>%
  mutate(
    Drug = gsub("_I$", "", Drug),
    Species = factor(Species, levels = rev(sort(unique(Species)))),
    Drug = factor(Drug, levels = sort(unique(Drug)))
  )

bubble_data_all <- heatmap_data_all  # Same as heatmap data

# Precompute "By Species" data for general visualisation
species_resistant_data <- res_data %>%
  pivot_longer(cols = all_of(drug_cols), names_to = "Drug", values_to = "Status") %>%
  filter(Status == "Resistant") %>%
  count(Species) %>%
  top_n(20, n)

# --- UI Definition ---
# Main layout with navbar and multiple panels:
# 1. Dashboard: Visual filters + spatial maps
# 2. Major Findings: Summary tables from models
# 3. Trends: Time series by country/species
# 4. Visualizations: Generic plots (heatmap, bubble)

tags$script("$(function () { $('[data-toggle=\"tooltip\"]').tooltip(); })")
ui <- navbarPage(
  title = "🧬 AMR Recommendation Engine",
  theme = shinythemes::shinytheme("flatly"), # Applied the flatly theme
  
  
  # Main visual and control dashboard
  tabPanel("📊 Dashboard",
           sidebarLayout(
             sidebarPanel(
               helpText(HTML("Explore Antimicrobial Resistance (AMR) data using interactive tools. Select options to view spatial maps, count models, and logistic predictors. <br><br> <strong>Tip:</strong> Use the tabs in the main panel to switch between different visualizations.")),
               hr(),
               h4("Global Filters"), # Added section title
               selectInput("drug", "🧪 Select Drug:", choices = drug_cols, selected = drug_cols[1]),
               selectInput("year", "📅 Select Year:", choices = years, selected = years[1]),
               selectInput("species", "🦠 Select Species:", choices = species_list, selected = species_list[1]),
               selectInput("country", "🌍 Select Country:", choices = country_list, selected = country_list[1]),
               hr(),
               h4("Logistic Model Filters"),
               selectInput("log_drug", "📊 Logistic Drug:", choices = unique(log_all$Drug)),
               numericInput("top_n_predictors", "Show Top N Predictors:", value = 20, min = 5, max = 100, step = 5) # New input for filtering
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("🗺 Resistance Map",
                          h4("Geographic Distribution of Resistance"), # Added section title
                          helpText("Choropleth map showing resistance rate per country for the selected drug and year. Darker shades indicate higher resistance rates. Country names are labeled on the map."),
                          hr(),
                          # KEPT AS  PLOTLY HERE for mapPlot
                          plotlyOutput("mapPlot", height = "700px")
                 ),
                 tabPanel("📈 Count by Species",
                          h4("Observed vs. Predicted Resistance Counts by Species"),
                          helpText("This bar chart compares the observed (actual) and predicted resistance counts for top species. The table below provides full details."),
                          hr(),
                          plotlyOutput("countSpeciesPlot", height = "400px"), # ✅ This is correct
                          br(),
                          DTOutput("countSpecies")
                 ),
                 tabPanel("📉 Count by Country",
                          h4("Observed vs. Predicted Resistance Counts by Country"),
                          helpText("This bar chart compares the observed (actual) and predicted resistance counts for top countries. The table below provides full details."),
                          hr(),
                          plotlyOutput("countCountryPlot", height = "400px"), # ✅ Also correct
                          br(),
                          DTOutput("countCountry")
                 ),
                 tabPanel("🧪 Logistic Viewer",
                          h4("Significant Predictors of Resistance"), # Added section title
                          helpText("View significant predictors (p < 0.05) and estimate effects on resistance (log odds)."),
                          hr(),
                          plotlyOutput("logPlot"), # Changed to plotlyOutput
                          DTOutput("logTable")
                 ),
                 tabPanel("✅ Recommendation",
                          tags$h4("Antibiotic Susceptibility Recommendations",
                                  tags$span(
                                    icon("info-circle"),
                                    `data-toggle` = "tooltip",
                                    title = "Susceptibility shows the predicted probability of effectiveness from the logistic model. P-values indicate the model’s confidence in species/country effects. A high p-value doesn’t mean low susceptibility — just less certainty."
                                  )
                          )
                          , # Added section title
                          helpText("Top recommended antibiotics based on susceptibility rate for the selected species and country."),
                          hr(),
                          div(
                            strong(textOutput("recommendCaption")),
                            tableOutput("recommendTable")
                          )
                 )
               )
             )
           )
  ),
  
  tabPanel("📌 Major Findings",
           fluidPage(
             h3("Key Insights from AMR Data Analysis"), # Added section title
             p("This section presents overarching findings from the logistic models, highlighting the best-fitting models and the most consistently significant predictors of antimicrobial resistance across various drugs."),
             hr(),
             h4("Best Logistic Models by AIC"),
             helpText("Displays the best logistic model configuration per drug based on lowest AIC (model fit criteria)."),
             DTOutput("logBestModels"),
             br(), # Added break
             h4("Top Significant Predictors"),
             helpText("These variables showed statistically significant effects on resistance across different drugs."),
             DTOutput("sigPredictorsTable")
             
           )
           
  ),
  
  
  tabPanel("📈 Trends & Time Series",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               helpText("Explore resistance trends over time. The system automatically filters for data quality."),
               hr(),
               h4("Trend Filters"), # Added section title
               selectInput("trend_drug",
                           "🧪 Select Drug",
                           choices = drug_cols
               ),
               selectInput("trend_species",
                           "🦠 Select Species",
                           choices = species_list
               ),
               hr(),
               div( # Enhanced info box with shiny HTML helpers
                 style = "background-color: #e3f2fd; padding: 15px; border-radius: 8px; border-left: 5px solid #2196F3;",
                 h5("📊 Data Filters Applied:", style = "color: #1976D2; margin-top: 0;"),
                 tags$ul(style = "margin-bottom: 0; padding-left: 20px;"),
                 tags$li("<strong>Country Trends:</strong> Countries: ≥3 years of data & ≥20 total isolates"),
                 tags$li("<strong>Comparison:</strong> Top 15 countries by sample size"),
                 tags$li("<strong>Latest Year Data:</strong> For country ranking")
               )
             ),
             
             mainPanel(
               width = 9,
               tabsetPanel(
                 id = "trendTabs",
                 
                 tabPanel("📈 Overall Trend",
                          br(),
                          h4("Overall Resistance Trend Over Time"), # Added section title
                          helpText("Blue line: observed trend | Red band: smoothed trend with confidence interval"),
                          plotOutput("trendPlot", height = "450px"),
                          hr(),
                          h4("📋 Trend Summary"), # Added section title
                          DTOutput("trendSummary")
                 ),
                 
                 tabPanel("🌍 Country Trends",
                          br(),
                          div( # Enhanced info box
                            style = "background-color: #fff3cd; padding: 15px; border-radius: 8px; margin-bottom: 15px; border-left: 5px solid #ffc107;",
                            strong("💡 Interactive Plot: "),
                            "Hover over lines for details. Use legend to show/hide countries. Zoom and pan enabled."
                          ),
                          h4("Resistance Trends by Country"), # Added section title
                          plotlyOutput("countryTrendPlot", height = "500px") # This remains plotly
                 ),
                 
                 tabPanel("🏆 Country Ranking",
                          br(),
                          div( # Enhanced info box
                            style = "background-color: #e0f7fa; padding: 15px; border-radius: 8px; margin-bottom: 15px; border-left: 5px solid #00bcd4;",
                            strong("📊 Latest Year Comparison: "),
                            "Shows most recent resistance rates by country with sufficient data."
                          ),
                          h4("Country Resistance Ranking (Latest Year)"), # Added section title
                          plotlyOutput("countryComparisonPlot", height = "500px")
                          
                 )
               )
             )
           )
  ),
  
  tabPanel("📊 General Visualizations",
           fluidPage(
             h3("Overview of Resistance"), # Original title
             tabsetPanel(
               tabPanel("By Drug",
                        br(),
                        h4("Total Resistance Counts by Drug"), # Added section title
                        plotOutput("overviewPlot")
               ),
               tabPanel("By Species",
                        br(),
                        h4("Top Resistant Species (Across Drugs)"), # Added section title
                        plotOutput("speciesResistantPlot")
               ),
               tabPanel("Drug-Species Heatmap",
                        br(),
                        h4("Drug-Species Resistance Heatmap"), # Added section title
                        plotlyOutput("heatmapPlot") # This remains plotly
               ),
               tabPanel("Bubble Plot",
                        br(),
                        h4("Drug-Species Resistance Bubble Plot"), # Added section title
                        plotlyOutput("bubblePlot") # This remains plotly
               )
             )
           ),
           tags$footer(
             tags$div(
               style = "font-size: 12px; color: gray; text-align: center; padding: 10px; margin-top: 20px; border-top: 1px solid #ccc;",
               "Data Source: 2025 Vivli AMR Surveillance Data Challenge, Pfizer Atlas Dataset."
             )
           )
           
  )
)

# --- Server Logic ---
# Handles reactive behavior, plots, and data outputs.
# Organized by output type: maps, bar charts, models, trends, recommendations.

server <- function(input, output, session) {
  
  # --- Resistance Map Output ---
  # Shows geographic distribution of resistance for a given drug/year

  output$mapPlot <- renderPlotly({
    # req() ensures these inputs are available before running the code
    req(input$drug, input$year)
    
    df <- res_data %>% filter(!is.na(.data[[input$drug]]), Year == input$year)
    df_sum <- df %>%
      group_by(Country) %>%
      summarise(
        Total = n(),
        Resistant = sum(.data[[input$drug]] == "Resistant", na.rm = TRUE),
        Rate = Resistant / Total,
        .groups = "drop"
      )
    
    map_df <- left_join(world, df_sum, by = c("name" = "Country")) %>% filter(!is.na(Rate))
    
    p <- ggplot(map_df) +
      geom_sf(aes(
        fill = Rate,
        # 'text' aesthetic is used by ggplotly for tooltips
        text = paste(
          "Country:", name,
          "<br>Resistance Rate:", scales::percent(Rate, accuracy = 0.1),
          "<br>Total Isolates:", Total
        )
      ), color = "white", linewidth = 0.2) +
      # Changed palette for better contrast (previously "lajolla" in the non-working version)
      scale_fill_scico(
        palette = "lajolla",
        direction = 1,
        na.value = "grey90",
        name = "Resistance Rate (%)",
        labels = scales::percent_format(accuracy = 1)
      ) +
      labs(
        title = paste("Resistance Rate for", gsub("_I$", "", input$drug), "in", input$year),
        subtitle = "Darker shades indicate higher resistance rates. Hover for details."
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray50"),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()
      )
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
  })
  
  # --- Count by Species Output ---
  # Plot and table showing top predicted resistance counts by species
  output$countSpeciesPlot <- renderPlotly({
    req(input$drug)
    
    p <- species_count %>%
      filter(Drug == input$drug) %>%
      arrange(desc(Total_Predicted)) %>%
      slice_head(n = 15) %>%
      ggplot(aes(x = reorder(Species, Total_Predicted), y = Total_Predicted,
                 text = paste("Species:", Species, "<br>Predicted:", round(Total_Predicted)))) +
      geom_col(fill = "#4e79a7") +
      coord_flip() +
      labs(
        title = paste("Top Species (Predicted Counts) for", gsub("_I$", "", input$drug)),
        x = "Species", y = "Total Predicted Resistance"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.y = element_text(size = 10)
      )
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
  })
  
  
  output$countSpecies <- renderDT({
    # CRITICAL: req() added for robustness
    req(input$drug)
    species_count %>%
      filter(Drug == input$drug) %>%
      arrange(desc(Total_Predicted)) %>%
      datatable(options = list(pageLength = 10))
  })
  
  # --- Count by countries Output ---
  # Plot and table showing top predicted resistance counts by country
  
  output$countCountryPlot <- renderPlotly({
    req(input$drug)
    
    p <- country_count %>%
      filter(Drug == input$drug) %>%
      arrange(desc(Total_Predicted)) %>%
      slice_head(n = 15) %>%
      ggplot(aes(x = reorder(Country, Total_Predicted), y = Total_Predicted,
                 text = paste("Country:", Country, "<br>Predicted:", round(Total_Predicted)))) +
      geom_col(fill = "#f28e2b") +
      coord_flip() +
      labs(
        title = paste("Top Countries (Predicted Counts) for", gsub("_I$", "", input$drug)),
        x = "Country", y = "Total Predicted Resistance"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.y = element_text(size = 10)
      )
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
  })
  
  
  output$countCountry <- renderDT({
    # CRITICAL: req() added for robustness
    req(input$drug)
    country_count %>%
      filter(Drug == input$drug) %>%
      arrange(desc(Total_Predicted)) %>%
      datatable(options = list(pageLength = 10))
  })
  
  # Logistic Plot
  # Logistic Plot (Now interactive with Plotly and N-filter)
  output$logPlot <- renderPlotly({
    # CRITICAL: req() added for robustness for all inputs used
    req(input$log_drug, input$top_n_predictors)
    
    df_filtered <- log_all %>%
      filter(Drug == input$log_drug, term != "(Intercept)") %>% # Exclude intercept
      # Calculate absolute estimate for sorting and filter by significance
      mutate(abs_estimate = abs(estimate)) %>%
      filter(p.value < 0.05) %>% # Only significant predictors
      arrange(desc(abs_estimate)) %>% # Order by absolute estimate for top N
      slice_head(n = input$top_n_predictors) # Take top N
    
    if (nrow(df_filtered) == 0) {
      return(
        plotly_empty(type = "scatter", mode = "markers") %>%
          layout(title = list(text = paste("No significant predictors (p < 0.05) found for", gsub("_I$", "", input$log_drug), "or none within top N selection."),
                              yref = "paper", y = 0.5))
      )
    }
    
    # Ensure terms are reordered for the plot
    df_filtered <- df_filtered %>%
      mutate(term = fct_reorder(term, estimate))
    
    # Create ggplot for plotly conversion
    p <- ggplot(df_filtered, aes(x = term, y = estimate, fill = estimate > 0,
                                 # Tooltip information for plotly
                                 text = paste(
                                   "Predictor:", term,
                                   "<br>Estimate (Log Odds):", round(estimate, 3),
                                   "<br>Std. Error:", round(std.error, 3),
                                   "<br>P-value:", format.pval(p.value, digits = 3)
                                 ))) +
      geom_col(show.legend = FALSE) +
      # Use geom_text for p-value significance stars directly on plot
      geom_text(aes(label = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        TRUE ~ ""
      )), hjust = -0.1, size = 3, color = "black") + # Adjust hjust if labels overlap
      coord_flip() +
      labs(
        title = paste("Top", nrow(df_filtered), "Significant Predictors for", gsub("_I$", "", input$log_drug)),
        subtitle = "Bars show log odds. * p<0.05, ** p<0.01, *** p<0.001",
        x = "Predictor",
        y = "Estimate (Log Odds of Resistance)"
      ) +
      theme_minimal(base_size = 14) +
      scale_fill_manual(values = c("TRUE" = "#1a9850", "FALSE" = "#d73027")) + # Green for positive, Red for negative
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray60"),
        axis.text.y = element_text(size = 10), # Adjust font size for better fit
        axis.text.x = element_text(size = 10),
        panel.grid.minor = element_blank()
      )
    
    # Convert to plotly and add hover info
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = TRUE, displaylogo = FALSE) # Add plotly config
  })
  
  # --- Recommendation Engine ---
  # Computes top antibiotic suggestions based on logistic model estimates
  
  output$recommendTable <- renderTable({
    req(input$species, input$country)
    
    get_term_info <- function(df, term_label) {
      row <- df %>% filter(term == term_label)
      if (nrow(row) == 0) return(list(estimate = 0, p_value = NA))
      return(list(estimate = row$estimate[1], p_value = row$p.value[1]))
    }
    
    recomm <- lapply(drug_cols, function(drug_col) {
      model_df <- log_all %>% filter(Drug == drug_col)
      if (nrow(model_df) == 0) return(NULL)
      
      intercept_info <- get_term_info(model_df, "(Intercept)")
      intercept <- intercept_info$estimate
      
      species_term <- paste0("Species", input$species)
      country_term <- paste0("Country", input$country)
      
      species_info <- get_term_info(model_df, species_term)
      country_info <- get_term_info(model_df, country_term)
      
      if (is.na(species_info$p_value) & is.na(country_info$p_value)) return(NULL)
      
      logit <- intercept + species_info$estimate + country_info$estimate
      prob_resistant <- 1 / (1 + exp(-logit))
      prob_susceptible <- round(1 - prob_resistant, 3)
      max_p <- round(max(c(species_info$p_value, country_info$p_value), na.rm = TRUE), 4)
      
      data.frame(
        Drug = gsub("_I$", "", drug_col),
        Susceptibility = prob_susceptible,
        Recommendation = case_when(
          prob_susceptible >= 0.8 ~ "🟢 High Efficacy",
          prob_susceptible >= 0.5 ~ "🟡 Moderate",
          TRUE ~ "🔴 Low"
        )
      )
    }) %>% bind_rows()
    
    if (nrow(recomm) == 0) {
      return(data.frame(
        Recommendation = "No logistic model or terms available for this Species-Country combination.",
        stringsAsFactors = FALSE
      ))
    }
    
    recomm %>%
      arrange(desc(Susceptibility)) %>%
      head(5)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # ✅ 2. Caption for the Recommendation Table
  output$recommendCaption <- renderText({
    req(input$species, input$country)
    paste("Top 5 Model-Based Recommendations for", input$species, "in", input$country)
  })
  
  # Best Logistic Models
  output$logBestModels <- renderDT({
    # No input dependency, so no req() needed
    log_all %>%
      select(Drug, Model, AIC) %>%
      distinct() %>%
      arrange(AIC) %>%
      datatable(options = list(pageLength = 15))
  })
  
  # Significant Predictors Table
  output$sigPredictorsTable <- renderDT({
    # No input dependency, so no req() needed
    log_sig %>%
      arrange(p.value) %>%
      datatable(options = list(pageLength = 15))
  })
  
  # === ENHANCED TREND PLOT OUTPUT ===
  output$trendPlot <- renderPlot({
    # CRITICAL: req() added for robustness
    req(input$trend_species, input$trend_drug)
    
    # Filter and summarize data
    trend_data <- res_data %>%
      filter(Species == input$trend_species, !is.na(.data[[input$trend_drug]])) %>%
      group_by(Year) %>%
      summarise(
        Rate = mean(.data[[input$trend_drug]] == "Resistant", na.rm = TRUE),
        Count = n(),
        Resistant_Count = sum(.data[[input$trend_drug]] == "Resistant", na.rm = TRUE),
        .groups = "drop"
      )
    
    # Validate data availability
    validate(
      need(nrow(trend_data) > 0, paste("No data available for this species-drug combination to show overall trend."))
    )
    
    # Create enhanced trend plot
    ggplot(trend_data, aes(x = Year, y = Rate)) +
      geom_line(color = "#1a759f", size = 1.4, alpha = 0.8) +
      geom_point(color = "#1a759f", size = 3, alpha = 0.9) +
      geom_smooth(
        method = "loess", se = TRUE, color = "#d73027",
        fill = "#d73027", alpha = 0.2, linewidth = 0.8
      ) +
      scale_y_continuous(
        labels = percent_format(),
        limits = c(0, max(trend_data$Rate, na.rm = TRUE) * 1.1)
      ) +
      labs(
        title = paste("Resistance Trend:", gsub("_I$", "", input$trend_drug), "vs", input$trend_species),
        subtitle = paste("Sample size ranges from", min(trend_data$Count), "to", max(trend_data$Count), "isolates per year"),
        x = "Year",
        y = "Resistance Rate",
        caption = "Blue line: observed trend | Red band: smoothed trend with confidence interval"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5), # Center title
        plot.subtitle = element_text(size = 12, color = "gray60", hjust = 0.5), # Center subtitle
        plot.caption = element_text(size = 10, color = "gray50"),
        panel.grid.minor = element_blank()
      )
  })
  
  # === ENHANCED COUNTRY TREND PLOT WITH FILTERING (PLOTLY) ===
  output$countryTrendPlot <- renderPlotly({
    # CRITICAL: req() added for robustness
    req(input$trend_species, input$trend_drug)
    
    # Calculate country-level trends with filtering
    country_trend_data <- res_data %>%
      filter(Species == input$trend_species, !is.na(.data[[input$trend_drug]])) %>%
      group_by(Country, Year) %>%
      summarise(
        Rate = mean(.data[[input$trend_drug]] == "Resistant", na.rm = TRUE),
        Count = n(),
        Resistant_Count = sum(.data[[input$trend_drug]] == "Resistant", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      # Filter countries with sufficient data
      group_by(Country) %>%
      filter(
        n() >= 3, # At least 3 years of data
        sum(Count) >= 20 # At least 20 total isolates
      ) %>%
      ungroup()
    
    # Validate data
    validate(
      need(nrow(country_trend_data) > 0,
           paste("No countries with sufficient data (≥3 years, ≥20 isolates) for this combination to show country trends.")
      )
    )
    
    # Get top countries by total sample size for better visibility
    top_countries <- country_trend_data %>%
      group_by(Country) %>%
      summarise(Total_Count = sum(Count), .groups = "drop") %>%
      arrange(desc(Total_Count)) %>%
      slice_head(n = 10) %>% # Show only top 10 countries
      pull(Country)
    
    # Filter to top countries
    filtered_data <- country_trend_data %>%
      filter(Country %in% top_countries)
    
    # Create interactive plot
    p <- plot_ly(
      filtered_data,
      x = ~Year,
      y = ~Rate,
      color = ~Country,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(width = 2),
      marker = list(size = 4),
      hovertemplate = paste(
        "<b>%{fullData.name}</b><br>",
        "Year: %{x}<br>",
        "Resistance Rate: %{y:.1%}<br>",
        "Sample Size: %{customdata}<br>",
        "<extra></extra>"
      ),
      customdata = ~Count
    ) %>%
      layout(
        title = list(
          text = paste("Top 15 Countries: Resistance Trends for",
                       gsub("_I$", "", input$trend_drug), "vs", input$trend_species
          ),
          font = list(size = 16)
        ),
        xaxis = list(title = "Year", tickmode = "linear", dtick = 1), # Ensure integer years
        yaxis = list(title = "Resistance Rate", tickformat = ".0%"),
        hovermode = 'closest',
        legend = list(
          orientation = "v",
          x = 1.02,
          y = 1,
          bgcolor = 'rgba(255,255,255,0.8)',
          bordercolor = 'rgba(0,0,0,0.2)',
          borderwidth = 1
        ),
        margin = list(r = 150)
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
    
    return(p)
  })
  
  # === COUNTRY COMPARISON PLOT (PERCENTAGE + PLOTLY) ===
  output$countryComparisonPlot <- renderPlotly({
    req(input$trend_species, input$trend_drug)
    
    latest_year <- max(res_data$Year, na.rm = TRUE)
    
    # Prepare country-level percentage data
    country_data <- res_data %>%
      filter(
        Species == input$trend_species,
        Year == latest_year,
        !is.na(.data[[input$trend_drug]])
      ) %>%
      group_by(Country) %>%
      summarise(
        ResistanceRate = mean(.data[[input$trend_drug]] == "Resistant", na.rm = TRUE),
        Count = n(),
        .groups = "drop"
      ) %>%
      filter(Count >= 5) %>% # keep countries with enough samples
      arrange(desc(ResistanceRate))
    
    validate(
      need(nrow(country_data) > 0,
           paste("No valid data for", input$trend_species,
                 "and", gsub("_I$", "", input$trend_drug),
                 "in", latest_year))
    )
    
    # Plotly horizontal bar chart
    plot_ly(
      country_data,
      x = ~ResistanceRate,
      y = ~reorder(Country, ResistanceRate),
      type = "bar",
      orientation = "h",
      text = ~paste0(
        "Country: ", Country,
        "<br>Resistance Rate: ", scales::percent(ResistanceRate, accuracy = 0.1),
        "<br>Samples: ", Count
      ),
      hoverinfo = "text",
      marker = list(color = "#4e79a7")
    ) %>%
      layout(
        title = paste("Country Ranking by Resistance Rate in", latest_year),
        xaxis = list(title = "Resistance Rate", tickformat = "%"),
        yaxis = list(title = ""),
        margin = list(l = 100)
      )
  })
  
  
  # === TREND SUMMARY TABLE ===
  output$trendSummary <- renderDT({
    # CRITICAL: req() added for robustness
    req(input$trend_species, input$trend_drug)
    
    # Calculate trend statistics
    trend_summary <- res_data %>%
      filter(Species == input$trend_species, !is.na(.data[[input$trend_drug]])) %>%
      group_by(Country) %>%
      summarise(
        Years_Available = n_distinct(Year),
        Total_Isolates = n(),
        Resistant_Isolates = sum(.data[[input$trend_drug]] == "Resistant", na.rm = TRUE),
        Overall_Rate = round(mean(.data[[input$trend_drug]] == "Resistant", na.rm = TRUE) * 100, 1),
        First_Year = min(Year),
        Last_Year = max(Year),
        .groups = "drop"
      ) %>%
      filter(Years_Available >= 2, Total_Isolates >= 10) %>%
      arrange(desc(Total_Isolates))
    
    # Validate data
    validate(
      need(nrow(trend_summary) > 0, paste("No countries with sufficient data for summary for this combination."))
    )
    
    datatable(
      trend_summary,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = 'frtip'
      ),
      colnames = c(
        "Country", "Years of Data", "Total Isolates", "Resistant Isolates",
        "Resistance Rate (%)", "First Year", "Last Year"
      ),
      caption = paste("Data availability summary for", input$trend_species,
                      "and", gsub("_I$", "", input$trend_drug)
      )
    ) %>%
      formatStyle(
        "Overall_Rate",
        backgroundColor = styleInterval(c(25, 50, 75),
                                        c("#a6d96a", "#ffffbf", "#fdae61", "#d73027")
        )
      )
  })
  
  # General Visualizations - By Drug (Original ggplot)
  output$overviewPlot <- renderPlot({
    # No input dependency, so no req() needed
    res_data_long <- res_data %>%
      pivot_longer(cols = all_of(drug_cols), names_to = "Drug", values_to = "Status")
    res_data_long %>%
      filter(Status == "Resistant") %>%
      count(Drug) %>%
      ggplot(aes(x = reorder(Drug, n), y = n)) +
      geom_col(fill = "#4e79a7") +
      coord_flip() +
      labs(title = "Total Resistance Counts by Drug", x = "Drug", y = "Count") +
      theme_minimal(base_size = 15) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) # Center title
  })
  
  # === General Visualisations - By Species ===
  output$speciesResistantPlot <- renderPlot({
    ggplot(species_resistant_data, aes(x = reorder(Species, n), y = n)) +
      geom_col(fill = "#f28e2b") +
      coord_flip() +
      labs(title = "Top 20 Resistant Species (across drugs)", 
           x = "Species", 
           y = "Resistant Count") +
      theme_minimal(base_size = 15) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  # === Heatmap ===
  output$heatmapPlot <- renderPlotly({
    p <- ggplot(heatmap_data_all, aes(
      x = Drug, y = Species, fill = n,
      text = paste("Drug:", Drug,
                   "<br>Species:", Species,
                   "<br>Resistant Count:", n)
    )) +
      geom_tile(color = "white", linewidth = 0.5) +
      scale_fill_scico(palette = "lajolla", direction = 1, name = "Resistant Count") +
      labs(title = "Top 20 Drug–Species Resistance Heatmap",
           subtitle = "Each square shows resistant isolate counts",
           x = "Drug", y = "Species") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray50"),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = TRUE, displaylogo = FALSE)
  })
  
  # === Bubble Plot ===
  output$bubblePlot <- renderPlotly({
    p <- ggplot(bubble_data_all, aes(
      x = Drug, y = Species, size = n, color = n,
      text = paste("Drug:", Drug,
                   "<br>Species:", Species,
                   "<br>Resistant Count:", n)
    )) +
      geom_point(alpha = 0.7) +
      scale_size_area(max_size = 15, name = "Resistant Count") +
      scale_color_scico(palette = "batlow", direction = 1, name = "Resistant Count") +
      labs(title = "Resistance Bubble Plot by Drug and Species",
           subtitle = "Bubble size & color = resistant isolate counts",
           x = "Drug", y = "Species") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray50"),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = TRUE, displaylogo = FALSE)
  })
  
}

# === RUN APP ===
shinyApp(ui, server)
