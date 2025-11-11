# Loading packages and initial data exploration --------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,  
  here,    
  lubridate,   
  skimr,       
  glue,       
  gridExtra,        
  scales       
) 
options(scipen = 999, digits = 3)

# Data exploration and contextualization 
  # Import and quick structure and skim
  covid_raw <- read_csv(here("data","covid_full_data.csv"))
  # Check the overall structure and variable types of the loaded data
  glimpse(covid_raw)
  # Provide an overview including descriptive statistics and missing data counts
  skim(covid_raw)

# Tidy date variable
covid_tidy <- covid_raw %>% 
  mutate(date = ymd(date))

glimpse(covid_tidy)

# Calculate summary statistics for all numeric variables
numeric_summary <- covid_tidy %>%
  select(where(is.numeric)) %>%
  imap_dfr(~ tibble(
    variable = .y,
    n_missing = sum(is.na(.x)),
    mean = mean(.x, na.rm = TRUE),
    sd = sd(.x, na.rm = TRUE)
  ))

# Display summary table
numeric_summary

# 1B. Data processing and functional programming --------------------------------------------------

# Operation 1: Filter out aggregate regions (geographic, economic, political, global) and change column names
covid_processed <- covid_tidy %>%
  # Use filter() to remove aggregate regions in the original data source
  filter(!location %in% c(
    "World",
    "Africa", "Asia", "Europe", "North America", "South America", "Oceania",
    "High-income countries", "Upper-middle-income countries", 
    "Lower-middle-income countries", "Low-income countries",
    "European Union (27)"
  )) %>%
  # Use dplyr::rename()
  rename(country = location)

# Display the first five observations of the processed data
covid_processed %>% head(5) %>%
  kableExtra::kbl(caption = "First five observations after filtering regional aggregates") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))


# Operation 2: Functional programming for robust rate calculation (when 0, return 0)
safe_rate <- function(numerator, denominator) {
  # Use dplyr if_else() for type-safe vectorised conditional computation 
  if_else(denominator == 0, 0, numerator / denominator)
}


#Operation 3: Create New Variables and Dealing with NAs (3+ Operations)

analysis_data <- covid_processed %>% 
  # 1. Recode Variables: extract year and month for grouping/summarization
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date, label = TRUE)
  ) %>%
  # 2. Deal with NAs: Handle missing values in core daily metrics 
  mutate(
    across(
      .cols = c(new_cases, new_deaths),
      .fns  = ~ replace_na(.x, 0),
      .names = "{.col}_clean"
    )
  ) %>%
  # 3. Create New Variables: Calculate key social science metrics
  mutate(
    # Death Rate: Total deaths divided by total cases, uses the robust safe_rate function
    death_rate = safe_rate(total_deaths, total_cases),
    # New Case Rate (Daily Growth/Incidence Rate):New cases today divided by the *previous day's* total cases (using lag()).
    new_case_rate = safe_rate(new_cases_clean, total_cases)
  ) %>% 
  # 4. Selecting Variables: Final step to streamline the dataset.  
  select(
    country, year, month, date,
    new_cases, new_cases_clean,
    new_deaths, new_deaths_clean,
    total_cases, total_deaths,
    death_rate, new_case_rate
  )


# Display the first five observations of the final analysis data
analysis_data %>% head(5) %>%
  kableExtra::kbl(caption = "First five rows after recoding and variable creation") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))



# Operation 4: Define a complete processing function: Data Aggregation by Month and Country + Reshape Data from Wide to Long Format to prepare for visualization

aggregate_and_tidy <- function(data) {
  
  # 1.Aggregation (Reduce noise from daily data)
  monthly_data <- data %>%
    group_by(country, year, month) %>%
    summarise(
      avg_new_cases = mean(new_cases_clean, na.rm = TRUE),
      avg_case_rate = mean(new_case_rate, na.rm = TRUE),
      total_deaths_month = sum(new_deaths_clean, na.rm = TRUE),
      .groups = 'drop' 
    )
  
  # 2.Reshaping (Tidy Data Conversion)
  monthly_data %>%
    pivot_longer(
      cols = starts_with("avg_"),
      names_to = "metric_type",
      values_to = "metric_value"
    )
}

# This call achieves aggregation and tidying 
analysis_long_rates <- aggregate_and_tidy(analysis_data)
glimpse(analysis_long_rates)

# Display the first five observations of the reshaped data
analysis_long_rates %>% head(5) %>%
  kableExtra::kbl(caption = "Excerpt of monthly aggregated and reshaped data") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))


# Operation 5: Final Data Wrangling Step

# Functional Programming: function to calculate Global Daily Averages
calculate_global_daily_trends <- function(data) {
  
  data %>%
    # Group by the temporal unit (date) to aggregate across all countries
    group_by(date) %>%

    # Calculate the mean rate across all countries for that date
    summarise(
      global_avg_case_rate = mean(new_case_rate, na.rm = TRUE),
      global_death_rate = mean(death_rate, na.rm = TRUE),
      
      # Drop grouping structure
      .groups = 'drop'
    )
}

# Apply function to prepare Global Time Series Data for Visualization
plot_data_global <- calculate_global_daily_trends(analysis_data)

# Display the first five observations of the global daily averages
lot_data_global %>% head(5) %>%
  kableExtra::kbl(caption = "First five rows of global daily averages") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))


# 2A. Data Visualization and Functional Programming --------------------------------------------------

# Visualization 1: Global Daily Case Rate Trend
plot_v1_global_rate <- plot_data_global %>%
  ggplot(aes(x = date, y = global_avg_case_rate)) +
  geom_line(color = "lightblue", linewidth = 0.8) +
  geom_smooth(method = "loess", se = TRUE, color = "red", linetype = "dashed") +
  scale_y_log10() +
  labs(
    title = "Global Average Daily New Case Incidence Rate (2020–2024)",
    subtitle = "Smoothed trend highlights global pandemic waves (log scale)",
    x = "Date",
    y = "Average Daily Case Incidence Rate (log scale)"
  ) +
  theme_minimal()

plot_v1_global_rate


# Visualization 2: Top 10 Countries by Total Cumulative Deaths

# 1.Aggregate to find the final cumulative total deaths (using daily analysis_data)
top_10_countries <- analysis_data %>%
  group_by(country) %>%
  summarise(final_total_deaths = max(total_deaths, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(final_total_deaths)) %>%
  slice(1:10)

# 2.Create the ranked bar chart
plot_v2_top_10_deaths <- top_10_countries %>%
  ggplot(aes(x = reorder(country, final_total_deaths), y = final_total_deaths)) +
  geom_col(fill = "orange") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Top 10 Countries by Total Cumulative COVID-19 Deaths",
    x = "Country",
    y = "Total Deaths (Cumulative)"
  ) +
  theme_minimal()

plot_v2_top_10_deaths


#Visualization 3: Application of the country comparison function (functional programming)
# Split dataset by year
analysis_long_rates_yearly <- split(analysis_long_rates, analysis_long_rates$year)

# Define reusable plotting function
create_country_comparison_year <- function(data, metric_type_input, year_input) {
  data %>%
    filter(country %in% c("United States", "China", "Sweden", "Russia")) %>%
    filter(metric_type == metric_type_input) %>%
    mutate(date_plot = lubridate::make_date(year, month, 1)) %>%
    ggplot(aes(x = date_plot, y = metric_value, group = country, color = country)) +
    geom_line(linewidth = 0.9) +
    scale_y_log10() +
    labs(
      title = glue("Monthly {metric_type_input} in {year_input}"),
      subtitle = "Selected countries with contrasting governance and policy systems",
      x = "Date",
      y = glue("Monthly Average Value ({metric_type_input})"),
      color = "Country"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      legend.position = "bottom"
    )
}

# Select metrics to plot
metrics_to_plot_list <- unique(analysis_long_rates$metric_type)
metrics_to_plot_list <- metrics_to_plot_list[grepl("avg_", metrics_to_plot_list)]

# Apply function for all years
plots_yearly <- purrr::map2(
  analysis_long_rates_yearly,
  names(analysis_long_rates_yearly),
  ~ purrr::map(metrics_to_plot_list, function(metric) {
    create_country_comparison_year(.x, metric, .y)
  })
)

# Display 2020 and 2021 panels as examples
plot_v3_year2020 <- gridExtra::grid.arrange(grobs = plots_yearly[[1]], ncol = 2)
plot_v3_year2021 <- gridExtra::grid.arrange(grobs = plots_yearly[[2]], ncol = 2)

plot_v3_year2020
plot_v3_year2021

