# ============================================================
# World Air Quality Shiny App
# Pages:
# 1. Data Cleaning Summary
# 2. Pollutant Correlation Heatmap
# 3. PM2.5 Country Ranking
# 4. Country PM2.5 Profile
# 5. Global PM2.5 Map
# ============================================================

library(shiny)
library(tidyverse)
library(plotly)
library(leaflet)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(htmlwidgets)
library(shinyjs)
library(markdown)

# =========================
# 1. Data path
# =========================
data_path <- if (file.exists("data/world_air_quality.csv")) {
  "data/world_air_quality.csv"
} else if (file.exists("world_air_quality.csv")) {
  "world_air_quality.csv"
} else {
  "../data/world_air_quality.csv"
}

# =========================
# 2. Read data
# =========================
air_raw <- read_delim(
  data_path,
  delim = ";",
  col_types = cols(.default = col_character()),
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

# =========================
# 3. Data cleaning
# =========================

# The original data `air_raw` is processed and then assigned to the new variable `air_all`.
air_all <- air_raw %>%
  
  # Change the original column names (right side) to more standardized variable names (left side) for easier subsequent calls.
  rename(
    country_code = `Country Code`,
    city = City,
    location = Location,
    coordinates = Coordinates,
    pollutant = Pollutant,
    source_name = `Source Name`,
    unit = Unit,
    value = Value,
    last_updated = `Last Updated`,
    country = `Country Label`
  ) %>%
  
  # [Data Transformation/Preprocessing]
  # Use the mutate function to modify existing columns
  mutate(
    
    # Use str_trim to remove leading and trailing whitespace from a string to prevent matching failures caused by whitespace.
    country = str_trim(country),
    city = str_trim(city),
    location = str_trim(location),
    pollutant = str_trim(pollutant),
    unit = str_trim(unit),
    
    # Use parse_number to convert a character number to a numeric number
    # It automatically ignores non-numeric signs (such as unit or thousands separators) in the number.
    value = parse_number(value)
  ) %>%
  
  # [Data Filtering]
  # Only retain valid observations that meet the following criteria
  filter(
    !is.na(country),# Exclude rows with missing country names (NA)
    !is.na(pollutant),# Exclude rows with missing pollutant types
    !is.na(value),# Exclude rows with missing values
    value >= 0 # Exclude negative values ​​(air quality values ​​are usually not negative)
  )

# =========================
# 4. PM2.5 country cleaning
# =========================

# Create a summary table of PM2.5 statistics for every country
pm25_country_before <- air_all %>%
  filter(pollutant == "PM2.5") %>% # Focus only on PM2.5 data
  group_by(country) %>% # Group data by country name
  summarise(
    n_obs_pm25 = n(), # Count the number of observations per country
    mean_pm25 = mean(value, na.rm = TRUE),# Calculate the average PM2.5 value
    min_pm25 = min(value, na.rm = TRUE),# Find the minimum value
    max_pm25 = max(value, na.rm = TRUE),# Find the maximum value
    .groups = "drop"# Ungroup the data after summarizing
  )

# Identify countries that should be removed based on specific data quality rules
removed_countries <- pm25_country_before %>%
  mutate(
    reason = case_when(
      
      # Rule 1: Flag countries where ALL readings are exactly 0 (likely sensor/data error)
      min_pm25 == 0 & max_pm25 == 0 ~ "All PM2.5 values are 0",
      # Rule 2: Flag countries with fewer than 3 readings (too small for meaningful analysis)
      n_obs_pm25 < 3 ~ "Less than 3 PM2.5 observations",
      # Otherwise, keep the country (assign NA to reason)
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(reason))# Keep only the countries that failed the quality checks

# Create the final 'clean' master dataset by removing the flagged countries
air_all_clean <- air_all %>%
  filter(!country %in% removed_countries$country)# Exclude countries found in the 'removed' list

# Create a specific clean subset containing ONLY PM2.5 data for visualization
pm25_clean <- air_all_clean %>%
  filter(pollutant == "PM2.5")

# =========================
# 5. Static Page 1 cleaning summary data
# =========================

# Create a summary table to track data volume at each cleaning stage
cleaning_count_summary <- tibble(
  # Define the cleaning stages as a factor with a specific order for plotting
  stage = factor(
    c("Raw observations", "After basic cleaning", "After country-level cleaning"),
    levels = c("Raw observations", "After basic cleaning", "After country-level cleaning")
  ),
  # Calculate row counts for each stage
  count = c(
    nrow(air_raw),# Initial row count
    nrow(air_all),# Count after renaming and NA removal
    nrow(air_all_clean)# Count after country-level QC
  )
)

# Prepare a detailed table of removed countries to be displayed in the UI
removed_countries_table <- removed_countries %>%
  # Sort by reason and then country name for better organization
  arrange(desc(reason), country) %>%
  # Select and format columns for the end-user display
  transmute(
    Country = country,
    `PM2.5 observations` = n_obs_pm25,
    # Round statistical values to 3 decimal places for readability
    `Mean PM2.5` = round(mean_pm25, 3),
    `Min PM2.5` = round(min_pm25, 3),
    `Max PM2.5` = round(max_pm25, 3),
    Reason = reason
  )

# =========================
# 6. Top 3 and bottom 3 PM2.5 countries
# =========================

# Step 1: Calculate the final average PM2.5 for each country using the clean dataset
pm25_country_after <- pm25_clean %>%
  group_by(country) %>%
  summarise(
    n_obs_pm25 = n(), # Total number of clean observations
    mean_pm25 = mean(value, na.rm = TRUE), # Final average concentration
    unit = first(unit), # Capture the unit (e.g., µg/m³)
    .groups = "drop" # Ungroup for subsequent operations
  )

# Step 2: Identify the top 3 most polluted countries (Highest PM2.5)
top3_pm25_after <- pm25_country_after %>% 
  arrange(desc(mean_pm25)) %>% # Sort in descending order (highest first)
  slice_head(n = 3) # Take the first 3 rows

# Step 3: Identify the bottom 3 cleanest countries (Lowest PM2.5)
bottom3_pm25_after <- pm25_country_after %>%
  arrange(mean_pm25) %>% # Sort in ascending order (lowest first)
  slice_head(n = 3) # Take the first 3 rows

# Step 4: Create a combined list of these 6 "extreme" countries for focus pages
selected_countries <- c(
  top3_pm25_after$country,
  bottom3_pm25_after$country
)

# Step 5: Extract the measurement unit for use in plot labels/titles
pm25_unit <- pm25_country_after %>%
  filter(!is.na(unit), unit != "") %>% # Ignore empty or NA unit strings
  pull(unit) %>% # Extract column as a vector
  first() # Take the first available valid unit

# Step 6: Safety Check - Fallback to default unit if none was found in the data
if (is.na(pm25_unit) || length(pm25_unit) == 0) {
  pm25_unit <- "µg/m³" # Set default to micrograms per cubic meter
}

# ============================================================
# 6.5 GDP + Geography Radar Data (FIXED JOIN + FIXED SCALING)
# ============================================================
# This section creates the standalone radar dataset for Page 6.
# It combines:
# 1. Average PM2.5
# 2. GDP in 2024
# 3. Average latitude
# 4. Average longitude
# 5. PM2.5 record count
# 6. Pollutant type count
#
# Main fixes:
# - GDP is matched by country name and country code.
# - Country code mismatch is handled manually for selected countries.
# - Missing GDP is no longer blindly turned into 0 before matching.
# - Radar values are scaled across the selected six countries.
# ============================================================


# -------------------------
# Step 1: Locate GDP file
# -------------------------
gdp_path <- if (file.exists("data/GDP.csv")) {
  "data/GDP.csv"
} else if (file.exists("GDP.csv")) {
  "GDP.csv"
} else {
  "../data/GDP.csv"
}


# -------------------------
# Step 2: Helper function for country-name matching
# -------------------------
# This standardizes country names so that GDP country names and air-quality
# country names are easier to match.

clean_country_name <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("&", "and") %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_squish()
}


# -------------------------
# Step 3: Read and clean GDP data
# -------------------------
# The GDP file has several description rows at the top, so skip = 4 is used.
# X1 = country/economy code
# X4 = country/economy name
# X5 = GDP 2024 value

gdp_raw <- readr::read_csv(
  gdp_path,
  skip = 4,
  col_names = FALSE,
  show_col_types = FALSE
)

gdp_clean <- gdp_raw %>%
  transmute(
    gdp_code = str_trim(toupper(as.character(X1))),
    gdp_country = str_trim(as.character(X4)),
    gdp_country_key = clean_country_name(gdp_country),
    gdp_2024_million_usd = readr::parse_number(as.character(X5))
  ) %>%
  filter(
    !is.na(gdp_country),
    !is.na(gdp_2024_million_usd)
  )


# -------------------------
# Step 4: Prepare air-quality data with coordinates
# -------------------------
# Coordinates are separated into latitude and longitude.
# Missing coordinates are not removed from the main dataset.
# They are only ignored when calculating average latitude/longitude.

air_for_geo <- air_all_clean %>%
  separate(
    coordinates,
    into = c("lat", "lon"),
    sep = ",",
    convert = TRUE,
    fill = "right",
    remove = FALSE
  ) %>%
  mutate(
    country_code = str_trim(toupper(country_code)),
    country_key = clean_country_name(country),
    lat = as.numeric(lat),
    lon = as.numeric(lon)
  )


# -------------------------
# Step 5: Create manual ISO3 code backup
# -------------------------
# Some GDP files use 3-letter country codes, while the air-quality file may use
# 2-letter country codes. This backup helps selected countries match GDP.

country_code_backup <- tibble(
  country = selected_countries
) %>%
  mutate(
    iso3_backup = case_when(
      country == "Ghana" ~ "GHA",
      country == "Mongolia" ~ "MNG",
      country == "India" ~ "IND",
      country == "Finland" ~ "FIN",
      country == "Switzerland" ~ "CHE",
      country == "Iceland" ~ "ISL",
      country == "Singapore" ~ "SGP",
      country == "Afghanistan" ~ "AFG",
      country == "Ecuador" ~ "ECU",
      country == "Denmark" ~ "DNK",
      country == "Saudi Arabia" ~ "SAU",
      TRUE ~ NA_character_
    )
  )


# -------------------------
# Step 6: Create country-level profile
# -------------------------
# This creates one row per selected country.

geo_gdp_profile_base <- air_for_geo %>%
  filter(country %in% selected_countries) %>%
  group_by(country) %>%
  summarise(
    country_code = first(country_code[!is.na(country_code) & country_code != ""]),
    country_key = first(country_key),
    avg_pm25 = mean(value[pollutant == "PM2.5"], na.rm = TRUE),
    avg_latitude = mean(lat, na.rm = TRUE),
    avg_longitude = mean(lon, na.rm = TRUE),
    pm25_records = sum(pollutant == "PM2.5", na.rm = TRUE),
    pollutant_types = n_distinct(pollutant),
    .groups = "drop"
  ) %>%
  left_join(country_code_backup, by = "country")


# -------------------------
# Step 7: Join GDP by country name and country code
# -------------------------
# GDP is joined in three ways:
# 1. country name
# 2. original country code
# 3. manual ISO3 backup code

geo_gdp_profile <- geo_gdp_profile_base %>%
  left_join(
    gdp_clean %>%
      select(gdp_country_key, gdp_by_name = gdp_2024_million_usd),
    by = c("country_key" = "gdp_country_key")
  ) %>%
  left_join(
    gdp_clean %>%
      select(gdp_code, gdp_by_code = gdp_2024_million_usd),
    by = c("country_code" = "gdp_code")
  ) %>%
  left_join(
    gdp_clean %>%
      select(gdp_code, gdp_by_iso3 = gdp_2024_million_usd),
    by = c("iso3_backup" = "gdp_code")
  ) %>%
  mutate(
    gdp_2024_million_usd = coalesce(gdp_by_name, gdp_by_code, gdp_by_iso3),
    
    # Replace remaining missing values only after all matching attempts
    avg_pm25 = replace_na(avg_pm25, 0),
    avg_latitude = replace_na(avg_latitude, 0),
    avg_longitude = replace_na(avg_longitude, 0),
    pm25_records = replace_na(pm25_records, 0),
    pollutant_types = replace_na(pollutant_types, 0),
    gdp_2024_million_usd = replace_na(gdp_2024_million_usd, 0)
  ) %>%
  select(
    country,
    country_code,
    iso3_backup,
    avg_pm25,
    gdp_2024_million_usd,
    avg_latitude,
    avg_longitude,
    pm25_records,
    pollutant_types
  )


# -------------------------
# Step 8: Convert country profile into radar format
# -------------------------
# This version fixes the scaling problem.
# Each indicator is normalized across the selected six countries,
# so the radar shapes can show real differences between countries.

geo_gdp_radar_data <- geo_gdp_profile %>%
  select(
    country,
    avg_pm25,
    gdp_2024_million_usd,
    avg_latitude,
    avg_longitude,
    pm25_records,
    pollutant_types
  ) %>%
  pivot_longer(
    cols = c(
      avg_pm25,
      gdp_2024_million_usd,
      avg_latitude,
      avg_longitude,
      pm25_records,
      pollutant_types
    ),
    names_to = "indicator",
    values_to = "raw_value"
  ) %>%
  group_by(indicator) %>%
  mutate(
    min_value = min(raw_value, na.rm = TRUE),
    max_value = max(raw_value, na.rm = TRUE),
    
    # Normalize each indicator across the six selected countries.
    # If all countries have the same value for one indicator,
    # assign 0.5 to avoid division by zero.
    radar_value = ifelse(
      max_value == min_value,
      0.5,
      (raw_value - min_value) / (max_value - min_value)
    ),
    
    # Avoid the polygon collapsing fully into the center.
    radar_value = scales::rescale(radar_value, to = c(0.12, 1))
  ) %>%
  ungroup() %>%
  mutate(
    indicator = recode(
      indicator,
      avg_pm25 = "PM2.5",
      gdp_2024_million_usd = "GDP 2024",
      avg_latitude = "Latitude",
      avg_longitude = "Longitude",
      pm25_records = "PM2.5 Records",
      pollutant_types = "Pollutant Types"
    ),
    indicator = factor(
      indicator,
      levels = c(
        "PM2.5",
        "GDP 2024",
        "Latitude",
        "Longitude",
        "PM2.5 Records",
        "Pollutant Types"
      )
    )
  )

# -------------------------
# Step 9: Debug output
# -------------------------
# Check this printed table in the Console.
# GDP, PM2.5, latitude, and longitude should NOT all be zero.

print(geo_gdp_profile)
print(geo_gdp_radar_data)

# =========================
# 7. Static Page 3 ranking data
# =========================

# Create a new dataset specifically for the ranking plot
pm25_ranking_data <- pm25_country_after %>%
  
  # 1. SORTING: Arrange countries by their mean PM2.5 value in descending order 
  # (Highest pollution levels will be at the top)
  arrange(desc(mean_pm25)) %>%
  
  # 2. DATA ENRICHMENT: Add rank numbers and text labels
  mutate(
    
    # Assign a numerical rank (1, 2, 3...) based on the sorted order
    rank = row_number(),
    
    # Create selective labels to prevent the chart from becoming cluttered
    label = case_when(
      
      # If the country is one of the top 3 most polluted, show its name
      country %in% top3_pm25_after$country ~ country,
      
      # If the country is one of the bottom 3 cleanest, show its name
      country %in% bottom3_pm25_after$country ~ country,
      
      # For all other countries in the middle, leave the label blank ("")
      TRUE ~ ""
    )
  )

# =========================
# 8. Country-level average table
# =========================

# Step 1: Calculate the average value for each country and each pollutant (long table format)
country_avg_long <- air_all_clean %>%
  group_by(country, pollutant) %>% # Group the data by country and pollutant type
  summarise(
    avg_value = mean(value, na.rm = TRUE), # Calculate the mean value for each group
    unit = first(na.omit(unit)), # Capture the first non-missing unit label
    .groups = "drop" # Remove grouping structure after calculation
  )

# Step 2: Transform the data from "Long" to "Wide" format for matrix operations
country_avg <- country_avg_long %>%
  select(country, pollutant, avg_value) %>% # Retain only the columns needed for pivoting
  pivot_wider(
    names_from = pollutant, # Turn pollutant names into column headers
    values_from = avg_value # Fill the cells with the calculated average values
  ) %>%
  
  # Data Thresholding: Only keep pollutant columns that have valid data 
  # for at least 5 different countries. This ensures statistical significance 
  # for the upcoming correlation analysis.
  select(country, where(~ sum(!is.na(.)) >= 5))

# Step 3: Prepare coordinates and axis labels for the Heatmap
pollutants <- colnames(country_avg)[-1] # Get all pollutant names (excluding the 'country' column)
pollutant_levels <- pollutants # Standard ordering for the X-axis
y_pollutant_levels <- rev(pollutant_levels) # Reversed order for the Y-axis to match visual standards

# =========================
# 9. Pollutant correlation heatmap data
# =========================

# Step 1: Calculate the Correlation Matrix
cor_mat <- cor(
  country_avg[, pollutants], # Select only the pollutant columns for comparison
  use = "pairwise.complete.obs", # Handle missing data: only use pairs where both values exist
  method = "pearson" # Use Pearson's method to find linear correlation (-1 to 1)
)

# Step 2: Reshape the matrix into a "Long" data frame for plotting (ggplot2/Plotly)
cor_long <- as.data.frame(as.table(cor_mat)) %>%
  rename(
    x = Var1, # Rename Var1 (first pollutant) to x
    y = Var2, # Rename Var2 (second pollutant) to y
    cor = Freq # Rename Freq (the correlation coefficient) to cor
  ) %>%
  
  # Step 3: Enrich the data with metadata for interactivity
  mutate(
    # Apply factor levels to ensure pollutants stay in the correct order on the axes
    x_name = as.character(x),
    y_name = as.character(y),
    x = factor(x_name, levels = pollutant_levels),
    y = factor(y_name, levels = y_pollutant_levels),
    
    # Identify diagonal cells (where the X pollutant is the same as the Y pollutant)
    diagonal = x_name == y_name,
    
    # Create the text label shown on the tile (leave blank for diagonal cells)
    label = ifelse(diagonal, "", sprintf("%.2f", cor)),
    
    # Unique key used to identify which cell is clicked in the Shiny app
    click_key = paste(x_name, y_name, sep = "::"),
    
    # Build the HTML-formatted hover text (tooltip)
    hover_text = paste0(
      "X pollutant: ", x_name,
      "<br>Y pollutant: ", y_name,
      "<br>Correlation: ", round(cor, 2),
      ifelse(
        diagonal,
        "<br><b>Self-correlation: disabled</b>", # Don't allow clicking the diagonal
        "<br>Click to view regression plot" # Instruction for interactive use
      )
    )
  )

# =========================
# 10. Helper function
# =========================

# This function extracts paired observation data for PM2.5 and a target pollutant 
# within a specific country to allow for localized correlation analysis.
make_country_pair_data <- function(target_country, target_pollutant) {
  
  # 1. Data Filtering and Reshaping
  internal_wide <- air_all_clean %>%
    filter(country == target_country) %>% # Filter for the selected country
    group_by(country, city, location, pollutant) %>% # Group by specific monitoring locations
    summarise(
      mean_value = mean(value, na.rm = TRUE), # Calculate average concentration at each site
      unit = first(na.omit(unit)), # Keep the measurement unit
      .groups = "drop" # Remove grouping structure after calculation
    ) %>%
    select(country, city, location, pollutant, mean_value) %>%
    
    # Pivot from "Long" to "Wide" format so different pollutants become columns
    pivot_wider(
      names_from = pollutant,
      values_from = mean_value
    )
  # 2. Safety Check: Ensure PM2.5 column exists in the results
  if (!("PM2.5" %in% colnames(internal_wide))) {
    return(tibble()) # Return an empty table if PM2.5 data is missing
  }
  
  # 3. Safety Check: Ensure the target pollutant column exists
  if (!(target_pollutant %in% colnames(internal_wide))) {
    return(tibble()) # Return an empty table if the selected pollutant is missing
  }
  
  # 4. Extract and Clean Final Paired Data
  internal_wide %>%
    
    # Select only the relevant location info and the two pollutants for comparison
    select(country, city, location, `PM2.5`, all_of(target_pollutant)) %>%
    
    # Rename columns for standardized use in plotting functions
    rename(
      pm25 = `PM2.5`,
      selected_value = all_of(target_pollutant)
    ) %>%
    
    # CRITICAL STEP: Filter out rows where either value is missing
    # This ensures we only compare "paired" observations from the same site.
    filter(complete.cases(pm25, selected_value))
}

# ============================================================
# Pollutant Unit Lookup
# ============================================================

# Create a reference table to quickly find the measurement unit for each pollutant.
pollutant_unit_lookup <- air_all_clean %>%
  select(pollutant, unit) %>%
  filter(!is.na(unit), unit != "") %>% # Remove missing or empty unit labels
  group_by(pollutant) %>%
  summarise(unit = first(unit), .groups = "drop") # Take the first valid unit found for each pollutant

# =========================
# 11. Country profile heatmap data
# =========================

# Generate all combinations of countries and pollutants to calculate localized correlations
country_pollutant_heatmap <- expand_grid(
  country = selected_countries, # Use the top 3 and bottom 3 countries identified earlier
  pollutant = pollutant_levels # List of all available pollutant types
) %>%
  
  # Exclude PM2.5 because we are correlating other pollutants AGAINST PM2.5
  filter(pollutant != "PM2.5") %>%
  mutate(
    
    # 1. DATA RETRIEVAL: Call the helper function to get paired site data for each combination
    pair_data = map2(country, pollutant, make_country_pair_data),
    
    # 2. COUNTING: Determine how many valid paired observations exist for each pair
    n_pairs = map_int(pair_data, nrow),
    
    # 3. STATISTICAL CALCULATION: Compute localized Pearson correlation
    correlation = map2_dbl(pair_data, n_pairs, function(df, n) {
      # Only calculate if there are at least 3 pairs and both variables have variation
      if (n >= 3 && sd(df$pm25, na.rm = TRUE) > 0 && sd(df$selected_value, na.rm = TRUE) > 0) {
        cor(df$pm25, df$selected_value, use = "complete.obs")
      } else {
        NA_real_# Return NA if data is insufficient or variation is zero
      }
    })
  ) %>%
  
  # Filter out results that couldn't be calculated or have too few observations
  filter(!is.na(correlation), n_pairs >= 3) %>%
  
  # Add the measurement unit (e.g., µg/m³) for each pollutant for display purposes
  left_join(
    pollutant_unit_lookup,
    by = "pollutant"
  ) %>%
  
  # Formatting for the Plotly Heatmap
  mutate(
    country_name = as.character(country),
    pollutant_name = as.character(pollutant),
    
    # Set factor levels to control the order of countries and pollutants on chart axes
    country = factor(country_name, levels = selected_countries),
    pollutant = factor(pollutant_name, levels = y_pollutant_levels),
    
    # Generate a unique key for the click event to identify which tile is selected
    click_key = paste(country_name, pollutant_name, sep = "::"),
    
    # Construct HTML-formatted hover labels (tooltips) for the interactive chart
    hover_text = paste0(
      "Country: ", country_name,
      "<br>Pollutant: ", pollutant_name,
      "<br>Correlation with PM2.5: ", round(correlation, 2),
      "<br>Paired observations: ", n_pairs,
      "<br>Unit: ", unit,
      "<br>Click to view within-country regression"
    )
  )

# =========================
# 12. World map data
# =========================

# Step 1: Load and standardize geographic map boundaries
world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(
    
    # DATA ALIGNMENT: Standardize country names to match the CSV data labels.
    # This ensures that air quality data for "United States" correctly attaches 
    # to the polygon named "United States of America" in the spatial dataset.
    name = case_when(
      name == "United States of America" ~ "United States",
      name == "Viet Nam" ~ "Vietnam",
      name == "Czechia" ~ "Czech Republic",
      name == "Taiwan" ~ "Taiwan, China",
      name == "Republic of Korea" ~ "South Korea",
      name == "The Bahamas" ~ "Bahamas",
      name == "Hong Kong" ~ "Hong Kong, China",
      name == "Lao PDR" ~ "Lao People's Dem. Rep.",
      name == "North Macedonia" ~ "Macedonia, The former Yugoslav Rep. of",
      name == "Russia" ~ "Russian Federation",
      TRUE ~ name # Keep original name if not listed above
    )
  )

# Step 2: Define a dynamic color palette for the map
# This function creates a mapping between PM2.5 values and a specific color scale.
# HEX CODE PALETTE: 
# #2E7D32 (Green - Good)
# #F9E076 (Yellow - Moderate)
# #D84315 (Orange - Unhealthy)
# #B71C1C (Red - Very Unhealthy)
# #3E2723 (Deep Brown - Hazardous)
create_pal_function <- function(data_range) {
  colorNumeric(
    palette = c("#2E7D32", "#F9E076", "#D84315", "#B71C1C", "#3E2723"),
    domain = data_range, # Scales the colors based on the min/max values in the data
    na.color = "transparent" # Hide countries with no data
  )
}

# =========================
# 13. Decode click positions
# =========================

# Function to translate a click value into the corresponding X-axis pollutant name
decode_x_pollutant <- function(v) {
  # Ensure the input value is treated as a character string
  v <- as.character(v)
  
  # Check if the click value 'v' is a numeric index (contains only digits)
  if (grepl("^[0-9]+$", v)) {
    idx <- as.integer(v) # Convert the string to an integer
    
    # If the index is valid and within the range of our pollutant list,
    # return the specific pollutant name at that position
    if (!is.na(idx) && idx >= 1 && idx <= length(pollutant_levels)) {
      return(pollutant_levels[idx])
    }
  }
  
  # If it's already a name or the index is invalid, return 'v' as-is
  return(v)
}

# Function to translate a click value into the corresponding Y-axis pollutant name
decode_y_pollutant <- function(v) {
  v <- as.character(v)
  
  # Check if the click value 'v' is a numeric index
  if (grepl("^[0-9]+$", v)) {
    idx <- as.integer(v)
    
    # Check index validity against the reversed Y-axis pollutant levels
    if (!is.na(idx) && idx >= 1 && idx <= length(y_pollutant_levels)) {
      return(y_pollutant_levels[idx])
    }
  }
  
  # Return original value if no numeric decoding is needed
  return(v)
}

# =========================
# 14. UI
# The main UI container. fluidPage creates a layout that fills the 
# browser window width.
# =========================

ui <- fluidPage(  # Create the full Shiny user interface page
  
  useShinyjs(),  # Enable shinyjs so the app can control UI behavior dynamically if needed
  
  # =========================
  # Opening / Welcome Page
  # =========================
  # This overlay appears when the app first loads.
  # It introduces PM2.5 with a typewriter animation.
  # After the typing animation, it shows the Markdown welcome text and Start button.
  
  div(
    id = "opening_overlay",
    class = "opening-overlay",
    
    div(
      class = "opening-card",
      
      # Opening language buttons
      div(
        class = "opening-lang-row",
        
        actionButton(
          inputId = "open_lang_en",
          label = "English",
          class = "opening-lang-btn opening-lang-active"
        ),
        
        actionButton(
          inputId = "open_lang_zh",
          label = "中文",
          class = "opening-lang-btn"
        ),
        
        actionButton(
          inputId = "open_lang_ja",
          label = "日本語",
          class = "opening-lang-btn"
        )
      ),
      
      # -------------------------
      # Typewriter text area
      # -------------------------
      div(
        id = "opening_typewriter",
        class = "opening-typewriter",
        
        span(id = "typewriter_text"),
        # JavaScript will type PM2.5 introduction here.
        
        span(id = "typewriter_cursor", class = "opening-cursor", "|")
        # Blinking cursor.
      ),
      
      # -------------------------
      # Final welcome message
      # -------------------------
      div(
        id = "opening_final",
        class = "opening-final",
        
        uiOutput("opening_markdown"),
        # Load opening_en.md / opening_zh.md / opening_ja.md.
        
        actionButton(
          inputId = "start_app",
          label = "Start",
          class = "start-btn",
          onclick = "
          document.getElementById('opening_overlay').classList.add('hide-opening');
        "
        )
        # Clicking Start hides the opening overlay.
      )
    ),
    
    
    
    # -------------------------
    # Opening animation script
    # -------------------------
    tags$script(HTML("
  window.openingTexts = {
    en: 'Have you ever heard of PM2.5? PM2.5 refers to tiny airborne particles that can enter the human body and harm our health.',
    zh: '你是否听说过 PM2.5？PM2.5 是一种非常微小的空气颗粒物，它可以进入人体，并对健康造成危害。',
    ja: 'PM2.5 を聞いたことがありますか？PM2.5 は非常に小さな大気中の粒子で、人体に入り込み、健康に悪影響を与える可能性があります。'
  };
// Store the current typing timer.
// This allows us to stop the old typewriter animation when the user switches language.
  window.openingTypingTimer = null;
// Main function for switching the opening-page language.
// lang can be en,zh, or ja.
  function setOpeningLang(lang) {

    // Send selected language to Shiny server
    // This tells the Shiny server which language the user selected.
  // The server can then update the Markdown content and Start button label.
    if (window.Shiny) {
      Shiny.setInputValue('opening_lang_selected', lang, {priority: 'event'});
    }

    // Button active style
    document.querySelectorAll('.opening-lang-btn').forEach(function(btn) {
      btn.classList.remove('opening-lang-active');
    });

// Add the active style only to the selected language button.
  // Example: if lang =  zh, the selected button id is open_lang_zh.
  
    const activeBtn = document.getElementById('open_lang_' + lang);
    if (activeBtn) {
      activeBtn.classList.add('opening-lang-active');
    }

  // =========================
  // 3. Get opening-page HTML elements
  // =========================
  // typewriter_text: where the animated text is typed.
  // typewriter_cursor: the blinking cursor after the text.
  // opening_typewriter: the container for the typewriter animation.
  // opening_final: the final Markdown welcome message and Start button.

    const target = document.getElementById('typewriter_text');
    const cursor = document.getElementById('typewriter_cursor');
    const typewriterBox = document.getElementById('opening_typewriter');
    const finalBox = document.getElementById('opening_final');

    if (!target || !typewriterBox || !finalBox) return;

    // Clear old timer
    if (window.openingTypingTimer) {
      clearTimeout(window.openingTypingTimer);
    }

    // Reset display
    target.textContent = '';
    typewriterBox.style.display = 'block';
    typewriterBox.style.opacity = '1';
    typewriterBox.style.transform = 'translateY(0)';
    finalBox.classList.remove('show-final');

// Show the blinking cursor again.
    if (cursor) {
      cursor.style.display = 'inline-block';
    }

  // =========================
  // 6. Select the correct opening text
   // =========================
  // window.openingTexts should be defined earlier.
  
    let text = window.openingTexts[lang];
    let index = 0;



  // =========================
  // 7. Typewriter animation function
  // =========================

    function typeText() {
      if (index < text.length) {
        target.textContent += text.charAt(index);
        index++;
        window.openingTypingTimer = setTimeout(typeText, 45);
      } else {
        window.openingTypingTimer = setTimeout(function() {

          if (cursor) {
            cursor.style.display = 'none';
          }

          typewriterBox.style.transition = 'opacity 0.7s ease, transform 0.7s ease';
          typewriterBox.style.opacity = '0';
          typewriterBox.style.transform = 'translateY(-12px)';

          window.openingTypingTimer = setTimeout(function() {
            typewriterBox.style.display = 'none';
            finalBox.classList.add('show-final');
          }, 750);

        }, 900);
      }
    }

    typeText();
  }

  document.addEventListener('DOMContentLoaded', function() {

    const enBtn = document.getElementById('open_lang_en');
    const zhBtn = document.getElementById('open_lang_zh');
    const jaBtn = document.getElementById('open_lang_ja');

    if (enBtn) {
      enBtn.addEventListener('click', function() {
        setOpeningLang('en');
      });
    }

    if (zhBtn) {
      zhBtn.addEventListener('click', function() {
        setOpeningLang('zh');
      });
    }

    if (jaBtn) {
      jaBtn.addEventListener('click', function() {
        setOpeningLang('ja');
      });
    }

    setOpeningLang('en');
  });
"))
    
    
    
  ),
  
  tags$head(  # Put global CSS settings into the HTML <head> section
    
    tags$style(  # Define custom CSS styles for the whole app
      
      HTML("  /* Start writing raw CSS code inside an HTML string */

      /* =========================
         Modal Layer Fix
         ========================= */

      .modal {  /* Select the white popup modal box */
        z-index: 1000001 !important;  /* Keep the modal above the sticky Dynamic Island header */
      }

      .modal-backdrop {  /* Select the dark overlay behind the popup modal */
        z-index: 1000000 !important;  /* Keep the overlay above the page but below the modal box */
      }

      .modal-dialog {  /* Select the popup dialog container */
        margin-top: 130px;  /* Push the popup slightly downward so it does not overlap the header */
      }

      /* =========================
         Global Page Styling
         ========================= */

      body {  /* Style the whole app background and default text */
        background-color: #f8f9ff;  /* Set a very light blue-gray background color */
        background-image:  /* Add two dotted radial-gradient background layers */
          radial-gradient(circle, #e0e8ff 7%, transparent 8%),  /* Add soft blue dots */
          radial-gradient(circle, #ffe8f0 7%, transparent 8%);  /* Add soft pink dots */
        background-size: 42px 42px;  /* Control the repeating dot spacing */
        background-position: 0 0, 21px 21px;  /* Offset the second dot layer for a balanced pattern */
        color: #2d3748;  /* Set the default text color */
        font-family: 'Segoe UI', Tahoma, sans-serif;  /* Use a clean modern font stack */
        margin: 0;  /* Remove default browser margin */
        padding: 0;  /* Remove default browser padding */
      }

      .container-fluid {  /* Style Shiny's main page container */
        max-width: 1900px;  /* Prevent the page from becoming too stretched on wide screens */
      }

      /* =========================
         Dynamic Island Header
         ========================= */

      .dynamic-island {  /* Style the custom floating navigation header */
        background: rgba(255,255,255,0.92);  /* Use a semi-transparent white background */
        backdrop-filter: blur(16px);  /* Add glass blur effect for modern UI */
        -webkit-backdrop-filter: blur(16px);  /* Add Safari-compatible blur effect */
        border-radius: 34px;  /* Make the header corners rounded like a Dynamic Island */
        padding: 18px 28px;  /* Add inner spacing around title and buttons */
        margin: 14px auto 22px auto;  /* Center the header and add vertical spacing */
        box-shadow: 0 10px 30px rgba(0,0,0,0.08);  /* Add soft floating shadow */
        text-align: center;  /* Center-align title, subtitle, and buttons */
        width: 86%;  /* Let the header occupy most of the page width */
        max-width: 1180px;  /* Prevent the header from becoming too wide */
        position: sticky;  /* Keep the header visible while scrolling */
        top: 12px;  /* Keep a small gap from the top of the browser */
        z-index: 99999;  /* Place the header above normal page content */
        border: 1px solid rgba(226,232,240,0.9);  /* Add a subtle border */
        transition: all 0.25s ease;  /* Smooth visual changes */
      }

      .dynamic-island h1 {  /* Style the main platform title inside the header */
        font-size: 30px;  /* Set title size */
        font-weight: 700;  /* Make the title bold */
        margin: 0;  /* Remove default heading margin */
        color: #334155;  /* Use a dark blue-gray title color */
        letter-spacing: 0.2px;  /* Slightly improve title spacing */
      }

      .dynamic-island .sub-title {  /* Style the subtitle inside the header */
        font-size: 14px;  /* Set subtitle size */
        color: #718096;  /* Use a muted gray-blue subtitle color */
        margin-top: 6px;  /* Add small spacing above subtitle */
        margin-bottom: 12px;  /* Add spacing between subtitle and buttons */
      }

/* Opening page language button row */
.opening-lang-row {
  display: flex;
  justify-content: center;
  gap: 10px;
  margin-bottom: 28px;
}

/* Opening page language buttons */
.opening-lang-btn {
  background: rgba(255,255,255,0.96) !important;
  color: #2d3748 !important;
  border: 1px solid #d8dee9 !important;
  border-radius: 14px !important;
  padding: 8px 16px !important;
  font-weight: 700 !important;
  font-size: 14px !important;
  box-shadow: 0 5px 14px rgba(0,0,0,0.07) !important;
  transition: all 0.18s ease-in-out !important;
}

/* Active opening language button */
.opening-lang-active {
  background: linear-gradient(135deg, #3b82f6, #1d4ed8) !important;
  color: white !important;
  border: 1px solid transparent !important;
}

/* Hover effect */
.opening-lang-btn:hover {
  transform: translateY(-2px);
}

      /* =========================
         Navigation Button Layout
         ========================= */

      .island-buttons {  /* Style the container holding all navigation buttons */
        display: flex;  /* Use flexbox to align buttons */
        justify-content: center;  /* Center buttons horizontally */
        align-items: center;  /* Align buttons vertically */
        flex-wrap: wrap;  /* Allow buttons to wrap on smaller screens */
        gap: 8px;  /* Add consistent spacing between buttons */
      }

      /* =========================
         Default navigation button style
         =========================
         All buttons are white by default.
      */
      .island-btn {
        background: rgba(255,255,255,0.96) !important;
        color: #2d3748 !important;
        border: 1px solid #d8dee9 !important;
        border-radius: 999px !important;
        padding: 8px 14px !important;
        font-weight: 500 !important;
        box-shadow: 0 3px 10px rgba(0,0,0,0.04);
        transition: all 0.18s ease-in-out;
      }
      
      /* =========================
         Active navigation button style
         =========================
         The currently selected page button becomes blue.
      */
      .island-btn.active {
        background: linear-gradient(135deg, #3b82f6, #1d4ed8) !important;
        color: white !important;
        border: 1px solid transparent !important;
        box-shadow: 0 5px 16px rgba(37,99,235,0.42);
      }
      
      /* =========================
         Hover effect for inactive buttons only
         =========================
         Important:
         Use .island-btn:not(.active):hover so the active blue button
         will NOT be overwritten by hover styling.
      */
      .island-btn:not(.active):hover {
        background: #edf6ff !important;
        color: #1f5f99 !important;
        transform: translateY(-2px) scale(1.03);
        box-shadow: 0 5px 14px rgba(0,0,0,0.12);
      }
      
      /* =========================
         Hover effect for active button
         =========================
         The active button stays blue even when hovered.
      */
      .island-btn.active:hover {
        background: linear-gradient(135deg, #3b82f6, #1d4ed8) !important;
        color: white !important;
        transform: translateY(-2px) scale(1.03);
        box-shadow: 0 6px 18px rgba(37,99,235,0.50);
      }
      
      /* =========================
         Top Control Bar
         =========================
         This bar contains language buttons and BGM buttons.
         It is aligned to the right side below the Dynamic Island header.
      */
      .top-control-bar {
        width: 86%;
        max-width: 1180px;
        margin: -6px auto 18px auto;
        display: flex;
        justify-content: flex-end;
        align-items: center;
        gap: 12px;
      }
      
      /* =========================
         Language Button Style
         ========================= */
      .lang-btn {
        background: rgba(255,255,255,0.96) !important;
        color: #2d3748 !important;
        border: 1px solid #d8dee9 !important;
        border-radius: 14px !important;
        padding: 10px 18px !important;
        font-weight: 700 !important;
        font-size: 15px !important;
        box-shadow: 0 5px 14px rgba(0,0,0,0.07) !important;
        transition: all 0.18s ease-in-out !important;
      }
      
      /* Active language button */
      .lang-btn-active {
        background: #ffffff !important;
        color: #1f2937 !important;
        border: 2px solid #d8dee9 !important;
      }
      
      /* Hover effect for language buttons */
      .lang-btn:hover {
        transform: translateY(-2px);
        background: #edf6ff !important;
        color: #1f5f99 !important;
      }
      
      /* =========================
         BGM Button Base Style
         ========================= */
      .bgm-btn {
        border: none !important;
        border-radius: 14px !important;
        padding: 10px 18px !important;
        font-weight: 700 !important;
        font-size: 15px !important;
        box-shadow: 0 6px 16px rgba(0,0,0,0.10) !important;
        transition: all 0.18s ease-in-out !important;
      }
      
      /* Play button color */
      .play-bgm-btn {
        background: #72c8bd !important;
        color: white !important;
      }
      
      /* Pause button color */
      .pause-bgm-btn {
        background: #f4b28f !important;
        color: white !important;
      }
      
      /* Hover effect for BGM buttons */
      .bgm-btn:hover {
        transform: translateY(-2px);
        opacity: 0.92;
      }

      /* =========================
         Card and Panel Styling
         ========================= */

      .modern-card {  /* Style main content cards */
        background: rgba(255,255,255,0.96);  /* Use near-white card background */
        border-radius: 18px;  /* Round card corners */
        padding: 22px;  /* Add inner spacing */
        box-shadow: 0 8px 24px rgba(0,0,0,0.06);  /* Add soft card shadow */
        border: 1px solid rgba(226,232,240,0.9);  /* Add subtle card border */
        margin-bottom: 24px;  /* Add spacing below each card */
      }

      .well {  /* Style Shiny wellPanel sidebars */
        background-color: rgba(255,255,255,0.95);  /* Use clean white sidebar background */
        border: none;  /* Remove default Bootstrap border */
        border-radius: 16px;  /* Round sidebar corners */
        box-shadow: 0 6px 20px rgba(0,0,0,0.06);  /* Add soft sidebar shadow */
        padding: 20px;  /* Add inner sidebar spacing */
        
        position: fixed;
        /* Fix the info ball/card on the screen, so it stays visible while scrolling */
        
        left: 150px;
        /* Put the info ball near the left side of the Dynamic Island */
        /* Increase this value to move right; decrease it to move left */
        
        top: 118px;
        /* Vertically align the info ball around the middle-left of the Dynamic Island */
        /* Increase this value to move lower; decrease it to move higher */
        
        z-index: 9998;
        /* Keep it above normal content, but below the Dynamic Island header */
              }

      /* =========================
   Interactive Sidebar Explanation Cards
   =========================
   This applies to all wellPanel() explanation cards
   across the six pages:
   Cleaning, Correlation, Ranking, Profile, Map, and Radar.
*/

/* Add animation behavior to every sidebar card */
.well {
  position: relative;                /* Allows the floating label to be positioned inside the card */
  transition: all 0.22s ease-in-out; /* Makes hover animation smooth */
  cursor: pointer;                   /* Shows that the card is interactive */
}

/* Hover effect for all sidebar explanation cards */
.well:hover {
  transform: translateY(-6px) scale(1.015);          /* Move the card upward and slightly enlarge it */
  box-shadow: 0 14px 34px rgba(37, 99, 235, 0.16);   /* Add stronger blue shadow */
  border: 1px solid rgba(59, 130, 246, 0.35);        /* Add subtle blue border */
  background-color: rgba(255, 255, 255, 0.98);       /* Make the card slightly brighter */
}

/* Smooth heading color change */
.well h4 {
  transition: color 0.22s ease-in-out; /* Smoothly changes heading color */
}

/* Headings become blue when the card is hovered */
.well:hover h4 {
  color: #1d4ed8; /* Blue heading color on hover */
}

/* =========================
   Apple-style floating info ball/card
   =========================
   This effect is applied only to panels with class info-float.
   It will NOT affect normal wellPanel controls such as Map dropdowns.
*/

/* Default collapsed state: small info ball */
.info-float {
  position: fixed;
  /* Fix the info ball/card on the screen */

  left: 42px;
  /* Horizontal position of the info ball */

  top: 70px;
  /* Vertical position of the info ball */

  width: 72px;
  /* Collapsed ball width */

  height: 72px;
  /* Collapsed ball height */

  min-height: 72px;
  /* Keep collapsed ball size stable */

  overflow: hidden;
  /* Hide Markdown content when collapsed */

  border-radius: 999px;
  /* Make collapsed state a circle */

  padding: 0 !important;
  /* Remove normal padding when collapsed */

  cursor: pointer;
  /* Show that the ball is interactive */

  transition: all 0.35s ease-in-out;
  /* Smooth expand/collapse animation */

  background: rgba(255, 255, 255, 0.18);
  /* Very transparent glass background */

  backdrop-filter: blur(6px);
  -webkit-backdrop-filter: blur(6px);
  /* Light glass blur */

  border: 1px solid rgba(59, 130, 246, 0.35);
  /* Light blue border */

  box-shadow: 0 10px 28px rgba(37, 99, 235, 0.08);
  /* Soft floating shadow */

  z-index: 1000000;
  /* Keep the floating info card above Dynamic Island but below modal */
}

/* Small info icon inside collapsed ball */
.info-float::before {
  content: 'i';
  /* Information icon text */

  position: absolute;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%);

  width: 38px;
  height: 38px;
  border-radius: 999px;

  background: linear-gradient(135deg, #3b82f6, #1d4ed8);
  color: white;

  font-size: 24px;
  font-weight: 800;
  font-family: Georgia, serif;

  display: flex;
  align-items: center;
  justify-content: center;

  box-shadow: 0 6px 16px rgba(37, 99, 235, 0.35);
  transition: all 0.25s ease-in-out;
}

/* Hide Markdown content in collapsed state */
.info-float > * {
  opacity: 0;
  transform: translateY(8px);
  transition: all 0.25s ease-in-out;
}

/* Expanded transparent floating card */
.info-float:hover {
  width: 430px;
  /* Expanded card width */

  max-height: 68vh;
  /* Limit expanded height */

  height: auto;
  min-height: 240px;

  overflow-y: auto;
  /* Allow long explanation text to scroll inside */

  border-radius: 26px;
  padding: 24px !important;

  background: rgba(255, 255, 255, 0.18);
  /* Very transparent expanded background */

  backdrop-filter: blur(6px);
  -webkit-backdrop-filter: blur(6px);

  border: 1px solid rgba(59, 130, 246, 0.40);
  box-shadow: 0 12px 28px rgba(37, 99, 235, 0.08);

  transform: translateY(-6px);
}

/* Keep the small info icon visible after expansion */
.info-float:hover::before {
  opacity: 1;
  top: 14px;
  left: auto;
  right: 58px;
  transform: scale(0.62);
}

/* Show Markdown content after expansion */
.info-float:hover > * {
  opacity: 1;
  transform: translateY(0);
}

/* Make headings blue when expanded */
.info-float:hover h4 {
  color: #1d4ed8;
}

/* Smooth heading color transition */
.info-float h4 {
  transition: color 0.25s ease-in-out;
}



      .control-group {  /* Style grouped map controls or input sections */
        margin-bottom: 25px;  /* Add spacing below each control group */
      }

/* =========================
   Make Focus on Country dropdown behave like Sort dropdown
   ========================= */

/* Limit dropdown height so it will not cover the Quick Jump section too much */
.selectize-dropdown-content {
  max-height: 170px !important;
  overflow-y: auto !important;
}

/* Keep the dropdown clean and solid */
.selectize-dropdown {
  background: rgba(255, 255, 255, 0.98) !important;
  border: 1px solid rgba(148, 163, 184, 0.45) !important;
  border-radius: 8px !important;
  box-shadow: 0 10px 24px rgba(15, 23, 42, 0.14) !important;
  z-index: 1000002 !important;
}

/* Make each option readable */
.selectize-dropdown .option {
  background: white !important;
  color: #1f2937 !important;
  padding: 7px 12px !important;
  font-size: 14px !important;
}

/* Blue highlight for the active option */
.selectize-dropdown .active {
  background: #3b82f6 !important;
  color: white !important;
}

/* =========================
   Radar page floating card position
   =========================
   Keep the Radar explanation as a floating card,
   but make it narrower so it does not cover the radar plot.
*/

.radar-float {
  left: 26px !important;
  /* Put the Radar info ball close to the left edge */

  top: 250px !important;
  /* Place it below the header and language buttons */

  z-index: 1000000 !important;
  /* Keep it above charts when expanded */
}

.radar-float:hover {
  width: 300px !important;
  /* Make the expanded Radar card narrower */

  max-height: 66vh !important;
  /* Keep the card scrollable if the Markdown is long */

  padding: 18px !important;
  /* Reduce inner spacing so more text fits */

  background: rgba(255, 255, 255, 0.50) !important;
  /* Semi-transparent but readable */

  backdrop-filter: blur(5px) !important;
  -webkit-backdrop-filter: blur(5px) !important;
  /* Light blur, so the chart behind is not heavily blocked */
}
/* =========================
   Radar info icon special position
   =========================
   Move the Radar i icon away from the title.
*/
/* =========================
   Radar info icon special position
   =========================
   Put the Radar i icon slightly outside the card
   so it will not cover the long title.
*/
    .radar-float:hover::before {
      opacity: 1 !important;
    
      top: 12px !important;
      left: auto !important;
      right: -14px !important;
    
      transform: scale(0.62) !important;
    }

}

      .btn-jump {  /* Style quick-jump buttons if used in the map page */
        margin: 4px;  /* Add spacing around small buttons */
        padding: 5px 10px;  /* Add compact inner spacing */
        border-radius: 4px;  /* Slightly round small buttons */
      }

      .view-all-btn {  /* Style wider map reset or batch-view buttons */
        margin-bottom: 15px;  /* Add spacing below the button */
        font-weight: bold;  /* Make important buttons bold */
      }

      hr {  /* Style horizontal separator lines */
        margin-top: 20px;  /* Add spacing above the line */
        margin-bottom: 20px;  /* Add spacing below the line */
      }
      
      /* =========================
         Main content spacing
         =========================
         Add spacing below the sticky header so large visualizations
         such as the map and radar chart are not covered.*/
         
      .tab-content {
        padding-top: 26px;
      }
      
      /* Add a little spacing inside each hidden tab page */
      
      .tab-pane {
        padding-top: 10px;
      }
      
      /* =========================
   Opening / Welcome Page Overlay
   =========================
   This full-screen opening page appears when the app first loads.
   It introduces PM2.5 with a typewriter animation,
   then shows a welcome message loaded from Markdown and a Start button.
*/

.opening-overlay {
  position: fixed;
  /* Fix the opening page on top of the whole browser window */

  top: 0;
  left: 0;
  width: 100vw;
  height: 100vh;
  /* Make the overlay cover the full screen */

  z-index: 2000000;
  /* Keep the opening page above all app elements, including maps and floating cards */

  background-color: #f8f9ff;
  /* Use the same light background as the main app */

  background-image:
    radial-gradient(circle, #dbeafe 7%, transparent 8%),
    radial-gradient(circle, #ffe4ec 7%, transparent 8%);
  /* Add soft blue and pink dotted background */

  background-size: 42px 42px;
  background-position: 0 0, 21px 21px;
  /* Control dot spacing and offset */

  display: flex;
  align-items: center;
  justify-content: center;
  /* Center the opening card */

  transition: opacity 0.7s ease, visibility 0.7s ease;
  /* Smooth fade-out after clicking Start */
}

.opening-overlay.hide-opening {
  opacity: 0;
  visibility: hidden;
  pointer-events: none;
  /* Hide the opening page after Start is clicked */
}

.opening-card {
  width: min(820px, 86vw);
  /* Card width adapts to screen size */

  min-height: 420px;
  /* Keep enough height for the typewriter and welcome text */

  background: rgba(255, 255, 255, 0.78);
  /* Semi-transparent glass card */

  backdrop-filter: blur(18px);
  -webkit-backdrop-filter: blur(18px);
  /* Glass blur effect */

  border: 1px solid rgba(191, 219, 254, 0.95);
  border-radius: 34px;
  /* Rounded card corners */

  box-shadow: 0 24px 70px rgba(37, 99, 235, 0.18);
  /* Soft blue shadow */

  padding: 56px 60px;
  /* Inner spacing */

  text-align: center;
  color: #1f2937;

  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
}

/* Typewriter text area */
.opening-typewriter {
  font-size: 30px;
  font-weight: 700;
  line-height: 1.55;
  color: #1d4ed8;

  min-height: 150px;
  max-width: 720px;
  margin-bottom: 26px;
}

/* Blinking cursor after the typing text */
.opening-cursor {
  display: inline-block;
  margin-left: 4px;
  color: #1d4ed8;
  animation: blinkCursor 0.8s infinite;
}

/* Cursor blinking animation */
@keyframes blinkCursor {
  0%, 45% {
    opacity: 1;
  }

  46%, 100% {
    opacity: 0;
  }
}

/* Final welcome message area */
.opening-final {
  opacity: 0;
  transform: translateY(18px);
  transition: all 0.7s ease;
  /* Hidden first, then fades in after typewriter animation */
}

.opening-final.show-final {
  opacity: 1;
  transform: translateY(0);
  /* Show final welcome content */
}

/* Markdown title style inside the opening page */
.opening-final h2 {
  font-size: 34px;
  font-weight: 800;
  color: #334155;
  margin-bottom: 18px;
}

/* Markdown paragraph style inside the opening page */
.opening-final p {
  font-size: 18px;
  line-height: 1.8;
  color: #475569;
  margin-bottom: 30px;
}

/* Game-style Start button */
.start-btn {
  border: none !important;
  border-radius: 999px !important;
  padding: 14px 42px !important;

  background: linear-gradient(135deg, #3b82f6, #1d4ed8) !important;
  color: white !important;

  font-size: 20px !important;
  font-weight: 800 !important;
  letter-spacing: 0.5px;

  box-shadow: 0 12px 30px rgba(37, 99, 235, 0.38) !important;
  transition: all 0.2s ease-in-out !important;
}

/* Start button hover effect */
.start-btn:hover {
  transform: translateY(-3px) scale(1.04);
  box-shadow: 0 16px 38px rgba(37, 99, 235, 0.48) !important;
}

/* =========================
   Final Page Horizontal Layout
   ========================= */

/* Final page container:
   no large white background, keep dotted background visible. */
.final-page-card {
  min-height: 820px;
  padding: 38px 12px 70px 12px !important;
  background: transparent !important;
  border: none !important;
  box-shadow: none !important;
}

/* Main title */
.final-main-title {
  text-align: center;
  font-size: 42px;
  font-weight: 900;
  color: #1f2937;
  margin-top: 8px;
  margin-bottom: 14px;
  transform: translateX(-160px);
}

/* Subtitle */
.final-subtitle {
  max-width: 1050px;
  margin: 0 auto 70px auto;
  text-align: center;
  font-size: 19px;
  line-height: 1.7;
  color: #475569;
  transform: translateX(-160px);
}

/* Board containing three cards in one horizontal row */
.final-horizontal-board {
  width: 100%;
  max-width: 1320px;
  margin: 0 auto 52px auto;
  transform: translateX(-160px);

  display: flex;
  justify-content: center;
  align-items: flex-start;
  gap: 32px;

  flex-wrap: nowrap;
  background: transparent !important;
}

/* Each horizontal card contains text on top and polaroid below */
.final-horizontal-card {
  width: 31%;
  min-width: 290px;

  display: flex;
  flex-direction: column;
  align-items: center;

  background: transparent !important;
  border: none !important;
  box-shadow: none !important;
}

/* Text block above each image */
.final-horizontal-text {
  width: 100%;
  min-height: 250px;
  text-align: left;
  background: transparent !important;
}

/* Big blue heading */
.final-horizontal-text h3 {
  color: #111111;
  font-weight: 900;
  font-size: 34px;
  line-height: 1.18;
  margin-top: 0;
  margin-bottom: 20px;
}

/* Larger paragraph */
.final-horizontal-text p {
  font-size: 18px;
  line-height: 1.65;
  color: #334155;
  margin-bottom: 16px;
}

/* Source link */
.image-credit {
  display: inline-block;
  font-size: 12px;
  font-style: italic;
  color: #64748b;
  text-decoration: underline;
}

/* Polaroid image frame */
.final-horizontal-polaroid {
  width: 100%;
  background: #ffffff;
  padding: 16px 16px 58px 16px;
  border-radius: 14px;
  box-shadow: 0 24px 50px rgba(15, 23, 42, 0.16);
  border: 1px solid rgba(226, 232, 240, 0.95);
  transition: all 0.25s ease-in-out;
}

/* Image inside polaroid */
.final-horizontal-img {
  width: 100%;
  height: 205px;
  object-fit: cover;
  border-radius: 9px;
  display: block;
}

/* Slight rotations */
.rotate-left {
  transform: rotate(-4deg);
}

.rotate-middle {
  transform: rotate(1deg);
}

.rotate-right {
  transform: rotate(4deg);
}

/* Hover effect */
.final-horizontal-polaroid:hover {
  transform: translateY(-8px) scale(1.025) rotate(0deg) !important;
  box-shadow: 0 30px 62px rgba(37, 99, 235, 0.24);
}

/* Final message */
.final-image-message {
  text-align: center;
  font-size: 30px;
  font-weight: 900;
  color: #1d4ed8;
  margin-top: 34px;
  margin-bottom: 34px;
}

/* Smaller screen: allow wrapping */
@media (max-width: 1250px) {
  .final-horizontal-board {
    flex-wrap: wrap;
  }

  .final-horizontal-card {
    width: 44%;
  }
}

@media (max-width: 850px) {
  .final-horizontal-card {
    width: 88%;
  }

  .final-horizontal-text {
    text-align: center;
  }
}

      ")  # End raw CSS string
      
    )  # End tags$style
    
  ),  # End tags$head
  
  div(  # Create the custom top navigation area
    
    class = "dynamic-island",  # Apply the Dynamic Island CSS class
    
    h1("Global Air Quality Research Platform"),  # Display the main app title
    
    div(  # Create subtitle container
      
      class = "sub-title",  # Apply subtitle CSS class
      
      "PM2.5 Distribution & Pollutant Analysis"  # Display subtitle text
      
    ),  # End subtitle div
    
    # =========================
    # Navigation button group
    # =========================
    # These buttons are custom navigation buttons for the hidden tabsetPanel.
    # Only the first button has "active" at app startup.
    # Important: do NOT add "active" to all buttons, otherwise all buttons will be blue.
    div(
      class = "island-buttons",
      
      # Cleaning is active by default because the app starts on the Cleaning page.
      actionButton("go_cleaning", "Cleaning", class = "island-btn active"),
      
      # Other buttons should NOT have "active" initially.
      actionButton("go_correlation", "Correlation", class = "island-btn"),
      actionButton("go_ranking", "Ranking", class = "island-btn"),
      actionButton("go_profile", "Profile", class = "island-btn"),
      actionButton("go_map", "Map", class = "island-btn"),
      actionButton("go_radar", "Radar", class = "island-btn"),
      # Final page button
      
      # This button opens the final call-to-action page.
      
      actionButton("go_final", "Final", class = "island-btn")
    ),
    
    
  ),  # End Dynamic Island header
  
  
  # =========================
  # Top Control Bar: Language + BGM
  # =========================
  # This section is placed below the Dynamic Island header.
  # It contains language buttons and background music controls.
  div(
    class = "top-control-bar",
    
    # -------------------------
    # Hidden audio element
    # -------------------------
    # The audio file should be saved in the www folder.
    # Example file path: www/bgm.mp3
    # The audio player itself is hidden because we use custom buttons.
    tags$audio(
      id = "bgm_audio",
      src = "bgm.mp3",
      type = "audio/mpeg",
      loop = TRUE
    ),
    
    # -------------------------
    # Language buttons
    # -------------------------
    # These buttons visually provide language options.
    # You can later connect them to translated text if needed.
    actionButton(
      inputId = "lang_en",
      label = "English",
      class = "lang-btn lang-btn-active"
    ),
    
    actionButton(
      inputId = "lang_zh",
      label = "中文",
      class = "lang-btn"
    ),
    
    actionButton(
      inputId = "lang_ja",
      label = "日本語",
      class = "lang-btn"
    ),
    
    # -------------------------
    # Background music buttons
    # -------------------------
    # Play button: starts the background music.
    actionButton(
      inputId = "play_bgm",
      label = "Play BGM",
      class = "bgm-btn play-bgm-btn",
      onclick = "document.getElementById('bgm_audio').play();"
    ),
    
    # Pause button: pauses the background music.
    actionButton(
      inputId = "pause_bgm",
      label = "Pause",
      class = "bgm-btn pause-bgm-btn",
      onclick = "document.getElementById('bgm_audio').pause();"
    )
  ),
  
  # =========================
  # Put your tabsetPanel(...) immediately below this line
  # =========================
  
  #titlePanel("Pollutant Correlation Heatmap After Data Cleaning"),
  
  tabsetPanel(
    id = "main_tabs",     # Unique ID for this tabset (used for button control)
    type = "hidden",      # Hide the default tab navigation bar (we will use buttons instead)
    
    # =========================
    # Page 1: Data Cleaning Summary
    # =========================
    # Create a tab panel in the main dashboard for cleaning statistics
    tabPanel(
      "Cleaning",
      
      # fluidRow creates a responsive horizontal row that adapts to screen size
      fluidRow(
        
        # Sidebar Column (Width 3 out of 12): Contains documentation text
        column(
          width = 3,
          
          wellPanel(
            class = "info-float",
            # Dynamic Markdown output for the Cleaning page.
            # The actual Markdown file is selected in the server by output$cleaning_about.
            # It changes according to the selected language:
            # English -> about/cleaning_en.md
            # Chinese -> about/cleaning_zh.md
            # Japanese -> about/cleaning_ja.md
            uiOutput("cleaning_about")
          )
        ),
        
        # Main Column (Width 9 out of 12): Contains charts and tables
        column(
          width = 9,
          
          # Modern card wrapper for the main content area
          div(
            class = "modern-card",
            
            # Section title for the cleaning-stage bar chart
            h3("Observation Count Before and After Cleaning"),
            
            # Bar chart generated from output$cleaning_count_plot in the server
            plotOutput("cleaning_count_plot", height = "520px"),
            
            br(),
            
            # Section title for the removed-country table
            h3("Removed Countries by Cleaning Rule"),
            
            # Table generated from output$removed_countries_table in the server
            tableOutput("removed_countries_table")
          )
        )
      )
    ),
    
    # =========================
    # Page 2: Pollutant Correlation
    # =========================
    
    # Define a tab panel for the correlation analysis
    tabPanel(
      "Correlation",
      
      # Organize the content into a responsive row
      fluidRow(
        # Left Column (Width 3/12): Sidebar containing app information and rules
        column(
          width = 3,
          wellPanel(
            class = "info-float",
            # Dynamic Markdown output for the Correlation page.
            # The actual Markdown file is selected in the server by output$correlation_about.
            # It changes according to the selected language:
            # English -> about/correlation_en.md
            # Chinese -> about/correlation_zh.md
            # Japanese -> about/correlation_ja.md
            uiOutput("correlation_about")
          )
        ),
        
        # Right Column (Width 9/12): Main display area for the heatmap
        column(
          width = 9,
          # Modern card for the main correlation heatmap
          div(
            class = "modern-card",
            h3("Pollutant Correlation Heatmap"),
            # plotlyOutput displays the interactive correlation heatmap
            # Set to a large height (900px) for high visibility of the matrix
            plotlyOutput("heatmap", height = "900px")
          )
        )
      ),
      
      # Create a new horizontal row located below the heatmap
      fluidRow(
        # Create a column that spans the full width of the page 
        column(
          width = 12,# (12 units in the Shiny grid system equals 100% width)
          
          # [Text Output]
          # Displays status information about the current selection
          # verbatimTextOutput("info"),
          
          # Modern card for selected-pair information and regression plot
          div(
            class = "modern-card",
            verbatimTextOutput("info"),
            
            # [Interactive Plot Output]
            # Displays the scatter plot and linear regression trend line.
            # Using plotlyOutput allows the user to hover over points for 
            # specific values and zoom in on data clusters.
            plotlyOutput("reg_plot", height = "750px")
          )
        )
      )
    ),
    
    # =========================
    # Page 3: PM2.5 Country Ranking
    # =========================
    
    # Define a tab panel for the global country ranking visualization
    tabPanel(
      "Ranking",
      
      # Organize the content into a responsive row
      fluidRow(
        
        # Left Column (Width 3/12): Sidebar containing ranking context
        column(
          width = 3,
          
          wellPanel(
            class = "info-float",
            # Dynamic Markdown output for the Ranking page.
            # The actual Markdown file is selected in the server by output$ranking_about.
            # It changes according to the selected language:
            # English -> about/ranking_en.md
            # Chinese -> about/ranking_zh.md
            # Japanese -> about/ranking_ja.md
            uiOutput("ranking_about")
          )
        ),
        
        # Right Column (Width 9/12): Main display area for the ranking chart
        column(
          width = 9,
          # Modern card for the PM2.5 ranking chart
          div(
            class = "modern-card",
            h3("Final Country Ranking by Average PM2.5"),
            # plotOutput displays the ranking bar chart generated in the server logic
            # Set to a large height (720px) to accommodate many country bars
            plotOutput("pm25_ranking_plot", height = "720px")
          )
        )
      )
    ),
    
    # =========================
    # Page 4: Country PM2.5 Profile
    # =========================
    
    # Define a tab panel for analyzing pollutant correlations inside specific countries
    tabPanel(
      "Profile",
      # Left Column (Width 3/12): Documentation and instructions
      fluidRow(
        column(
          width = 3,
          wellPanel(
            class = "info-float",
            # Dynamic Markdown output for the Country Profile page.
            # The actual Markdown file is selected in the server by output$profile_about.
            # It changes according to the selected language:
            # English -> about/profile_en.md
            # Chinese -> about/profile_zh.md
            # Japanese -> about/profile_ja.md
            uiOutput("profile_about")
          )
        ),
        # Right Column (Width 9/12): The interactive country-pollutant heatmap
        column(
          width = 9,
          # Modern card for the country-pollutant heatmap
          
          div(
            class = "modern-card",
            h3("Country PM2.5 Pollutant Profile"),
            
            # Displays the correlation matrix for specific countries
            plotlyOutput("country_pollutant_heatmap", height = "800px")
          )
        )
      ),
      # Bottom Row: Dedicated space for the regression analysis of a clicked tile
      fluidRow(
        column(
          width = 12,
          # Modern card for selected country-pollutant information and regression plot
          div(
            class = "modern-card",
            verbatimTextOutput("country_profile_info"),
            
            # Shows text info about the specific country/pollutant selected
            verbatimTextOutput("country_profile_info"),
            # Renders the scatter plot and trend line for the selected country
            plotlyOutput("country_profile_reg_plot", height = "720px")
          )
        )
      )
    ),
    
    # =========================
    # Page 5: Global PM2.5 Map
    # =========================
    # Define a tab panel for the geospatial visualization
    tabPanel(
      "Map",
      
      # Add a controlled vertical gap below the sticky Dynamic Island header.
      # Compared with br(), this gives more stable spacing for large Leaflet maps.
      # It helps prevent the header from visually covering the top of the map page.
      div(style = "height: 18px;"),
      
      fluidRow(
        
        # Left Column (Width 3/12): Navigation and Control Panel
        column(
          width = 3,
          wellPanel(
            class = "info-float",
            # Dynamic Markdown output for the Map page.
            # The actual Markdown file is selected in the server by output$map_about.
            # It changes according to the selected language:
            # English -> about/map_en.md
            # Chinese -> about/map_zh.md
            # Japanese -> about/map_ja.md
            uiOutput("map_about"),
            
            # Separator between the page explanation and the interactive controls.
            tags$hr(),
            
            # Reset Button: Instantly zooms the map back to a full global view
            actionButton(
              "btn_view_world", 
              "Reset to World View", 
              class = "view-all-btn",
              style = "width: 100%; margin-bottom: 20px; background-color: #2c3e50; color: white; border: none;"
            ),
            # Primary Selection Inputs
            div(
              class = "control-group",
              # Allows users to choose how the country dropdown is organized
              selectInput(
                "sort_order", 
                "1. Sort Countries By:", 
                choices = c(
                  "Alphabetical (A-Z)" = "alphabet", 
                  "PM2.5 High to Low" = "high_low", 
                  "PM2.5 Low to High" = "low_high"
                )
              ),
              # Select a specific country to zoom in on (choices populated by server)
              selectizeInput(
                "selected_country", 
                "2. Focus on Country:", 
                choices = NULL,
                
                selected = "none",
                
                # Default selection is View World.
                
                #options = list(
                
                #maxOptions = 3
                
                #dropdownParent = "body"
                
                #)
              )
            ),
            
            tags$hr(style = "margin-top: 170px; margin-bottom: 24px;"), # Horizontal line separator
            # Quick Jump & Batch View Section
            div(
              class = "control-group",
              h4("Quick Jump & Batch View"),
              # Special button to auto-zoom so all 6 "extreme" countries are visible at once
              actionButton(
                "view_all_6", 
                "View All 6 Extremes", 
                class = "view-all-btn",
                style = "width: 100%; background-color: #34495e; color: white;"
              ),
              
              br(),
              # Group navigation for the most polluted countries
              strong("Highest PM2.5 Group:"),
              actionButton(
                "view_top3", 
                "Preview Group", 
                style = "padding: 2px 8px; font-size: 0.7em; margin-left: 10px; background-color: #f8d7da; border: 1px solid #f5c6cb;"
              ),
              div(style = "margin-top: 8px;", uiOutput("top3_buttons")), # Container for individual country buttons (Top 3)
              
              br(),
              
              strong("Lowest PM2.5 Group:"), # Group navigation for the cleanest countries
              actionButton(
                "view_bot3", 
                "Preview Group", 
                style = "padding: 2px 8px; font-size: 0.7em; margin-left: 10px; background-color: #d4edda; border: 1px solid #c3e6cb;"
              ),
              div(style = "margin-top: 8px;", uiOutput("bot3_buttons")) # Container for individual country buttons (Bottom 3)
            ),
            
            tags$hr(), # Display the measurement unit for context
            strong("Legend Info"),
            p(paste0("Average PM2.5 (", pm25_unit, ")"))
          )
        ),
        # Right Column (Width 9/12): The Interactive Map
        column(
          width = 9, 
          
          # Modern card for the interactive PM2.5 world map
          div(
            class = "modern-card",
            h3("Global PM2.5 Concentration Distribution"),
            
            leafletOutput("main_map", height = "730px") # leafletOutput renders the dynamic map using OpenStreetMap data
          )
        )
      )
    ),
    
    # =========================
    # Page 6: GDP + Geography Radar
    # =========================
    # This standalone page compares the selected six countries using
    # PM2.5, GDP in 2024, latitude, longitude, PM2.5 record count,
    # and pollutant type count.
    tabPanel(
      "Radar",
      
      # Add a controlled vertical gap below the sticky Dynamic Island header.
      # This prevents the radar chart and selected-country map from starting too close
      # to the floating navigation bar.
      div(style = "height: 18px;"),
      
      fluidRow(
        
        # Left Column: explanation + control panel
        column(
          width = 3,
          
          wellPanel(
            class = "info-float radar-float",
            # Dynamic Markdown output for the Radar page.
            # The actual Markdown file is selected in the server by output$radar_about.
            # It changes according to the selected language:
            # English -> about/radar_en.md
            # Chinese -> about/radar_zh.md
            # Japanese -> about/radar_ja.md
            uiOutput("radar_about"),
            
            # Horizontal separator between the explanation and the input control.
            tags$hr(),
            
            # Country selector for the six selected PM2.5 countries.
            # When the user selects a country, the radar chart and country map update.
            selectInput(
              inputId = "geo_radar_country",
              label = "Select country:",
              choices = selected_countries,
              selected = selected_countries[1]
            )
          )
        ),
        
        # Right Column: radar chart + selected country map + raw data table
        column(
          width = 9,
          
          # Modern card wrapper for the main Radar page content.
          div(
            class = "modern-card",
            
            # Nested row: radar chart on the left, map on the right.
            fluidRow(
              
              # Left side: animated radar chart.
              column(
                width = 7,
                h3("GDP + Geography Radar Profile"),
                
                # This output is generated in server by output$geo_gdp_radar_plot.
                plotlyOutput("geo_gdp_radar_plot", height = "560px")
              ),
              
              # Right side: selected country map.
              column(
                width = 5,
                h3("Selected Country Map"),
                
                # This output is generated in server by output$geo_country_map.
                leafletOutput("geo_country_map", height = "560px")
              )
            ),
            
            # Add vertical spacing before the table.
            br(),
            
            # Raw data table title.
            h4("Raw Indicator Values"),
            
            # This output is generated in server by output$geo_gdp_profile_table.
            tableOutput("geo_gdp_profile_table")
          )
        )
      )
    ),
    
    # =========================
    # Page 7: Final Call to Action
    # =========================
    # This final page uses three pollution-related images.
    # The images are shown as animated polaroid-style cards.
    # Each image includes a clickable source link for proper attribution.
    
    tabPanel(
      "Final",
      
      # Add vertical spacing below the sticky header.
      div(style = "height: 18px;"),
      
      fluidRow(
        
        # =========================
        # Left Column: Floating explanation card
        # =========================
        column(
          width = 3,
          
          wellPanel(
            class = "info-float",
            
            # Dynamic Markdown output for the Final page.
            # The actual Markdown file is selected in the server by output$final_about.
            # It changes according to the selected language:
            # English -> about/final_en.md
            # Chinese -> about/final_zh.md
            # Japanese -> about/final_ja.md
            uiOutput("final_about")
          )
        ),
        
        # =========================
        # Right Column: Final visual board
        # =========================
        # Right Column: final visual call-to-action page
        # Right Column: final visual call-to-action page
        column(
          width = 9,
          
          # =========================
          # Three final story cards in one horizontal row
          # =========================
          div(
            class = "final-horizontal-board",
            
            # =========================
            # Card 1: Industrial emissions
            # =========================
            div(
              class = "final-horizontal-card",
              
              # Text block
              div(
                class = "final-horizontal-text",
                
                h3("Industrial Emissions"),
                
                p(
                  "Factory smoke is one visible source of air pollution. Industrial emissions can release fine particulate matter into the atmosphere and worsen PM2.5 pollution."
                ),
                
                tags$a(
                  "Image credit: iStock / Maksim Shmakov",
                  href = "https://www.istockphoto.com/hk/%E7%85%A7%E7%89%87/%E7%94%9F%E6%85%8B%E7%81%BD%E9%9B%A3-gm1141520118-305861595",
                  target = "_blank",
                  class = "image-credit"
                )
              ),
              
              # Polaroid image block
              div(
                class = "final-horizontal-polaroid rotate-left",
                
                tags$img(
                  src = "istockphoto-1141520118-1024x1024.jpg",
                  class = "final-horizontal-img"
                )
              )
            ),
            
            # =========================
            # Card 2: Human exposure
            # =========================
            div(
              class = "final-horizontal-card",
              
              # Text block
              div(
                class = "final-horizontal-text",
                
                h3("Human Exposure"),
                
                p(
                  "Air pollution is not only an environmental issue. When air quality becomes poor, people may need masks or other protection, especially during haze or smog events."
                ),
                
                tags$a(
                  "Image credit: iStock / Boyloso",
                  href = "https://www.istockphoto.com/hk/%E7%85%A7%E7%89%87/asian-woman-wearing-an-n95-mask-for-protect-bad-air-pollution-gm2150501657-571664864",
                  target = "_blank",
                  class = "image-credit"
                )
              ),
              
              # Polaroid image block
              div(
                class = "final-horizontal-polaroid rotate-middle",
                
                tags$img(
                  src = "istockphoto-2150501657-1024x1024.jpg",
                  class = "final-horizontal-img"
                )
              )
            ),
            
            # =========================
            # Card 3: Traffic and haze
            # =========================
            div(
              class = "final-horizontal-card",
              
              # Text block
              div(
                class = "final-horizontal-text",
                
                h3("Traffic and Haze"),
                
                p(
                  "Urban traffic can worsen air quality when vehicle emissions accumulate. Under hazy weather conditions, pollutants may remain in the air and increase exposure risks."
                ),
                
                tags$a(
                  "Image credit: iStock / plherrera",
                  href = "https://www.istockphoto.com/hk/%E7%85%A7%E7%89%87/cars-at-rush-hour-driving-through-thick-smog-gm174655376-8276806",
                  target = "_blank",
                  class = "image-credit"
                )
              ),
              
              # Polaroid image block
              div(
                class = "final-horizontal-polaroid rotate-right",
                
                tags$img(
                  src = "istockphoto-174655376-1024x1024.jpg",
                  class = "final-horizontal-img"
                )
              )
            )
          )
        ))
      
    )))

# =========================
# 15. Server
# =========================
server <- function(input, output, session) {
  # =========================
  # Dynamic Island Button Navigation
  # =========================
  # =========================
  # Dynamic Island Button Navigation + Page Explanation Modals
  # =========================
  # This section controls:
  # 1. Which hidden tab/page is displayed after clicking a top navigation button.
  # 2. Which navigation button should be highlighted as the active page.
  # 3. Which explanation popup should appear after clicking each page button.
  
  
  # ------------------------------------------------------------
  # Helper function: update active button style
  # ------------------------------------------------------------
  # This function removes the "active" class from all navigation buttons,
  # then adds the "active" class only to the button that was clicked.
  # It makes the current page button visually highlighted.
  set_active_button <- function(active_id) {
    shinyjs::runjs(
      paste0(
        "$('.island-btn').removeClass('active');",
        "$('#", active_id, "').addClass('active');"
      )
    )
  }
  
  
  # ------------------------------------------------------------
  # Helper function: reusable modal style
  # ------------------------------------------------------------
  # This function creates a consistent popup explanation box.
  # It avoids repeating the same modalDialog structure many times.
  show_page_modal <- function(title, content) {
    showModal(
      modalDialog(
        title = title,          # Popup title
        easyClose = TRUE,       # Allow closing by clicking outside the modal
        size = "m",             # Medium-sized modal
        
        # Main modal content
        div(
          style = "
          font-size: 15px;
          line-height: 1.7;
          color: #2d3748;
        ",
          content
        ),
        
        # Footer button
        footer = modalButton("Close")
      )
    )
  }
  
  
  # ------------------------------------------------------------
  # Cleaning page button
  # ------------------------------------------------------------
  # ============================================================
  # Dynamic Island Button Navigation + Popup Explanations
  # ============================================================
  # Each navigation button does three things:
  # 1. Switch the hidden tabsetPanel to the selected page.
  # 2. Update the Dynamic Island button style:
  #    - clicked/current button becomes blue
  #    - all other buttons return to white
  # 3. Show a popup explanation for the selected page.
  # ============================================================
  
  
  # ------------------------------------------------------------
  # Cleaning page button
  # ------------------------------------------------------------
  observeEvent(input$go_cleaning, {
    
    # Switch the hidden tabsetPanel to the Cleaning page.
    updateTabsetPanel(
      session = session,
      inputId = "main_tabs",
      selected = "Cleaning"
    )
    
    # Make the Cleaning button blue and reset all other buttons to white.
    set_active_button("go_cleaning")
    
    # Show a popup explanation based on the Cleaning sidebar content.
    show_page_modal(
      title = "Cleaning Page Explanation",
      content = tagList(
        
        h4("Purpose"),
        p("This page summarizes the data-cleaning process before visualization."),
        p("It shows how many observations were kept after each cleaning step."),
        p("It also lists countries removed during PM2.5 country-level cleaning."),
        
        tags$hr(),
        
        h4("Cleaning Logic"),
        p("First, missing or invalid observations are removed."),
        p("Then, countries with unreliable PM2.5 records are removed."),
        p("Countries with fewer than 3 PM2.5 observations or all-zero PM2.5 values are excluded."),
        
        tags$hr(),
        
        h4("How to Read This Page"),
        p("The bar chart shows how the number of observations changes across cleaning stages."),
        p("The removed-country table explains which countries were excluded and why."),
        p("This cleaning step makes the later PM2.5 ranking, profile, map, and radar analysis more reliable.")
      )
    )
  })
  
  
  # ------------------------------------------------------------
  # Correlation page button
  # ------------------------------------------------------------
  observeEvent(input$go_correlation, {
    
    # Switch the hidden tabsetPanel to the Correlation page.
    updateTabsetPanel(
      session = session,
      inputId = "main_tabs",
      selected = "Correlation"
    )
    
    # Make the Correlation button blue and reset all other buttons to white.
    set_active_button("go_correlation")
    
    # Show a popup explanation based on the Correlation sidebar content.
    show_page_modal(
      title = "Correlation Page Explanation",
      content = tagList(
        
        h4("App Functions"),
        p("1. Clean invalid air-quality observations."),
        p("2. Remove countries with unreliable PM2.5 data."),
        p("3. Calculate country-level average pollutant values."),
        p("4. Display pollutant-by-pollutant correlations."),
        p("5. Click a non-diagonal cell to generate a regression plot."),
        
        tags$hr(),
        
        h4("Cleaning Rules"),
        p("1. Remove missing country, pollutant, and value."),
        p("2. Remove negative pollutant values."),
        p("3. Remove countries with fewer than 3 PM2.5 observations."),
        p("4. Remove countries whose PM2.5 values are all zero."),
        
        tags$hr(),
        
        h4("How to Read This Page"),
        p("Each heatmap cell shows the correlation between two pollutants."),
        p("Redder cells indicate stronger positive relationships, while bluer cells indicate stronger negative relationships."),
        p("Diagonal cells are disabled because self-correlation is always 1."),
        p("Only non-diagonal cells can be clicked to generate a regression plot.")
      )
    )
  })
  
  
  # ------------------------------------------------------------
  # Ranking page button
  # ------------------------------------------------------------
  observeEvent(input$go_ranking, {
    
    # Switch the hidden tabsetPanel to the Ranking page.
    updateTabsetPanel(
      session = session,
      inputId = "main_tabs",
      selected = "Ranking"
    )
    
    # Make the Ranking button blue and reset all other buttons to white.
    set_active_button("go_ranking")
    
    # Show a popup explanation based on the Ranking sidebar content.
    show_page_modal(
      title = "PM2.5 Ranking Page Explanation",
      content = tagList(
        
        h4("PM2.5 Ranking"),
        p("This page shows the final country ranking by average PM2.5 after data cleaning."),
        p("Countries are sorted from highest to lowest average PM2.5."),
        
        tags$hr(),
        
        h4("Top 3 Polluted Countries"),
        p(paste(top3_pm25_after$country, collapse = ", ")),
        
        tags$hr(),
        
        h4("Bottom 3 Cleanest Countries"),
        p(paste(bottom3_pm25_after$country, collapse = ", ")),
        
        tags$hr(),
        
        h4("How to Read This Page"),
        p("The x-axis represents country ranking after cleaning."),
        p("The y-axis represents average PM2.5 concentration."),
        p("Higher bars indicate countries with higher average PM2.5 levels."),
        p("The top 3 and bottom 3 countries are highlighted because they are used in later profile, map, and radar pages."),
        
        tags$hr(),
        
        h4("PM2.5 Unit"),
        p(pm25_unit)
      )
    )
  })
  
  
  # ------------------------------------------------------------
  # Profile page button
  # ------------------------------------------------------------
  observeEvent(input$go_profile, {
    
    # Switch the hidden tabsetPanel to the Profile page.
    updateTabsetPanel(
      session = session,
      inputId = "main_tabs",
      selected = "Profile"
    )
    
    # Make the Profile button blue and reset all other buttons to white.
    set_active_button("go_profile")
    
    # Show a popup explanation based on the Profile sidebar content.
    show_page_modal(
      title = "Country PM2.5 Profile Page Explanation",
      content = tagList(
        
        h4("Country Profile"),
        p("This page shows within-country correlations between PM2.5 and other pollutants."),
        p("The x-axis shows the top 3 and bottom 3 PM2.5 countries after cleaning."),
        p("The color represents the correlation between PM2.5 and each pollutant inside the selected country."),
        p("Blank cells mean that there are not enough paired observations for a reliable correlation."),
        
        tags$hr(),
        
        h4("Countries Included"),
        p(paste(selected_countries, collapse = ", ")),
        
        tags$hr(),
        
        h4("PM2.5 Unit"),
        p(pm25_unit),
        
        tags$hr(),
        
        h4("How to Use This Page"),
        p("Hover over each tile to see country, pollutant, correlation, paired observations, and unit."),
        p("Click a tile to analyze the relationship between PM2.5 and the selected pollutant within that country."),
        
        tags$hr(),
        
        h4("Why Ghana May Not Appear in the Heatmap"),
        p("Ghana is included in the selected countries because it is one of the top PM2.5 countries after cleaning."),
        p("However, this Profile heatmap only displays country-pollutant pairs that have enough valid paired observations."),
        p("A tile is shown only when PM2.5 and another pollutant have at least 3 paired observations within the same country."),
        p("If Ghana does not appear, it usually means Ghana does not have enough paired PM2.5-other pollutant observations, or one of the variables has no variation, so a reliable correlation cannot be calculated."),
        p("This is why Ghana can appear in the PM2.5 ranking page but may be missing from the Profile heatmap.")
      )
    )
  })
  
  
  # ------------------------------------------------------------
  # Map page button
  # ------------------------------------------------------------
  observeEvent(input$go_map, {
    
    # Switch the hidden tabsetPanel to the Map page.
    updateTabsetPanel(
      session = session,
      inputId = "main_tabs",
      selected = "Map"
    )
    
    # Make the Map button blue and reset all other buttons to white.
    set_active_button("go_map")
    
    # Show a popup explanation for the Map page.
    show_page_modal(
      title = "Global PM2.5 Map Page Explanation",
      content = tagList(
        
        h4("Global PM2.5 Map"),
        p("This page visualizes average PM2.5 concentration across countries on a world map."),
        p("Countries are colored by their average PM2.5 level after data cleaning."),
        p("Darker or warmer colors indicate higher PM2.5 concentration, while lighter or greener colors indicate lower PM2.5 concentration."),
        
        tags$hr(),
        
        h4("Interactive Controls"),
        p("You can sort countries alphabetically, from high to low PM2.5, or from low to high PM2.5."),
        p("You can select a country from the dropdown list to automatically zoom to that country."),
        p("You can also use the quick buttons to view the top 3 polluted countries, bottom 3 cleanest countries, or all 6 extreme countries together."),
        
        tags$hr(),
        
        h4("Map Interpretation"),
        p("This map helps compare the geographic distribution of PM2.5 pollution across countries."),
        p("It is useful for identifying regional pollution patterns and locating countries with extreme PM2.5 values.")
      )
    )
  })
  
  
  # ------------------------------------------------------------
  # Radar page button
  # ------------------------------------------------------------
  observeEvent(input$go_radar, {
    
    # Switch the hidden tabsetPanel to the Radar page.
    updateTabsetPanel(
      session = session,
      inputId = "main_tabs",
      selected = "Radar"
    )
    
    # Make the Radar button blue and reset all other buttons to white.
    set_active_button("go_radar")
    
    # Show a popup explanation for the Radar page.
    show_page_modal(
      title = "GDP + Geography Radar Page Explanation",
      content = tagList(
        
        h4("GDP + Geography Radar"),
        p("This page compares the selected six countries using multiple indicators in one radar chart."),
        p("The selected six countries come from the top 3 highest PM2.5 countries and the bottom 3 lowest PM2.5 countries after cleaning."),
        
        tags$hr(),
        
        h4("Indicators Used"),
        p("The radar chart includes average PM2.5, GDP in 2024, average latitude, average longitude, number of PM2.5 records, and number of pollutant types."),
        p("Because these indicators have different units and scales, all values are rescaled from 0 to 1 before plotting."),
        
        tags$hr(),
        
        h4("How to Use This Page"),
        p("Use the country selector to choose one of the six selected countries."),
        p("The radar chart shows the selected country's normalized profile."),
        p("The map on the right shows the geographic location of the selected country."),
        p("The raw indicator table below shows the original unscaled values for interpretation.")
      )
    )
  })
  
  # =========================
  # Language Button Highlight Logic
  # =========================
  # These observers only control the visual highlight of language buttons.
  # They do not translate the page content yet.
  # If translation is needed later, the page text should be stored reactively.
  set_active_language <- function(active_lang_id) {
    
    # Remove active style from all language buttons.
    shinyjs::runjs("
    $('.lang-btn').removeClass('lang-btn-active');
  ")
    
    # Add active style to the selected language button.
    shinyjs::runjs(
      paste0(
        "$('#", active_lang_id, "').addClass('lang-btn-active');"
      )
    )
  }
  
  # English button
  observeEvent(input$lang_en, {
    set_active_language("lang_en")
  })
  
  # Chinese button
  observeEvent(input$lang_zh, {
    set_active_language("lang_zh")
  })
  
  # Japanese button
  observeEvent(input$lang_ja, {
    set_active_language("lang_ja")
  })
  
  # =========================
  # Language Switching Logic
  # =========================
  # This reactive value stores the current language selected by the user.
  # "en" = English, "zh" = Chinese, "ja" = Japanese
  current_lang <- reactiveVal("en")
  
  # Opening page language buttons
  observeEvent(input$opening_lang_selected, {
    current_lang(input$opening_lang_selected)
    
    if (input$opening_lang_selected == "en") {
      updateActionButton(session, "start_app", label = "Start")
    }
    
    if (input$opening_lang_selected == "zh") {
      updateActionButton(session, "start_app", label = "开始")
    }
    
    if (input$opening_lang_selected == "ja") {
      updateActionButton(session, "start_app", label = "スタート")
    }
  })
  
  # =========================
  # Opening Page Typewriter Animation
  # =========================
  # This version waits until the UI is fully rendered before starting.
  # It fixes the issue where only the blinking cursor "|" appears.
  
  
  
  # Helper function: update active language button style
  # This function makes only the selected language button visually active.
  set_active_language <- function(active_lang_id) {
    shinyjs::runjs("
    $('.lang-btn').removeClass('lang-btn-active');
  ")
    
    shinyjs::runjs(
      paste0(
        "$('#", active_lang_id, "').addClass('lang-btn-active');"
      )
    )
  }
  
  
  # English button
  observeEvent(input$lang_en, {
    current_lang("en")
    set_active_language("lang_en")
    
    # Update Dynamic Island navigation labels to English
    updateActionButton(session, "go_cleaning", label = "Cleaning")
    updateActionButton(session, "go_correlation", label = "Correlation")
    updateActionButton(session, "go_ranking", label = "Ranking")
    updateActionButton(session, "go_profile", label = "Profile")
    updateActionButton(session, "go_map", label = "Map")
    updateActionButton(session, "go_radar", label = "Radar")
    updateActionButton(session, "go_final", label = "Final")
  })
  
  
  # Chinese button
  observeEvent(input$lang_zh, {
    current_lang("zh")
    set_active_language("lang_zh")
    
    # Update Dynamic Island navigation labels to Chinese
    updateActionButton(session, "go_cleaning", label = "数据清洗")
    updateActionButton(session, "go_correlation", label = "相关性")
    updateActionButton(session, "go_ranking", label = "排名")
    updateActionButton(session, "go_profile", label = "国家画像")
    updateActionButton(session, "go_map", label = "地图")
    updateActionButton(session, "go_radar", label = "雷达图")
    updateActionButton(session, "go_final", label = "结尾")
  })
  
  
  # Japanese button
  observeEvent(input$lang_ja, {
    current_lang("ja")
    set_active_language("lang_ja")
    
    # Update Dynamic Island navigation labels to Japanese
    updateActionButton(session, "go_cleaning", label = "クリーニング")
    updateActionButton(session, "go_correlation", label = "相関")
    updateActionButton(session, "go_ranking", label = "ランキング")
    updateActionButton(session, "go_profile", label = "プロファイル")
    updateActionButton(session, "go_map", label = "地図")
    updateActionButton(session, "go_radar", label = "レーダー")
    updateActionButton(session, "go_final", label = "終わり")
  })
  
  output$opening_markdown <- renderUI({
    if (current_lang() == "en") {
      return(includeMarkdown("about/opening_en.md"))
    }
    
    if (current_lang() == "zh") {
      return(includeMarkdown("about/opening_zh.md"))
    }
    
    if (current_lang() == "ja") {
      return(includeMarkdown("about/opening_ja.md"))
    }
  })
  
  # =========================
  # Dynamic Markdown Outputs
  # =========================
  # These outputs select different Markdown files based on current_lang().
  # This keeps includeMarkdown() in the project while allowing language switching.
  
  output$cleaning_about <- renderUI({
    if (current_lang() == "en") {
      return(includeMarkdown("about/cleaning_en.md"))
    }
    if (current_lang() == "zh") {
      return(includeMarkdown("about/cleaning_zh.md"))
    }
    if (current_lang() == "ja") {
      return(includeMarkdown("about/cleaning_ja.md"))
    }
  })
  
  output$correlation_about <- renderUI({
    if (current_lang() == "en") {
      return(includeMarkdown("about/correlation_en.md"))
    }
    if (current_lang() == "zh") {
      return(includeMarkdown("about/correlation_zh.md"))
    }
    if (current_lang() == "ja") {
      return(includeMarkdown("about/correlation_ja.md"))
    }
  })
  
  output$ranking_about <- renderUI({
    if (current_lang() == "en") {
      return(includeMarkdown("about/ranking_en.md"))
    }
    if (current_lang() == "zh") {
      return(includeMarkdown("about/ranking_zh.md"))
    }
    if (current_lang() == "ja") {
      return(includeMarkdown("about/ranking_ja.md"))
    }
  })
  
  output$profile_about <- renderUI({
    if (current_lang() == "en") {
      return(includeMarkdown("about/profile_en.md"))
    }
    if (current_lang() == "zh") {
      return(includeMarkdown("about/profile_zh.md"))
    }
    if (current_lang() == "ja") {
      return(includeMarkdown("about/profile_ja.md"))
    }
  })
  
  output$map_about <- renderUI({
    if (current_lang() == "en") {
      return(includeMarkdown("about/map_en.md"))
    }
    if (current_lang() == "zh") {
      return(includeMarkdown("about/map_zh.md"))
    }
    if (current_lang() == "ja") {
      return(includeMarkdown("about/map_ja.md"))
    }
  })
  
  output$radar_about <- renderUI({
    if (current_lang() == "en") {
      return(includeMarkdown("about/radar_en.md"))
    }
    if (current_lang() == "zh") {
      return(includeMarkdown("about/radar_zh.md"))
    }
    if (current_lang() == "ja") {
      return(includeMarkdown("about/radar_ja.md"))
    }
  })
  
  # Final page floating explanation card
  # This output changes the Final page explanation according to current_lang().
  output$final_about <- renderUI({
    if (current_lang() == "en") {
      return(includeMarkdown("about/final_en.md"))
    }
    if (current_lang() == "zh") {
      return(includeMarkdown("about/final_zh.md"))
    }
    if (current_lang() == "ja") {
      return(includeMarkdown("about/final_ja.md"))
    }
  })
  
  # =========================
  # Reactive values
  # =========================
  # Reactive values act as "placeholders" that store user choices (like clicks) 
  # so other parts of the app can react to them.
  selected <- reactiveVal(NULL) # Stores pollutant pairs from Page 2
  selected_country_profile <- reactiveVal(NULL) # Stores country-pollutant pairs from Page 4
  
  # =========================
  # Page 1 static cleaning plot
  # =========================
  
  # This plot shows how many observations remain after each data cleaning step.
  output$cleaning_count_plot <- renderPlot({
    ggplot(
      cleaning_count_summary,
      aes(x = stage, y = count)
    ) +
      geom_col(fill = "#4E79A7", width = 0.65) + # Bar chart of observations left
      geom_text(
        aes(label = count),
        vjust = -0.3,
        size = 5,
        fontface = "bold"
      ) +
      labs(
        title = "Observation Count Across Cleaning Stages",
        x = NULL,
        y = "Number of Observations"
      ) +
      theme_minimal(base_size = 16) +
      theme(
        plot.title = element_text(face = "bold", size = 18),
        axis.text.x = element_text(angle = 15, hjust = 1),
        axis.title.y = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      ) +
      expand_limits(y = max(cleaning_count_summary$count) * 1.15) # Expand y-axis to ensure labels don't get cut off at the top
  }) # Expands the y-axis so the labels above the bars are not cut off.
  
  # Display removed countries table
  output$removed_countries_table <- renderTable({
    removed_countries_table
  })
  
  # =========================
  # Page 3 static PM2.5 ranking plot
  # =========================
  
  # This plot shows ranking of countries based on average PM2.5 after cleaning.
  output$pm25_ranking_plot <- renderPlot({
    ggplot(
      pm25_ranking_data,
      aes(x = rank, y = mean_pm25, fill = mean_pm25)
    ) +
      geom_col(width = 0.9) + # bar chart
      # Labels only show for Top 3 and Bottom 3 to keep the chart clean
      geom_text(
        aes(label = label),
        angle = 45,
        hjust = -0.05,
        vjust = 0.5,
        size = 3.3,
        fontface = "bold"
      ) +
      # Blue (Clean) to Red (Polluted)
      scale_fill_gradient(
        low = "#4E79A7", # low pollution (blue)
        high = "#D55E5E", # high pollution (red)
        name = "Average PM2.5"
      ) +
      labs(
        title = "Final Country Ranking by Average PM2.5 After Cleaning",
        subtitle = paste0(
          "Countries are sorted from highest to lowest average PM2.5. Unit: ",
          pm25_unit
        ),
        x = "Country ranking after cleaning",
        y = paste0("Average PM2.5 (", pm25_unit, ")")
      ) +
      theme_minimal(base_size = 15) +
      theme(
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 12),
        axis.text.x = element_blank(),  # hide x labels (too many countries)
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        panel.grid.minor = element_blank() # Hide x-axis text as there are too many countries
      ) +
      coord_cartesian(clip = "off") +
      expand_limits(y = max(pm25_ranking_data$mean_pm25, na.rm = TRUE) * 1.25)
  })
  
  # =========================
  # Page 2 click event
  # =========================
  # Listen for clicks on the Page 2 Global Heatmap
  observeEvent(event_data("plotly_click", source = "heatmap"), {
    
    click <- event_data("plotly_click", source = "heatmap")
    # If no valid click data, stop
    if (is.null(click) || is.null(click$x) || is.null(click$y)) {
      return()
    }
    # Translate numeric coordinates back into pollutant names
    x_var <- decode_x_pollutant(click$x)
    y_var <- decode_y_pollutant(click$y)
    # Ignore diagonal clicks (where the pollutant is compared to itself)
    if (is.na(x_var) || is.na(y_var)) {
      return()
    }
    # Ignore diagonal (same pollutant)
    if (x_var == y_var) {
      selected(NULL)
      return()
    }
    # Save selected pair
    selected(list(x = x_var, y = y_var))
  }, ignoreInit = TRUE)
  
  # =========================
  # Page 4 click event
  # =========================
  # Listen for clicks on the Page 4 Country Profile Heatmap
  observeEvent(event_data("plotly_click", source = "country_heatmap"), {
    # Get information about the tile clicked by the user
    click <- event_data("plotly_click", source = "country_heatmap")
    # If the clicked information is empty or the key is missing, stop.
    if (is.null(click) || is.null(click$key) || is.na(click$key)) {
      return()
    }
    # Split the unique key (e.g., "Vietnam::Ozone") into separate variables
    parts <- strsplit(as.character(click$key), "::", fixed = TRUE)[[1]]
    # Stop if the result of disassembly is less than two parts.
    if (length(parts) < 2) {
      return()
    }
    
    clicked_country <- parts[1] # The first part is the country
    clicked_pollutant <- parts[2] # The second part is pollutants
    # If the country or pollutant is empty, stop.
    if (is.na(clicked_country) || is.na(clicked_pollutant)) {
      return()
    }
    # Ignore PM2.5 self-selection
    if (clicked_pollutant == "PM2.5") {
      selected_country_profile(NULL)
      return()
    }
    # Save the country and pollutant clicked by the user
    selected_country_profile(
      list(
        country = clicked_country,
        pollutant = clicked_pollutant
      )
    )
  }, ignoreInit = TRUE)
  
  # =========================
  # Page 2 heatmap
  # =========================
  # Display selected pollutant pair
  output$heatmap <- renderPlotly({
    
    p <- ggplot(
      cor_long,
      aes(
        x = x, # x-axis contaminants
        y = y, # y-axis contaminants
        fill = cor, # Fill color represents correlation coefficient
        text = hover_text, # Text displayed when mouse hovers over the mouse
        key = click_key# The key used to identify the cell when clicked
      )
    ) +
      geom_tile(
        aes(alpha = ifelse(diagonal, 0.25, 1)), # Lower the transparency of the diagonal lines
        color = "white" # Use white to separate the tiles
      ) +
      geom_text(
        aes(label = label), # Display correlation coefficients in the grid
        size = 4
      ) +
      # Correlation coefficient colors:
      # Blue = Negative correlation
      # White = Close to 0
      # Red = Positive correlation
      scale_alpha_identity() + # Use the alpha value directly without remapping.
      scale_fill_gradient2(
        low = "#4575b4",
        mid = "white",
        high = "#d73027",
        midpoint = 0,
        limits = c(-1, 1),
        name = "cor"
      ) +
      coord_fixed() +
      labs(
        title = "Correlation Heatmap of Pollutants",
        x = "Pollutant",
        y = "Pollutant"
      ) +
      theme_minimal(base_size = 18) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold")
      )
    # ggplotly converts ggplot into an interactive plot
    ggplotly(p, tooltip = "text", source = "heatmap") %>%
      event_register("plotly_click") %>%
      layout(
        margin = list(l = 130, r = 80, b = 160, t = 80)
      )
  })
  
  # =========================
  # Page 4 heatmap
  # =========================
  # Display selected country and pollutant
  output$country_pollutant_heatmap <- renderPlotly({
    
    p <- ggplot(
      country_pollutant_heatmap,
      aes(
        x = country,
        y = pollutant,
        fill = correlation,
        text = hover_text,
        key = click_key
      )
    ) +
      geom_tile(
        color = "grey90",
        linewidth = 0.8,
        width = 0.98,
        height = 0.98
      ) + # Draw one tile for each country-pollutant correlation
      geom_text(
        aes(label = sprintf("%.2f", correlation)),# Display correlation coefficients to two decimal places
        size = 3.8
      ) + # Display correlation values inside the tiles
      scale_fill_gradient2(
        low = "#4575b4",
        mid = "white",
        high = "#d73027",
        midpoint = 0,
        limits = c(-1, 1),
        name = "Correlation",
        na.value = "white"
      ) + # Use blue-white-red color scale for correlations
      
      labs(
        title = "Within-Country Correlation with PM2.5",
        subtitle = paste0(
          "Only tiles with at least 3 paired observations are shown. PM2.5 unit: ",
          pm25_unit
        ),
        x = paste0("Top and Bottom PM2.5 Countries (PM2.5 unit: ", pm25_unit, ")"),
        y = "Pollutant"
      ) +
      theme_minimal(base_size = 16) +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 13),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(face = "bold", size = 15, margin = margin(t = 15)),
        axis.title.y = element_text(face = "bold", size = 15, margin = margin(r = 15)),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 20),
        plot.subtitle = element_text(size = 12)
      )
    # Convert to plotly and register click events
    ggplotly(p, tooltip = "text", source = "country_heatmap") %>%
      event_register("plotly_click") %>%
      layout(
        margin = list(l = 140, r = 80, b = 110, t = 100)
      )
  })
  
  # =========================
  # Page 2 selected info
  # =========================
  # Show which two pollutants the user selected in the heatmap on Page 2.
  output$info <- renderText({
    
    sel <- selected()
    
    if (is.null(sel)) {
      return("Click a non-diagonal cell in the heatmap to generate the regression plot.")
    }
    
    paste0("Selected: ", sel$x, " vs ", sel$y)
  })
  
  # =========================
  # Page 2 regression plot
  # =========================
  output$reg_plot <- renderPlotly({
    
    sel <- selected()
    req(sel)
    
    x_var <- sel$x
    y_var <- sel$y
    # Prepare country-level data for the selected two pollutants
    dat <- country_avg %>%
      select(country, all_of(x_var), all_of(y_var)) %>%
      rename(
        x = all_of(x_var),
        y = all_of(y_var)
      ) %>%
      filter(complete.cases(x, y)) %>%
      mutate(
        hover_text = paste0(
          "Country: ", country,
          "<br>", x_var, ": ", round(x, 2),
          "<br>", y_var, ": ", round(y, 2)
        )
      )
    # Require at least three complete observations for regression
    req(nrow(dat) >= 3)
    # If one variable has no variation, regression cannot be calculated
    if (sd(dat$x, na.rm = TRUE) == 0 || sd(dat$y, na.rm = TRUE) == 0) {
      return(
        plot_ly(
          data = dat,
          x = ~x,
          y = ~y,
          customdata = ~hover_text,
          hovertemplate = "%{customdata}<extra></extra>",
          type = "scatter",
          mode = "markers",
          marker = list(color = "rgba(40,40,40,0.55)", size = 8)
        ) %>%
          layout(
            title = paste0(
              x_var, " vs ", y_var,
              "<br><sup>Regression is not available because one variable has no variation.</sup>"
            ),
            xaxis = list(title = x_var),
            yaxis = list(title = y_var),
            showlegend = FALSE
          )
      )
    }
    # Label countries with extreme high or low values
    dat <- dat %>%
      mutate(
        label_country = ifelse(
          x > quantile(x, 0.90, na.rm = TRUE) |
            y > quantile(y, 0.90, na.rm = TRUE) |
            x < quantile(x, 0.10, na.rm = TRUE) |
            y < quantile(y, 0.10, na.rm = TRUE),
          country,
          ""
        )
      )
    
    # Fit a linear regression model
    fit <- lm(y ~ x, data = dat)
    
    # Generate x values for the regression line
    x_seq <- seq(min(dat$x), max(dat$x), length.out = 100)
    pred_data <- data.frame(x = x_seq)
    
    # Predict fitted values and confidence intervals
    pred <- tryCatch(
      predict(fit, newdata = pred_data, interval = "confidence"),
      error = function(e) NULL
    )
    
    if (is.null(pred)) {
      line_data <- pred_data %>%
        mutate(
          fit = predict(fit, newdata = pred_data),
          lwr = NA_real_,
          upr = NA_real_
        )
    } else {
      line_data <- pred_data %>%
        mutate(
          fit = pred[, "fit"],
          lwr = pred[, "lwr"],
          upr = pred[, "upr"]
        )
    }
    
    # Create regression equation text
    subtitle_text <- paste0(
      "y = ",
      round(coef(fit)[1], 2),
      " + ",
      round(coef(fit)[2], 2),
      "x"
    )
    
    # Start an empty Plotly object
    p <- plot_ly()
    
    # Add confidence interval ribbon if available
    if (all(!is.na(line_data$lwr)) && all(!is.na(line_data$upr))) {
      p <- p %>%
        add_ribbons(
          data = line_data,
          x = ~x,
          ymin = ~lwr,
          ymax = ~upr,
          fillcolor = "rgba(150,150,150,0.25)",
          line = list(color = "rgba(150,150,150,0)")
        )
    }
    
    # Add regression line and scatter points
    p %>%
      add_lines(
        data = line_data,
        x = ~x,
        y = ~fit,
        line = list(color = "royalblue", width = 3)
      ) %>%
      add_markers(
        data = dat,
        x = ~x,
        y = ~y,
        customdata = ~hover_text,
        text = ~label_country,
        hovertemplate = "%{customdata}<extra></extra>",
        mode = "markers+text",
        textposition = "top center",
        marker = list(
          color = "rgba(40,40,40,0.55)",
          size = 7
        ),
        textfont = list(
          color = "rgba(40,40,40,0.85)",
          size = 10
        )
      ) %>%
      layout(
        title = list(
          text = paste0(
            x_var, " vs ", y_var,
            "<br><sup>", subtitle_text, "</sup>"
          )
        ),
        xaxis = list(title = x_var),
        yaxis = list(title = y_var),
        margin = list(l = 80, r = 40, b = 80, t = 90),
        showlegend = FALSE
      )
  })
  
  # =========================
  # Page 4 selected info
  # =========================
  output$country_profile_info <- renderText({
    
    sel <- selected_country_profile()
    
    # Show instruction text if no tile has been selected
    if (is.null(sel)) {
      return("Click a tile to analyze the within-country relationship between PM2.5 and the selected pollutant.")
    }
    
    # Display the selected country and pollutant
    paste0(
      "Selected country: ", sel$country,
      " | Selected pollutant: ", sel$pollutant,
      " | Regression: PM2.5 vs ", sel$pollutant,
      " within ", sel$country
    )
  })
  
  # =========================
  # Page 4 regression plot
  # =========================
  output$country_profile_reg_plot <- renderPlotly({
    
    sel <- selected_country_profile()
    req(sel)
    
    clicked_country <- sel$country
    y_var <- sel$pollutant
    
    # Prepare paired PM2.5 and selected pollutant data within the selected country
    country_internal <- make_country_pair_data(clicked_country, y_var)
    
    # Stop if there are not enough paired observations
    if (nrow(country_internal) < 3) {
      return(
        plot_ly() %>%
          layout(
            title = paste0(
              clicked_country, ": PM2.5 vs ", y_var,
              "<br><sup>Not enough paired observations for regression.</sup>"
            ),
            xaxis = list(title = paste0("PM2.5 (", pm25_unit, ")")),
            yaxis = list(title = y_var)
          )
      )
    }
    
    # Prepare hover text and point labels
    dat <- country_internal %>%
      mutate(
        point_label = case_when(
          !is.na(city) & city != "" ~ city,
          !is.na(location) & location != "" ~ location,
          TRUE ~ clicked_country
        ),
        hover_text = paste0(
          "Country: ", clicked_country,
          "<br>Location: ", point_label,
          "<br>PM2.5: ", round(pm25, 2), " ", pm25_unit,
          "<br>", y_var, ": ", round(selected_value, 2)
        )
      ) %>%
      filter(
        !is.na(pm25),
        !is.na(selected_value)
      )
    
    # Stop if valid data are still insufficient after filtering
    if (nrow(dat) < 3) {
      return(
        plot_ly() %>%
          layout(
            title = paste0(
              clicked_country, ": PM2.5 vs ", y_var,
              "<br><sup>Not enough valid paired observations after filtering.</sup>"
            ),
            xaxis = list(title = paste0("PM2.5 (", pm25_unit, ")")),
            yaxis = list(title = y_var)
          )
      )
    }
    
    # If either variable has no variation, regression is not meaningful
    if (sd(dat$pm25, na.rm = TRUE) == 0 || sd(dat$selected_value, na.rm = TRUE) == 0) {
      return(
        plot_ly(
          data = dat,
          x = ~pm25,
          y = ~selected_value,
          customdata = ~hover_text,
          text = ~point_label,
          hovertemplate = "%{customdata}<extra></extra>",
          type = "scatter",
          mode = "markers+text",
          textposition = "top center",
          marker = list(
            color = "rgba(40,40,40,0.55)",
            size = 8
          )
        ) %>%
          layout(
            title = paste0(
              clicked_country, ": PM2.5 vs ", y_var,
              "<br><sup>Regression is not available because one variable has no variation.</sup>"
            ),
            xaxis = list(title = paste0("PM2.5 (", pm25_unit, ")")),
            yaxis = list(title = y_var),
            margin = list(l = 80, r = 40, b = 80, t = 90),
            showlegend = FALSE
          )
      )
    }
    
    # Fit the within-country linear regression model
    fit <- tryCatch(
      lm(selected_value ~ pm25, data = dat),
      error = function(e) NULL
    )
    
    # Stop if the regression model fails
    if (is.null(fit)) {
      return(
        plot_ly() %>%
          layout(
            title = paste0(
              clicked_country, ": PM2.5 vs ", y_var,
              "<br><sup>Regression model failed for this selected tile.</sup>"
            ),
            xaxis = list(title = paste0("PM2.5 (", pm25_unit, ")")),
            yaxis = list(title = y_var)
          )
      )
    }
    
    # Generate PM2.5 values for the regression line
    x_seq <- seq(min(dat$pm25), max(dat$pm25), length.out = 100)
    pred_data <- data.frame(pm25 = x_seq)
    
    # Predict fitted values and confidence intervals
    pred <- tryCatch(
      predict(fit, newdata = pred_data, interval = "confidence"),
      error = function(e) NULL
    )
    
    # Build regression line data
    if (is.null(pred)) {
      line_data <- pred_data %>%
        mutate(
          fit = predict(fit, newdata = pred_data),
          lwr = NA_real_,
          upr = NA_real_
        )
    } else {
      line_data <- pred_data %>%
        mutate(
          fit = pred[, "fit"],
          lwr = pred[, "lwr"],
          upr = pred[, "upr"]
        )
    }
    
    # Create regression equation subtitle
    subtitle_text <- paste0(
      "Within ", clicked_country,
      ": y = ",
      round(coef(fit)[1], 2),
      " + ",
      round(coef(fit)[2], 2),
      "x"
    )
    
    # Label extreme points
    dat <- dat %>%
      mutate(
        label_point = ifelse(
          pm25 > quantile(pm25, 0.90, na.rm = TRUE) |
            selected_value > quantile(selected_value, 0.90, na.rm = TRUE) |
            pm25 < quantile(pm25, 0.10, na.rm = TRUE) |
            selected_value < quantile(selected_value, 0.10, na.rm = TRUE),
          point_label,
          ""
        )
      )
    
    # Start an empty Plotly object
    p <- plot_ly()
    
    # Add confidence interval ribbon if available
    if (all(!is.na(line_data$lwr)) && all(!is.na(line_data$upr))) {
      p <- p %>%
        add_ribbons(
          data = line_data,
          x = ~pm25,
          ymin = ~lwr,
          ymax = ~upr,
          fillcolor = "rgba(150,150,150,0.25)",
          line = list(color = "rgba(150,150,150,0)")
        )
    }
    
    # Add regression line and scatter points
    p %>%
      add_lines(
        data = line_data,
        x = ~pm25,
        y = ~fit,
        line = list(color = "royalblue", width = 3)
      ) %>%
      add_markers(
        data = dat,
        x = ~pm25,
        y = ~selected_value,
        customdata = ~hover_text,
        text = ~label_point,
        hovertemplate = "%{customdata}<extra></extra>",
        mode = "markers+text",
        textposition = "top center",
        marker = list(
          color = "rgba(40,40,40,0.55)",
          size = 8
        ),
        textfont = list(
          color = "rgba(40,40,40,0.85)",
          size = 10
        )
      ) %>%
      layout(
        title = list(
          text = paste0(
            clicked_country, ": PM2.5 vs ", y_var,
            "<br><sup>", subtitle_text, "</sup>"
          )
        ),
        xaxis = list(title = paste0("PM2.5 (", pm25_unit, ")")),
        yaxis = list(title = y_var),
        margin = list(l = 80, r = 40, b = 80, t = 90),
        showlegend = FALSE
      )
  })
  
  # =========================
  # Page 5 map data
  # =========================
  # Tracks whether the map is showing the whole world, one country, or a group
  # Store the current map display mode:
  # "all" shows all available countries,
  # "single" shows one selected country,
  # "top3" shows the three most polluted countries,
  # "bot3" shows the three least polluted countries,
  # "all6" shows both top 3 and bottom 3 countries.
  map_view_mode <- reactiveVal("all")
  # Calculate the global range of PM2.5 values for consistent map color scaling
  global_range <- reactive({
    all_avgs <- pm25_clean %>%
      group_by(country) %>%
      summarise(avg_val = mean(value, na.rm = TRUE), .groups = "drop") %>%
      pull(avg_val)
    
    range(all_avgs, na.rm = TRUE)
  })
  # Calculate average PM2.5 and observation count for each country
  extreme_data <- reactive({
    pm25_clean %>%
      group_by(country) %>%
      summarise(
        avg_val = mean(value, na.rm = TRUE),
        n_obs = n(),
        .groups = "drop"
      )
  })
  # Reset button: Returns the map zoom and position to the global view
  observeEvent(input$btn_view_world, {
    map_view_mode("all")
    
    updateSelectInput(session, "selected_country", selected = "none")
    
    leafletProxy("main_map") %>% 
      flyTo(lng = 0, lat = 20, zoom = 2, options = list(duration = 1))
  })
  # Leaflet Base Rendering
  output$top3_buttons <- renderUI({
    top3 <- pm25_country_after %>% arrange(desc(mean_pm25)) %>% slice_head(n = 3)
    tagList(lapply(top3$country, function(cty) {
      actionButton(
        paste0("btn_", cty), 
        cty, 
        style = "margin: 2px; padding: 2px 6px; font-size: 0.8em; background-color: #fdecea;"
      )
    }))
  })
  
  output$bot3_buttons <- renderUI({
    bot3 <- pm25_country_after %>% arrange(mean_pm25) %>% slice_head(n = 3)
    tagList(lapply(bot3$country, function(cty) {
      actionButton(
        paste0("btn_", cty), 
        cty, 
        style = "margin: 2px; padding: 2px 6px; font-size: 0.8em; background-color: #e8f5e9;"
      )
    }))
  })
  
  # Update the country dropdown list based on the selected sorting method
  observeEvent(input$sort_order, {
    sorted_data <- pm25_country_after
    
    if (input$sort_order == "alphabet") {
      sorted_list <- sort(sorted_data$country)
    } else if (input$sort_order == "high_low") {
      sorted_list <- sorted_data %>% arrange(desc(mean_pm25)) %>% pull(country)
    } else {
      sorted_list <- sorted_data %>% arrange(mean_pm25) %>% pull(country)
    }
    
    updateSelectInput(
      session, 
      "selected_country", 
      choices = c("View World" = "none", sorted_list)
    )
  })
  
  # Add click events to dynamically generated country buttons
  observe({
    for (cty in selected_countries) {
      local({
        country_id <- cty
        observeEvent(input[[paste0("btn_", country_id)]], {
          updateSelectInput(session, "selected_country", selected = country_id)
          map_view_mode("single")
        })
      })
    }
  })
  
  # ==========================
  # Helper function: fly to multiple countries
  # ==========================
  # This function is used to automatically zoom the map to multiple countries.
  fly_to_multiple <- function(country_names, duration = 1.2) {
    target_shapes <- world_sf %>% filter(name %in% country_names) # Find the map boundary data of the corresponding country
    
    # If a national border is found, calculate the bbox and fly over it.
    if (nrow(target_shapes) > 0) {
      bbox <- st_bbox(target_shapes)
      leafletProxy("main_map") %>% 
        flyToBounds(
          as.numeric(bbox[["xmin"]]), 
          as.numeric(bbox[["ymin"]]),
          as.numeric(bbox[["xmax"]]), 
          as.numeric(bbox[["ymax"]]),
          options = list(duration = duration, padding = c(50, 50))
        )
    }
  }
  
  # Click the "View All 6" button:
  # The map displays the top 3 countries with the highest PM2.5 levels and the bottom 3 countries with the lowest PM2.5 levels.
  observeEvent(input$view_all_6, {
    map_view_mode("all6")
    updateSelectInput(session, "selected_country", selected = "none")
    
    all_6 <- c(
      pm25_country_after %>% arrange(desc(mean_pm25)) %>% slice_head(n = 3) %>% pull(country),
      pm25_country_after %>% arrange(mean_pm25) %>% slice_head(n = 3) %>% pull(country)
    )
    
    fly_to_multiple(all_6, 1.2)
  })
  
  # Click the "View Top 3" button:
  # The map only displays the 3 countries with the highest PM2.5 levels.
  observeEvent(input$view_top3, {
    map_view_mode("top3")
    updateSelectInput(session, "selected_country", selected = "none")
    
    top3 <- pm25_country_after %>% arrange(desc(mean_pm25)) %>% slice_head(n = 3) %>% pull(country)
    
    fly_to_multiple(top3, 1.0)
  })
  
  # Click the "View Bottom 3" button:
  # The map only displays the 3 countries with the lowest PM2.5 levels.
  observeEvent(input$view_bot3, {
    map_view_mode("bot3")
    updateSelectInput(session, "selected_country", selected = "none")
    
    bot3 <- pm25_country_after %>% arrange(mean_pm25) %>% slice_head(n = 3) %>% pull(country)
    
    fly_to_multiple(bot3, 1.0)
  })
  
  # =========================
  # Selected country logic
  # =========================
  # Update map mode when user selects country from dropdown menu
  observeEvent(input$selected_country, {
    req(input$selected_country)
    # If the user selects View World
    if (input$selected_country == "none") {
      # If the current mode is not top3 / bot3 / all6, revert to all mode.
      if (!(map_view_mode() %in% c("top3", "bot3", "all6"))) {
        map_view_mode("all")
      }
      # If the user selected a specific country
    } else {
      map_view_mode("single")
    }
  }, ignoreInit = TRUE)
  
  # =========================
  # Leaflet Base Rendering
  # =========================
  # Initial map rendering
  output$main_map <- renderLeaflet({
    # Calculate the average PM2.5 for each country
    initial_data <- pm25_clean %>%
      group_by(country) %>%
      summarise(avg_val = mean(value, na.rm = TRUE), .groups = "drop")
    
    data_range <- range(initial_data$avg_val, na.rm = TRUE) # Get color range
    pal <- create_pal_function(data_range) # Create color function
    # Connect PM2.5 data with world map boundary data
    map_data <- world_sf %>%
      inner_join(initial_data, by = c("name" = "country"))
    
    # Generate Leaflet Map
    leaflet(map_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% # Use a light-colored background image
      setView(lng = 0, lat = 20, zoom = 2) %>% # Initial World Perspective
      # Add country polygons
      addPolygons(
        fillColor = ~pal(avg_val), # Color according to PM2.5 average
        fillOpacity = 0.58, # Fill transparency
        weight = 0.4, # National Boundary Line Thickness
        color = "#FFFFFF", # border color
        opacity = 1,
        label = ~paste0(name, ": ", round(avg_val, 2), " ", pm25_unit),
        highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.85)
      ) %>%
      # Add legend
      addLegend(
        pal = pal,
        values = ~avg_val,
        title = paste0("PM2.5 Average<br>(", pm25_unit, ")"),
        position = "bottomright",
        layerId = "mapLegend"
      )
  })
  
  # =========================
  # Dynamic map update
  # =========================
  # This observe will dynamically update which countries are displayed on the map based on user selection.
  observe({
    req(extreme_data(), global_range())
    
    current_data <- extreme_data()
    data_range <- global_range()
    pal <- create_pal_function(data_range)
    proxy <- leafletProxy("main_map")
    
    top3_names <- pm25_country_after %>% arrange(desc(mean_pm25)) %>% slice_head(n = 3) %>% pull(country) # Found the 3 countries with the highest PM2.5 levels
    bot3_names <- pm25_country_after %>% arrange(mean_pm25) %>% slice_head(n = 3) %>% pull(country) # Found the 3 countries with the lowest PM2.5 levels
    all_extreme_names <- c(top3_names, bot3_names) # Merge top3 and bottom3
    # Select the data to display based on the map mode
    if (map_view_mode() == "top3") {
      display_data <- current_data %>% filter(country %in% top3_names)
    } else if (map_view_mode() == "bot3") {
      display_data <- current_data %>% filter(country %in% bot3_names)
    } else if (map_view_mode() == "all6") {
      display_data <- current_data %>% filter(country %in% all_extreme_names)
    } else if (map_view_mode() == "single") {
      display_data <- current_data %>% filter(country == input$selected_country)
    } else {
      display_data <- current_data 
    }
    # Connect the filtered data to the map boundary
    map_data <- world_sf %>%
      inner_join(display_data, by = c("name" = "country"))
    # If there is no map data, clear the map layer.
    if (nrow(map_data) == 0) {
      proxy %>% clearShapes()
      return()
    }
    # Update the shapes of countries on the map
    proxy %>%
      clearShapes() %>%
      addPolygons(
        data = map_data,
        fillColor = ~pal(avg_val),
        # Selected country or top/bottom country (deeper)
        fillOpacity = ifelse(
          map_data$name == input$selected_country | 
            (map_view_mode() != "all" & map_data$name %in% all_extreme_names), 
          0.85, 
          0.58
        ),
        # The borders of the selected countries are thicker
        weight = ifelse(
          map_data$name == input$selected_country, 
          2.5, 
          ifelse(map_data$name %in% all_extreme_names, 1.2, 0.4)
        ),
        # Selected countries use black borders, top/bottom countries use gray borders
        color = ifelse(
          map_data$name == input$selected_country, 
          "#000000",
          ifelse(map_data$name %in% all_extreme_names, "#666666", "#FFFFFF")
        ),
        opacity = 1,
        label = ~paste0(name, ": ", round(avg_val, 2), " ", pm25_unit),
        highlightOptions = highlightOptions(weight = 3, color = "#222", fillOpacity = 0.9)
      )
    # If the map mode is single, it will automatically jump to the selected country.
    if (map_view_mode() == "single") {
      req(input$selected_country)
      
      if (input$selected_country != "none") {
        target_box <- map_data %>% filter(name == input$selected_country)
        # If the country is found, fly to the border area of that country.
        if (nrow(target_box) > 0) {
          bbox <- st_bbox(target_box)
          proxy %>% 
            flyToBounds(
              lng1 = as.numeric(bbox[["xmin"]]), 
              lat1 = as.numeric(bbox[["ymin"]]),
              lng2 = as.numeric(bbox[["xmax"]]), 
              lat2 = as.numeric(bbox[["ymax"]]),
              options = list(duration = 0.75, padding = c(40, 40))
            )
        }
      }
    }
  })
  # ============================================================
  # Page 6: Animated GDP + Geography Radar Plot
  # ============================================================
  # This plot shows the selected country's profile as an animated radar chart.
  # The animation expands the radar polygon from the center to the final shape.
  # ============================================================
  
  output$geo_gdp_radar_plot <- renderPlotly({
    
    # Make sure the user has selected a country
    req(input$geo_radar_country)
    
    # Get radar data for the selected country
    df <- geo_gdp_radar_data %>%
      filter(country == input$geo_radar_country) %>%
      arrange(indicator)
    
    # Create animation steps from small size to full size
    animation_steps <- seq(0.05, 1, length.out = 12)
    
    # Create one copy of the radar data for each animation frame
    df_anim <- purrr::map_dfr(seq_along(animation_steps), function(i) {
      df %>%
        mutate(
          animated_value = radar_value * animation_steps[i],
          frame_id = i
        )
    })
    
    # Close the radar polygon for every animation frame
    df_closed <- df_anim %>%
      group_by(frame_id) %>%
      group_modify(~ bind_rows(.x, .x[1, ])) %>%
      ungroup()
    
    # Draw animated radar chart
    plot_ly(
      data = df_closed,
      type = "scatterpolar",
      mode = "lines+markers",
      r = ~animated_value,
      theta = ~indicator,
      frame = ~frame_id,   # This line creates the Play animation
      fill = "toself",
      fillcolor = "rgba(69,117,180,0.30)",
      line = list(color = "#4575b4", width = 3),
      marker = list(color = "#4575b4", size = 6),
      text = ~paste0(
        "Country: ", country,
        "<br>Indicator: ", indicator,
        "<br>Raw value: ", round(raw_value, 3),
        "<br>Radar score: ", round(radar_value, 3)
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = paste0("Radar Profile: ", input$geo_radar_country),
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, 1),
            tickvals = c(0, 0.25, 0.5, 0.75, 1)
          )
        ),
        showlegend = FALSE,
        margin = list(l = 160, r = 80, t = 70, b = 130)
      ) %>%
      animation_opts(
        frame = 60,
        transition = 30,
        redraw = TRUE
      ) %>%
      animation_button(
        x = -0.08,
        y = -0.12,
        xanchor = "left",
        yanchor = "bottom"
      ) %>%
      animation_slider(hide = TRUE)
  })
  # ============================================================
  # Page 6: Selected Country Map
  # ============================================================
  # This map shows the geographic location of the country selected
  # in the GDP + Geography Radar page.
  # It helps users connect the radar profile with the country's location.
  # ============================================================
  
  output$geo_country_map <- renderLeaflet({
    
    # Make sure a country has been selected before drawing the map
    req(input$geo_radar_country)
    
    # Load world country boundaries as spatial data
    world_map <- rnaturalearth::ne_countries(
      scale = "medium",
      returnclass = "sf"
    )
    
    # Standardize country names so they match the country names in the selector
    world_map <- world_map %>%
      mutate(
        map_country = case_when(
          admin == "United States of America" ~ "United States",
          admin == "Republic of Korea" ~ "South Korea",
          admin == "Russian Federation" ~ "Russia",
          TRUE ~ admin
        )
      )
    
    # Extract the boundary of the selected country
    selected_shape <- world_map %>%
      filter(map_country == input$geo_radar_country)
    
    # Create the base map
    m <- leaflet(world_map) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      # Add all countries in light gray as background
      addPolygons(
        fillColor = "#E5E7EB",
        color = "#FFFFFF",
        weight = 0.5,
        fillOpacity = 0.45,
        opacity = 1
      )
    
    # If the selected country is found, highlight it and zoom to it
    if (nrow(selected_shape) > 0) {
      
      # Get bounding box of selected country for automatic zoom
      bbox <- sf::st_bbox(selected_shape)
      
      m <- m %>%
        addPolygons(
          data = selected_shape,
          fillColor = "#34495e",
          color = "#111111",
          weight = 2.5,
          fillOpacity = 0.75,
          opacity = 1,
          label = ~map_country
        ) %>%
        fitBounds(
          lng1 = as.numeric(bbox["xmin"]),
          lat1 = as.numeric(bbox["ymin"]),
          lng2 = as.numeric(bbox["xmax"]),
          lat2 = as.numeric(bbox["ymax"])
        )
    }
    
    # Return the final map
    m
  })
  
  # ------------------------------------------------------------
  # Final page button
  # ------------------------------------------------------------
  # This observer controls the Final navigation button.
  # When the user clicks Final, the app switches to the ending page,
  # highlights the Final button, and shows a short explanation modal.
  
  observeEvent(input$go_final, {
    
    # Switch the hidden tabsetPanel to the Final page.
    updateTabsetPanel(
      session = session,
      inputId = "main_tabs",
      selected = "Final"
    )
    
    # Make the Final button blue and reset all other navigation buttons to white.
    set_active_button("go_final")
    
    # Show a short explanation popup for the final page.
    show_page_modal(
      title = "Final Call to Action",
      content = tagList(
        
        h4("Why This Ending Matters"),
        
        p(
          "This final page uses polaroid-style images to remind users that air pollution is connected to industrial emissions, traffic haze, and human health."
        ),
        
        tags$hr(),
        
        h4("Main Message"),
        
        p(
          "Clean air should not be taken for granted. Understanding PM2.5 data helps us recognize why air pollution deserves public attention."
        )
      )
    )
  })
  # Check if the program ran successfully.
  print("App running successfully")
}

# run shinyApp
shinyApp(ui, server)