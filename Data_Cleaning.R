library(tidyverse)

# =========================
# 0. Auto-detect data path
# =========================
data_path <- if (file.exists("data/world_air_quality.csv")) {
  "data/world_air_quality.csv"
} else if (file.exists("world_air_quality.csv")) {
  "world_air_quality.csv"
} else {
  "../data/world_air_quality.csv"
}

# =========================
# 1. Read raw data
# =========================
air_raw <- read_delim(
  data_path,
  delim = ";",
  col_types = cols(.default = col_character()),
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

# =========================
# 2. Clean and standardize columns
# Keep ALL pollutants at this stage
# =========================
air_all <- air_raw %>%
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
  mutate(
    country = str_trim(country),
    country_code = str_trim(country_code),
    city = str_trim(city),
    location = str_trim(location),
    pollutant = str_trim(pollutant),
    unit = str_trim(unit),
    value = parse_number(value)
  ) %>%
  filter(
    !is.na(country),
    !is.na(pollutant),
    !is.na(value),
    value >= 0
  )

cat("Number of observations after basic cleaning:", nrow(air_all), "\n")

# =========================
# 3. Use PM2.5 only for country ranking and cleaning decision
# Other pollutants are still kept in air_all
# =========================
pm25_data <- air_all %>%
  filter(pollutant == "PM2.5")

cat("Number of valid PM2.5 observations:", nrow(pm25_data), "\n")

# =========================
# 4. Country-level PM2.5 summary before final cleaning
# =========================
pm25_country_before <- pm25_data %>%
  group_by(country) %>%
  summarise(
    n_obs_pm25 = n(),
    mean_pm25 = mean(value, na.rm = TRUE),
    min_pm25 = min(value, na.rm = TRUE),
    max_pm25 = max(value, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nTop 3 highest PM2.5 countries BEFORE cleaning:\n")
print(
  pm25_country_before %>%
    arrange(desc(mean_pm25)) %>%
    slice_head(n = 3)
)

cat("\nTop 3 lowest PM2.5 countries BEFORE cleaning:\n")
print(
  pm25_country_before %>%
    arrange(mean_pm25) %>%
    slice_head(n = 3)
)

# =========================
# 5. Unified country removal rule
# Rule 1: remove countries with fewer than 3 PM2.5 observations
# Rule 2: remove countries whose PM2.5 values are all zero
# =========================
removed_countries <- pm25_country_before %>%
  mutate(
    reason = case_when(
      n_obs_pm25 < 3 ~ "Less than 3 PM2.5 observations",
      min_pm25 == 0 & max_pm25 == 0 ~ "All PM2.5 values are 0",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(reason)) %>%
  arrange(country)


# =========================
# 6. Apply country-level cleaning to ALL pollutants
# This removes unreliable countries from the full dataset
# =========================
air_all_clean <- air_all %>%
  filter(!country %in% removed_countries$country)

pm25_clean <- air_all_clean %>%
  filter(pollutant == "PM2.5")

cat("\nNumber of observations after applying cleaning to all pollutants:", nrow(air_all_clean), "\n")
cat("Number of PM2.5 observations after cleaning:", nrow(pm25_clean), "\n")

# =========================
# 7. PM2.5 ranking after cleaning
# =========================
pm25_country_after <- pm25_clean %>%
  group_by(country) %>%
  summarise(
    n_obs_pm25 = n(),
    mean_pm25 = mean(value, na.rm = TRUE),
    min_pm25 = min(value, na.rm = TRUE),
    max_pm25 = max(value, na.rm = TRUE),
    .groups = "drop"
  )

top3_pm25_after <- pm25_country_after %>%
  arrange(desc(mean_pm25)) %>%
  slice_head(n = 3)

bottom3_pm25_after <- pm25_country_after %>%
  arrange(mean_pm25) %>%
  slice_head(n = 3)

cat("\nTop 3 highest PM2.5 countries AFTER cleaning:\n")
print(top3_pm25_after)

cat("\nTop 3 lowest PM2.5 countries AFTER cleaning:\n")
print(bottom3_pm25_after)

# =========================
# 8. Select countries based on PM2.5 ranking
# These countries can be used for later multi-pollutant analysis
# =========================
selected_countries <- c(
  top3_pm25_after$country,
  bottom3_pm25_after$country
)

cat("\nSelected countries based on PM2.5 ranking:\n")
print(selected_countries)

# =========================
# 9. Extract ALL pollutants for selected countries
# Use this for radar charts / multi-pollutant plots
# =========================
selected_all_pollutants <- air_all_clean %>%
  filter(country %in% selected_countries)

selected_pollutant_summary <- selected_all_pollutants %>%
  group_by(country, pollutant) %>%
  summarise(
    n_obs = n(),
    mean_value = mean(value, na.rm = TRUE),
    min_value = min(value, na.rm = TRUE),
    max_value = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(country, pollutant)

cat("\nAll pollutants for selected PM2.5-ranked countries:\n")
print(selected_pollutant_summary, n = Inf)
