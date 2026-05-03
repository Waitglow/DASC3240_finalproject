library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(lubridate)
library(stringr)
library(plotly)

# =========================================================
# Unified data pipeline
# Semicolon-delimited CSV -> all character -> standardized columns
# -> trimmed text -> parsed numeric values -> daily means
# All heatmaps below are derived from the same cleaned base table.
# =========================================================

# ---- 0. Auto-detect data path ----


# ---- 1. Read raw data (semicolon-separated) ----
air_raw <- read_delim(
  'C:/Users/user/Desktop/world_air_quality.csv',
  delim = ",",
  col_types = cols(.default = col_character()),
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

# ---- 2. Standardize columns ----
required_cols <- c(
  "Country Code", "City", "Location", "Coordinates", "Pollutant",
  "Source Name", "Unit", "Value", "Last Updated", "Country Label"
)
missing_cols <- setdiff(required_cols, names(air_raw))
if (length(missing_cols) > 0) {
  stop(paste0(
    "Missing required columns: ", paste(missing_cols, collapse = ", "),
    "
Available columns: ", paste(names(air_raw), collapse = ", ")
  ))
}

air_basic <- air_raw %>%
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
    country_code = str_trim(country_code),
    city = str_trim(city),
    location = str_trim(location),
    coordinates = str_trim(coordinates),
    pollutant = str_trim(pollutant),
    source_name = str_trim(source_name),
    unit = str_trim(unit),
    last_updated = str_trim(last_updated),
    country = str_trim(country),
    value = parse_number(value)
  )

# ---- 3. Clean to daily means ----
# Keep the same six-country focus used in the app.
target_countries <- c("Ghana", "Mongolia", "India", "Iceland", "Switzerland", "Finland")

clean_daily <- air_basic %>%
  mutate(
    datetime = suppressWarnings(parse_date_time(
      last_updated,
      orders = c("ymd HMS", "ymd HM", "ymd", "dmy HMS", "dmy HM"),
      tz = "UTC"
    )),
    date = as.Date(datetime)
  ) %>%
  filter(
    country %in% target_countries,
    !is.na(date),
    !is.na(pollutant),
    !is.na(value),
    value >= 0
  ) %>%
  group_by(country, date, pollutant) %>%
  summarise(daily_mean = mean(value, na.rm = TRUE), .groups = "drop")

country_levels <- target_countries

pollutant_levels_daily <- clean_daily %>%
  filter(pollutant != "PM2.5") %>%
  distinct(pollutant) %>%
  arrange(pollutant) %>%
  pull(pollutant)

# =========================================================
# Tab 1: PM2.5 daily correlation heatmap by country
# =========================================================
make_daily_pair_data <- function(dat, country_name, pollutant_name) {
  wide <- dat %>%
    filter(country == country_name) %>%
    select(date, pollutant, daily_mean) %>%
    pivot_wider(names_from = pollutant, values_from = daily_mean)
  
  if (!all(c("PM2.5", pollutant_name) %in% names(wide))) {
    return(data.frame())
  }
  
  wide %>%
    select(date, PM2.5, all_of(pollutant_name)) %>%
    rename(other = all_of(pollutant_name)) %>%
    filter(complete.cases(PM2.5, other))
}

cor_rows_daily <- lapply(country_levels, function(cty) {
  wide <- clean_daily %>%
    filter(country == cty) %>%
    select(date, pollutant, daily_mean) %>%
    pivot_wider(names_from = pollutant, values_from = daily_mean)
  
  if (!"PM2.5" %in% names(wide)) {
    return(data.frame(
      country = rep(cty, length(pollutant_levels_daily)),
      pollutant = pollutant_levels_daily,
      cor = NA_real_,
      n = NA_integer_,
      stringsAsFactors = FALSE
    ))
  }
  
  rows <- lapply(pollutant_levels_daily, function(p) {
    if (!p %in% names(wide)) {
      return(data.frame(
        country = cty,
        pollutant = p,
        cor = NA_real_,
        n = NA_integer_,
        stringsAsFactors = FALSE
      ))
    }
    
    x <- wide[["PM2.5"]]
    y <- wide[[p]]
    ok <- complete.cases(x, y)
    n <- sum(ok)
    cor_val <- if (n >= 2) cor(x[ok], y[ok], method = "pearson") else NA_real_
    
    data.frame(
      country = cty,
      pollutant = p,
      cor = cor_val,
      n = n,
      stringsAsFactors = FALSE
    )
  })
  
  bind_rows(rows)
})

cor_grid_daily <- bind_rows(cor_rows_daily) %>%
  mutate(
    country = factor(country, levels = country_levels),
    pollutant = factor(pollutant, levels = pollutant_levels_daily)
  )

# matrix for plotly heatmap
make_matrix <- function(df, x_levels, y_levels, value_col) {
  z <- matrix(NA_real_, nrow = length(y_levels), ncol = length(x_levels),
              dimnames = list(y_levels, x_levels))
  for (i in seq_len(nrow(df))) {
    x <- as.character(df[[x_levels[1]]][i])
  }
  z
}

daily_z <- matrix(
  NA_real_,
  nrow = length(pollutant_levels_daily),
  ncol = length(country_levels),
  dimnames = list(pollutant_levels_daily, country_levels)
)

for (i in seq_len(nrow(cor_grid_daily))) {
  r <- as.character(cor_grid_daily$pollutant[i])
  c <- as.character(cor_grid_daily$country[i])
  daily_z[r, c] <- cor_grid_daily$cor[i]
}

# =========================================================
# Tab 2: Cross-country pollutant mean correlations (upper triangle)
# =========================================================
country_mean <- clean_daily %>%
  group_by(country, pollutant) %>%
  summarise(country_mean = mean(daily_mean, na.rm = TRUE), .groups = "drop")

country_mean_wide <- country_mean %>%
  filter(country %in% country_levels) %>%
  pivot_wider(names_from = pollutant, values_from = country_mean)

mean_pollutant_levels <- setdiff(names(country_mean_wide), "country")
mean_pollutant_levels <- mean_pollutant_levels[
  colSums(!is.na(country_mean_wide[mean_pollutant_levels])) >= 3
]

if (length(mean_pollutant_levels) < 2) {
  stop("Not enough pollutants with country-level coverage to build the second heatmap.")
}

country_mean_wide <- country_mean_wide %>% select(country, all_of(mean_pollutant_levels))
mean_cor_mat <- cor(country_mean_wide %>% select(-country), use = "pairwise.complete.obs", method = "pearson")

mean_z <- mean_cor_mat
mean_z[lower.tri(mean_z, diag = TRUE)] <- NA_real_

# =========================================================
# Helper for clicking on plotly heatmaps
# =========================================================
decode_axis_value <- function(value, levels_vec) {
  if (length(value) == 0 || is.null(value) || all(is.na(value))) return(NA_character_)
  v <- as.character(value)[1]
  if (grepl("^[0-9]+$", v)) {
    idx <- as.integer(v)
    if (!is.na(idx) && idx >= 1 && idx <= length(levels_vec)) {
      return(as.character(levels_vec[idx]))
    }
  }
  v
}

# =========================================================
# UI
# =========================================================
ui <- fluidPage(
  tags$head(tags$style(HTML("
    .main-container { max-width: 1800px !important; }
    .plotly, .shiny-plot-output { width: 100% !important; }
  "))),
  titlePanel("Unified PM2.5 Heatmaps from One Standardized Dataset"),
  sidebarLayout(
    sidebarPanel(
      h4("Unified cleaning pipeline"),
      p("Semicolon-delimited CSV -> all columns as character -> standardized names -> trimmed strings -> numeric parsing -> daily means."),
      p("All heatmaps below are derived from the same cleaned daily dataframe."),
      tags$hr(),
      strong("Countries kept in the app:"),
      p(paste(country_levels, collapse = ", "))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Daily PM2.5 correlations",
          br(),
          plotlyOutput("daily_heatmap", height = "780px"),
          verbatimTextOutput("daily_selected"),
          plotOutput("daily_reg_plot", height = "520px")
        ),
        tabPanel(
          "Cross-country mean correlations",
          br(),
          plotlyOutput("mean_heatmap", height = "780px"),
          verbatimTextOutput("mean_selected"),
          plotOutput("mean_reg_plot", height = "520px")
        )
      )
    )
  )
)

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session) {
  selected_daily <- reactiveVal(NULL)
  selected_mean <- reactiveVal(NULL)
  
  observeEvent(event_data("plotly_click", source = "daily_heatmap"), {
    click <- event_data("plotly_click", source = "daily_heatmap")
    req(click$x, click$y)
    selected_daily(list(
      country = decode_axis_value(click$x, country_levels),
      pollutant = decode_axis_value(click$y, pollutant_levels_daily)
    ))
  })
  
  observeEvent(event_data("plotly_click", source = "mean_heatmap"), {
    click <- event_data("plotly_click", source = "mean_heatmap")
    req(click$x, click$y)
    selected_mean(list(
      x = decode_axis_value(click$x, mean_pollutant_levels),
      y = decode_axis_value(click$y, mean_pollutant_levels)
    ))
  })
  
  output$daily_heatmap <- renderPlotly({
    plot_ly(
      x = country_levels,
      y = pollutant_levels_daily,
      z = daily_z,
      type = "heatmap",
      source = "daily_heatmap",
      colors = c("#4575b4", "#f7f7f7", "#d73027"),
      zmin = -1,
      zmax = 1,
      hovertemplate = paste0(
        "Country: %{x}<br>",
        "Pollutant: %{y}<br>",
        "Correlation: %{z:.2f}<extra></extra>"
      )
    ) %>%
      layout(
        title = "PM2.5 vs Other Pollutants: Daily Mean Correlations by Country",
        xaxis = list(title = "Country", tickangle = 35),
        yaxis = list(title = "Pollutant"),
        margin = list(l = 120, r = 40, b = 120, t = 80)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$daily_selected <- renderText({
    sel <- selected_daily()
    if (is.null(sel)) {
      return("Click one tile in the daily heatmap to generate the regression plot.")
    }
    paste0("Selected: ", sel$country, " / ", sel$pollutant)
  })
  
  output$daily_reg_plot <- renderPlot({
    sel <- selected_daily()
    req(!is.null(sel), !is.na(sel$country), !is.na(sel$pollutant))
    
    dat <- make_daily_pair_data(clean_daily, sel$country, sel$pollutant)
    req(nrow(dat) >= 3)
    
    fit <- lm(other ~ PM2.5, data = dat)
    r2 <- summary(fit)$r.squared
    eq <- paste0("y = ", round(coef(fit)[1], 3), " + ", round(coef(fit)[2], 3), "x")
    
    ggplot(dat, aes(x = PM2.5, y = other)) +
      geom_point(alpha = 0.75) +
      geom_smooth(method = "lm", se = TRUE) +
      labs(
        x = "Daily mean PM2.5",
        y = paste0("Daily mean ", sel$pollutant),
        title = paste0(sel$country, ": PM2.5 vs ", sel$pollutant, " Regression"),
        subtitle = paste0(eq, "   |   R² = ", round(r2, 3), "   |   n = ", nrow(dat))
      ) +
      theme_minimal(base_size = 14)
  })
  
  output$mean_heatmap <- renderPlotly({
    plot_ly(
      x = mean_pollutant_levels,
      y = mean_pollutant_levels,
      z = mean_z,
      type = "heatmap",
      source = "mean_heatmap",
      colors = c("#4575b4", "#f7f7f7", "#d73027"),
      zmin = -1,
      zmax = 1,
      hovertemplate = paste0(
        "X pollutant: %{x}<br>",
        "Y pollutant: %{y}<br>",
        "Correlation: %{z:.2f}<extra></extra>"
      )
    ) %>%
      layout(
        title = "Upper-triangle Correlations of Country Means Across Pollutants",
        xaxis = list(title = "Pollutant", tickangle = 35),
        yaxis = list(title = "Pollutant", autorange = "reversed"),
        margin = list(l = 120, r = 40, b = 120, t = 80)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$mean_selected <- renderText({
    sel <- selected_mean()
    if (is.null(sel)) {
      return("Click one colored cell in the upper-triangle heatmap to generate the regression plot.")
    }
    paste0("Selected: ", sel$x, " / ", sel$y)
  })
  
  output$mean_reg_plot <- renderPlot({
    sel <- selected_mean()
    req(!is.null(sel), !is.na(sel$x), !is.na(sel$y))
    
    dat <- country_mean_wide %>%
      select(country, all_of(sel$x), all_of(sel$y)) %>%
      rename(x = all_of(sel$x), y = all_of(sel$y)) %>%
      filter(complete.cases(x, y))
    
    req(nrow(dat) >= 3)
    
    fit <- lm(y ~ x, data = dat)
    r2 <- summary(fit)$r.squared
    eq <- paste0("y = ", round(coef(fit)[1], 3), " + ", round(coef(fit)[2], 3), "x")
    
    ggplot(dat, aes(x = x, y = y)) +
      geom_point(alpha = 0.8) +
      geom_smooth(method = "lm", se = TRUE) +
      geom_text(aes(label = country), vjust = -0.6, size = 4) +
      labs(
        x = paste0("Country mean of ", sel$x),
        y = paste0("Country mean of ", sel$y),
        title = paste0(sel$x, " vs ", sel$y, " Across Countries"),
        subtitle = paste0(eq, "   |   R² = ", round(r2, 3), "   |   n = ", nrow(dat))
      ) +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui, server)
