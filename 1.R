# ============================================================
# World Air Quality Shiny App
# Pages:
# 1. Pollutant Correlation Heatmap
# 2. Country PM2.5 Profile
# 3. Global PM2.5 Map
# ============================================================

library(shiny)
library(tidyverse)
library(plotly)
library(leaflet)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

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

# =========================
# 4. PM2.5 country cleaning
# =========================
pm25_country_before <- air_all %>%
  filter(pollutant == "PM2.5") %>%
  group_by(country) %>%
  summarise(
    n_obs_pm25 = n(),
    mean_pm25 = mean(value, na.rm = TRUE),
    min_pm25 = min(value, na.rm = TRUE),
    max_pm25 = max(value, na.rm = TRUE),
    .groups = "drop"
  )

removed_countries <- pm25_country_before %>%
  mutate(
    reason = case_when(
      n_obs_pm25 < 3 ~ "Less than 3 PM2.5 observations",
      min_pm25 == 0 & max_pm25 == 0 ~ "All PM2.5 values are 0",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(reason))

air_all_clean <- air_all %>%
  filter(!country %in% removed_countries$country)

pm25_clean <- air_all_clean %>%
  filter(pollutant == "PM2.5")

# =========================
# 5. Top 3 and bottom 3 PM2.5 countries
# =========================
pm25_country_after <- pm25_clean %>%
  group_by(country) %>%
  summarise(
    n_obs_pm25 = n(),
    mean_pm25 = mean(value, na.rm = TRUE),
    unit = first(unit),
    .groups = "drop"
  )

top3_pm25_after <- pm25_country_after %>%
  arrange(desc(mean_pm25)) %>%
  slice_head(n = 3)

bottom3_pm25_after <- pm25_country_after %>%
  arrange(mean_pm25) %>%
  slice_head(n = 3)

selected_countries <- c(
  top3_pm25_after$country,
  bottom3_pm25_after$country
)

pm25_unit <- pm25_country_after %>%
  filter(!is.na(unit), unit != "") %>%
  pull(unit) %>%
  first()

if (is.na(pm25_unit) || length(pm25_unit) == 0) {
  pm25_unit <- "µg/m³"
}

# =========================
# 6. Country-level average table
# Used for Page 1 overall pollutant correlation
# =========================
country_avg_long <- air_all_clean %>%
  group_by(country, pollutant) %>%
  summarise(
    avg_value = mean(value, na.rm = TRUE),
    unit = first(na.omit(unit)),
    .groups = "drop"
  )

country_avg <- country_avg_long %>%
  select(country, pollutant, avg_value) %>%
  pivot_wider(
    names_from = pollutant,
    values_from = avg_value
  ) %>%
  select(country, where(~ sum(!is.na(.)) >= 5))

pollutants <- colnames(country_avg)[-1]
pollutant_levels <- pollutants
y_pollutant_levels <- rev(pollutant_levels)

# =========================
# 7. Page 1 pollutant correlation heatmap data
# =========================
cor_mat <- cor(
  country_avg[, pollutants],
  use = "pairwise.complete.obs",
  method = "pearson"
)

cor_long <- as.data.frame(as.table(cor_mat)) %>%
  rename(
    x = Var1,
    y = Var2,
    cor = Freq
  ) %>%
  mutate(
    x_name = as.character(x),
    y_name = as.character(y),
    x = factor(x_name, levels = pollutant_levels),
    y = factor(y_name, levels = y_pollutant_levels),
    diagonal = x_name == y_name,
    label = ifelse(diagonal, "", sprintf("%.2f", cor)),
    click_key = paste(x_name, y_name, sep = "::"),
    hover_text = paste0(
      "X pollutant: ", x_name,
      "<br>Y pollutant: ", y_name,
      "<br>Correlation: ", round(cor, 2),
      ifelse(
        diagonal,
        "<br><b>Self-correlation: disabled</b>",
        "<br>Click to view regression plot"
      )
    )
  )

# =========================
# 8. Helper function for Page 2
# Within-country paired PM2.5 and selected pollutant data
# =========================
make_country_pair_data <- function(target_country, target_pollutant) {
  
  internal_wide <- air_all_clean %>%
    filter(country == target_country) %>%
    group_by(country, city, location, pollutant) %>%
    summarise(
      mean_value = mean(value, na.rm = TRUE),
      unit = first(na.omit(unit)),
      .groups = "drop"
    ) %>%
    select(country, city, location, pollutant, mean_value) %>%
    pivot_wider(
      names_from = pollutant,
      values_from = mean_value
    )
  
  if (!("PM2.5" %in% colnames(internal_wide))) {
    return(tibble())
  }
  
  if (!(target_pollutant %in% colnames(internal_wide))) {
    return(tibble())
  }
  
  internal_wide %>%
    select(country, city, location, `PM2.5`, all_of(target_pollutant)) %>%
    rename(
      pm25 = `PM2.5`,
      selected_value = all_of(target_pollutant)
    ) %>%
    filter(complete.cases(pm25, selected_value))
}

pollutant_unit_lookup <- air_all_clean %>%
  select(pollutant, unit) %>%
  filter(!is.na(unit), unit != "") %>%
  group_by(pollutant) %>%
  summarise(unit = first(unit), .groups = "drop")

# =========================
# 9. Page 2 country-pollutant correlation heatmap data
# Each tile = within-country correlation between PM2.5 and another pollutant
# Blank cell = not enough paired observations
# =========================
country_pollutant_heatmap <- expand_grid(
  country = selected_countries,
  pollutant = pollutant_levels
) %>%
  filter(pollutant != "PM2.5") %>%
  mutate(
    pair_data = map2(country, pollutant, make_country_pair_data),
    n_pairs = map_int(pair_data, nrow),
    correlation = map2_dbl(pair_data, n_pairs, function(df, n) {
      if (n >= 3 && sd(df$pm25, na.rm = TRUE) > 0 && sd(df$selected_value, na.rm = TRUE) > 0) {
        cor(df$pm25, df$selected_value, use = "complete.obs")
      } else {
        NA_real_
      }
    })
  ) %>%
  filter(!is.na(correlation), n_pairs >= 3) %>%
  left_join(
    pollutant_unit_lookup,
    by = "pollutant"
  ) %>%
  mutate(
    country_name = as.character(country),
    pollutant_name = as.character(pollutant),
    country = factor(country_name, levels = selected_countries),
    pollutant = factor(pollutant_name, levels = y_pollutant_levels),
    click_key = paste(country_name, pollutant_name, sep = "::"),
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
# 10. World map data
# =========================
world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(
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
      TRUE ~ name
    )
  )

create_pal_function <- function(data_range) {
  colorNumeric(
    palette = c("#2E7D32", "#F9E076", "#D84315", "#B71C1C", "#3E2723"),
    domain = data_range,
    na.color = "transparent"
  )
}

# =========================
# 11. Decode Page 1 click positions
# =========================
decode_x_pollutant <- function(v) {
  v <- as.character(v)
  
  if (grepl("^[0-9]+$", v)) {
    idx <- as.integer(v)
    if (!is.na(idx) && idx >= 1 && idx <= length(pollutant_levels)) {
      return(pollutant_levels[idx])
    }
  }
  
  return(v)
}

decode_y_pollutant <- function(v) {
  v <- as.character(v)
  
  if (grepl("^[0-9]+$", v)) {
    idx <- as.integer(v)
    if (!is.na(idx) && idx >= 1 && idx <= length(y_pollutant_levels)) {
      return(y_pollutant_levels[idx])
    }
  }
  
  return(v)
}

# =========================
# 12. UI
# =========================
ui <- fluidPage(
  
  tags$head(
    tags$style(
      HTML("
        body {
          background-color: #f8f9fa;
          font-family: 'Segoe UI', Tahoma, sans-serif;
        }
        .container-fluid {
          max-width: 1900px;
        }
        .well {
          background-color: #f7f7f7;
        }
      ")
    )
  ),
  
  titlePanel("Pollutant Correlation Heatmap After Data Cleaning"),
  
  tabsetPanel(
    
    # =========================
    # Page 1
    # =========================
    tabPanel(
      "Pollutant Correlation",
      
      fluidRow(
        column(
          width = 3,
          wellPanel(
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
            
            strong("PM2.5 selected countries:"),
            p(paste(selected_countries, collapse = ", ")),
            tags$hr(),
            
            strong("Key countries shown in regression labels:"),
            p("Only countries with extreme pollutant values are labelled directly."),
            p("Other countries can still be viewed by hovering over the points."),
            tags$hr(),
            
            p("Note: Diagonal cells are disabled because self-correlation is always 1.")
          )
        ),
        
        column(
          width = 9,
          plotlyOutput("heatmap", height = "900px")
        )
      ),
      
      fluidRow(
        column(
          width = 12,
          verbatimTextOutput("info"),
          plotlyOutput("reg_plot", height = "750px")
        )
      )
    ),
    
    # =========================
    # Page 2
    # =========================
    tabPanel(
      "Country PM2.5 Profile",
      
      fluidRow(
        column(
          width = 3,
          wellPanel(
            h4("Country Profile"),
            p("This page shows within-country correlations between PM2.5 and other pollutants."),
            p("The x-axis shows the top 3 and bottom 3 PM2.5 countries after cleaning."),
            p("The color represents the correlation between PM2.5 and each pollutant inside the selected country."),
            p("Blank cells mean that there are not enough paired observations for a reliable correlation."),
            tags$hr(),
            
            strong("Countries included:"),
            p(paste(selected_countries, collapse = ", ")),
            tags$hr(),
            
            strong("PM2.5 unit:"),
            p(pm25_unit),
            tags$hr(),
            
            p("Hover over each tile to see country, pollutant, correlation, paired observations, and unit."),
            p("Click a tile to analyze the relationship between PM2.5 and the selected pollutant within that country.")
          )
        ),
        
        column(
          width = 9,
          plotlyOutput("country_pollutant_heatmap", height = "800px")
        )
      ),
      
      fluidRow(
        column(
          width = 12,
          verbatimTextOutput("country_profile_info"),
          plotlyOutput("country_profile_reg_plot", height = "720px")
        )
      )
    ),
    
    # =========================
    # Page 3
    # =========================
    # --- Global PM2.5 Map Page ---
    tabPanel(
      "Global PM2.5 Map",
      # Add custom CSS for spacing and button appearance
      tags$head(
        tags$style(HTML("
          .control-group { margin-bottom: 25px; } /* Space between sections */
          .btn-jump { margin: 4px; padding: 5px 10px; border-radius: 4px; }
          .view-all-btn { margin-bottom: 15px; font-weight: bold; }
          hr { margin-top: 20px; margin-bottom: 20px; }
        "))
      ),
      br(),
      fluidRow(
        column(width = 3, wellPanel(
          actionButton("btn_view_world", "Reset to World View", 
                       class = "view-all-btn",
                       style="width: 100%; margin-bottom: 20px; background-color: #2c3e50; color: white; border: none;"),
          
          # Section 1: Dropdowns
          div(class = "control-group",
              selectInput("sort_order", "1. Sort Countries By:", 
                          choices = c("Alphabetical (A-Z)" = "alphabet", 
                                      "PM2.5 High to Low" = "high_low", 
                                      "PM2.5 Low to High" = "low_high")),
              selectInput("selected_country", "2. Focus on Country:", 
                          choices = NULL) 
          ),
          
          tags$hr(),
          
          # Section 2: Quick Jump & Preview
          div(class = "control-group",
              h4("Quick Jump & Batch View"),
              
              # Global extremes button
              actionButton("view_all_6", "View All 6 Extremes", 
                           class = "view-all-btn",
                           style="width: 100%; background-color: #34495e; color: white;"),
              
              br(),
              
              # Highest section
              strong("Highest PM2.5 Group:"),
              actionButton("view_top3", "Preview Group", 
                           style="padding: 2px 8px; font-size: 0.7em; margin-left: 10px; background-color: #f8d7da; border: 1px solid #f5c6cb;"),
              div(style="margin-top: 8px;", uiOutput("top3_buttons")),
              
              br(), # Space between rows
              
              # Lowest section
              strong("Lowest PM2.5 Group:"),
              actionButton("view_bot3", "Preview Group", 
                           style="padding: 2px 8px; font-size: 0.7em; margin-left: 10px; background-color: #d4edda; border: 1px solid #c3e6cb;"),
              div(style="margin-top: 8px;", uiOutput("bot3_buttons"))
          ),
          
          tags$hr(),
          strong("Legend Info"),
          p(paste0("Average PM2.5 (", pm25_unit, ")"))
        )),
        
        column(width = 9, 
               leafletOutput("main_map", height = "730px"))
      )
    )
  )
)


# =========================
# 13. Server
# =========================
server <- function(input, output, session) {
  
  selected <- reactiveVal(NULL)
  selected_country_profile <- reactiveVal(NULL)
  
  # =========================
  # Page 1 click event
  # =========================
  observeEvent(event_data("plotly_click", source = "heatmap"), {
    
    click <- event_data("plotly_click", source = "heatmap")
    
    if (is.null(click) || is.null(click$x) || is.null(click$y)) {
      return()
    }
    
    x_var <- decode_x_pollutant(click$x)
    y_var <- decode_y_pollutant(click$y)
    
    if (is.na(x_var) || is.na(y_var)) {
      return()
    }
    
    if (x_var == y_var) {
      selected(NULL)
      return()
    }
    
    selected(list(x = x_var, y = y_var))
  })
  
  # =========================
  # Page 2 click event
  # =========================
  observeEvent(event_data("plotly_click", source = "country_heatmap"), {
    
    click <- event_data("plotly_click", source = "country_heatmap")
    
    if (is.null(click) || is.null(click$key) || is.na(click$key)) {
      return()
    }
    
    parts <- strsplit(as.character(click$key), "::", fixed = TRUE)[[1]]
    
    if (length(parts) < 2) {
      return()
    }
    
    clicked_country <- parts[1]
    clicked_pollutant <- parts[2]
    
    if (is.na(clicked_country) || is.na(clicked_pollutant)) {
      return()
    }
    
    if (clicked_pollutant == "PM2.5") {
      selected_country_profile(NULL)
      return()
    }
    
    selected_country_profile(
      list(
        country = clicked_country,
        pollutant = clicked_pollutant
      )
    )
  })
  
  # =========================
  # Page 1 heatmap
  # =========================
  output$heatmap <- renderPlotly({
    
    p <- ggplot(
      cor_long,
      aes(
        x = x,
        y = y,
        fill = cor,
        text = hover_text,
        key = click_key
      )
    ) +
      geom_tile(
        aes(alpha = ifelse(diagonal, 0.25, 1)),
        color = "white"
      ) +
      geom_text(
        aes(label = label),
        size = 4
      ) +
      scale_alpha_identity() +
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
    
    ggplotly(p, tooltip = "text", source = "heatmap") %>%
      event_register("plotly_click") %>%
      layout(
        margin = list(l = 130, r = 80, b = 160, t = 80)
      )
  })
  
  # =========================
  # Page 2 heatmap
  # =========================
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
      ) +
      geom_text(
        aes(label = sprintf("%.2f", correlation)),
        size = 3.8
      ) +
      scale_fill_gradient2(
        low = "#4575b4",
        mid = "white",
        high = "#d73027",
        midpoint = 0,
        limits = c(-1, 1),
        name = "Correlation",
        na.value = "white"
      ) +
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
    
    ggplotly(p, tooltip = "text", source = "country_heatmap") %>%
      event_register("plotly_click") %>%
      layout(
        margin = list(l = 140, r = 80, b = 110, t = 100)
      )
  })
  
  # =========================
  # Page 1 selected info
  # =========================
  output$info <- renderText({
    
    sel <- selected()
    
    if (is.null(sel)) {
      return("Click a non-diagonal cell in the heatmap to generate the regression plot.")
    }
    
    paste0("Selected: ", sel$x, " vs ", sel$y)
  })
  
  # =========================
  # Page 1 regression plot
  # Overall cross-country regression
  # =========================
  output$reg_plot <- renderPlotly({
    
    sel <- selected()
    req(sel)
    
    x_var <- sel$x
    y_var <- sel$y
    
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
    
    req(nrow(dat) >= 3)
    
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
    
    fit <- lm(y ~ x, data = dat)
    
    x_seq <- seq(min(dat$x), max(dat$x), length.out = 100)
    pred_data <- data.frame(x = x_seq)
    
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
    
    subtitle_text <- paste0(
      "y = ",
      round(coef(fit)[1], 2),
      " + ",
      round(coef(fit)[2], 2),
      "x"
    )
    
    p <- plot_ly()
    
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
  # Page 2 selected info
  # =========================
  output$country_profile_info <- renderText({
    
    sel <- selected_country_profile()
    
    if (is.null(sel)) {
      return("Click a tile to analyze the within-country relationship between PM2.5 and the selected pollutant.")
    }
    
    paste0(
      "Selected country: ", sel$country,
      " | Selected pollutant: ", sel$pollutant,
      " | Regression: PM2.5 vs ", sel$pollutant,
      " within ", sel$country
    )
  })
  
  # =========================
  # Page 2 regression plot
  # Within-country regression
  # =========================
  output$country_profile_reg_plot <- renderPlotly({
    
    sel <- selected_country_profile()
    req(sel)
    
    clicked_country <- sel$country
    y_var <- sel$pollutant
    
    country_internal <- make_country_pair_data(clicked_country, y_var)
    
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
    
    fit <- tryCatch(
      lm(selected_value ~ pm25, data = dat),
      error = function(e) NULL
    )
    
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
    
    x_seq <- seq(min(dat$pm25), max(dat$pm25), length.out = 100)
    pred_data <- data.frame(pm25 = x_seq)
    
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
    
    subtitle_text <- paste0(
      "Within ", clicked_country,
      ": y = ",
      round(coef(fit)[1], 2),
      " + ",
      round(coef(fit)[2], 2),
      "x"
    )
    
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
    
    p <- plot_ly()
    
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
  # Page 3 map data: global range
  # =========================
  
  map_view_mode <- reactiveVal("all")
  
  global_range <- reactive({
    all_avgs <- pm25_clean %>%
      group_by(country) %>%
      summarise(avg_val = mean(value, na.rm = TRUE), .groups = "drop") %>%
      pull(avg_val)
    
    range(all_avgs, na.rm = TRUE)
  })
  
  # =========================
  # Page 3 map ranking data
  # =========================
  extreme_data <- reactive({
    pm25_clean %>%
      group_by(country) %>%
      summarise(
        avg_val = mean(value, na.rm = TRUE),
        n_obs = n(),
        .groups = "drop"
      )
  })
  
  # ============================================================
  # Page 3 handle view world button (FIXED BUG 1)
  # ============================================================
  observeEvent(input$btn_view_world, {
    # 1. Update mode
    map_view_mode("all")
    
    # 2. Reset selection to "none" (triggers Step B/E)
    updateSelectInput(session, "selected_country", selected = "none")
    
    # 3. Fly back
    leafletProxy("main_map") %>% 
      flyTo(lng = 0, lat = 20, zoom = 2, options = list(duration = 1))
  })
  
  # --- Step A: Generate Quick Jump Buttons (Individual) ---
  output$top3_buttons <- renderUI({
    top3 <- pm25_country_after %>% arrange(desc(mean_pm25)) %>% slice_head(n = 3)
    tagList(lapply(top3$country, function(cty) {
      actionButton(paste0("btn_", cty), cty, 
                   style="margin: 2px; padding: 2px 6px; font-size: 0.8em; background-color: #fdecea;")
    }))
  })
  
  output$bot3_buttons <- renderUI({
    bot3 <- pm25_country_after %>% arrange(mean_pm25) %>% slice_head(n = 3)
    tagList(lapply(bot3$country, function(cty) {
      actionButton(paste0("btn_", cty), cty, 
                   style="margin: 2px; padding: 2px 6px; font-size: 0.8em; background-color: #e8f5e9;")
    }))
  })
  
  # --- Step B: Handle Sort Order & Dropdown Update ---
  observeEvent(input$sort_order, {
    sorted_data <- pm25_country_after
    if (input$sort_order == "alphabet") {
      sorted_list <- sort(sorted_data$country)
    } else if (input$sort_order == "high_low") {
      sorted_list <- sorted_data %>% arrange(desc(mean_pm25)) %>% pull(country)
    } else {
      sorted_list <- sorted_data %>% arrange(mean_pm25) %>% pull(country)
    }
    
    # Prepend "none" to choices
    updateSelectInput(session, "selected_country", choices = c("View World" = "none", sorted_list))
  })
  
  # --- Step C: Individual Button Clicks (FIXED BUG 2) ---
  # Iterate through selected countries to create dynamic observers
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
  
  # --- Step D: Batch Preview Logic ---
  
  # Helper function to fly to multiple countries
  fly_to_multiple <- function(country_names, duration = 1.2) {
    target_shapes <- world_sf %>% filter(name %in% country_names)
    if (nrow(target_shapes) > 0) {
      bbox <- st_bbox(target_shapes)
      leafletProxy("main_map") %>% flyToBounds(
        as.numeric(bbox[["xmin"]]), as.numeric(bbox[["ymin"]]),
        as.numeric(bbox[["xmax"]]), as.numeric(bbox[["ymax"]]),
        options = list(duration = duration, padding = c(50, 50))
      )
    }
  }
  
  observeEvent(input$view_all_6, {
    map_view_mode("all6")
    updateSelectInput(session, "selected_country", selected = "none")
    all_6 <- c(pm25_country_after %>% arrange(desc(mean_pm25)) %>% slice_head(n = 3) %>% pull(country),
               pm25_country_after %>% arrange(mean_pm25) %>% slice_head(n = 3) %>% pull(country))
    fly_to_multiple(all_6, 1.2)
  })
  
  observeEvent(input$view_top3, {
    map_view_mode("top3")
    updateSelectInput(session, "selected_country", selected = "none")
    top3 <- pm25_country_after %>% arrange(desc(mean_pm25)) %>% slice_head(n = 3) %>% pull(country)
    fly_to_multiple(top3, 1.0)
  })
  
  observeEvent(input$view_bot3, {
    map_view_mode("bot3")
    updateSelectInput(session, "selected_country", selected = "none")
    bot3 <- pm25_country_after %>% arrange(mean_pm25) %>% slice_head(n = 3) %>% pull(country)
    fly_to_multiple(bot3, 1.0)
  })
  
  # --- Step E: Handle Dropdown Selection ---
  observeEvent(input$selected_country, {
    req(input$selected_country)
    if (input$selected_country == "none") {
      # If user explicitly picks 'View World' from dropdown, and we are not in batch mode
      if (!(map_view_mode() %in% c("top3", "bot3", "all6"))) {
        map_view_mode("all")
      }
    } else {
      map_view_mode("single")
    }
  }, ignoreInit = TRUE)
  
  
  # =========================
  # Page 3 initial map
  # =========================
  output$main_map <- renderLeaflet({
    initial_data <- pm25_clean %>%
      group_by(country) %>%
      summarise(avg_val = mean(value, na.rm = TRUE), .groups = "drop")
    
    data_range <- range(initial_data$avg_val, na.rm = TRUE)
    pal <- create_pal_function(data_range)
    
    map_data <- world_sf %>%
      inner_join(initial_data, by = c("name" = "country"))
    
    leaflet(map_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addPolygons(
        fillColor = ~pal(avg_val),
        fillOpacity = 0.58,
        weight = 0.4,
        color = "#FFFFFF",
        opacity = 1,
        label = ~paste0(name, ": ", round(avg_val, 2), " ", pm25_unit),
        highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.85)
      ) %>%
      addLegend(
        pal = pal,
        values = ~avg_val,
        title = paste0("PM2.5 Average<br>(", pm25_unit, ")"),
        position = "bottomright",
        layerId = "mapLegend"
      )
  })
  
  
  # =========================
  # Page 3 update map (REFINED ZOOM LOGIC)
  # =========================
  observe({
    req(extreme_data(), global_range())
    
    current_data <- extreme_data()
    data_range <- global_range()
    pal <- create_pal_function(data_range)
    proxy <- leafletProxy("main_map")
    
    top3_names <- pm25_country_after %>% arrange(desc(mean_pm25)) %>% slice_head(n = 3) %>% pull(country)
    bot3_names <- pm25_country_after %>% arrange(mean_pm25) %>% slice_head(n = 3) %>% pull(country)
    all_extreme_names <- c(top3_names, bot3_names)
    
    # Filtering display data
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
    
    map_data <- world_sf %>%
      inner_join(display_data, by = c("name" = "country"))
    
    if (nrow(map_data) == 0) {
      proxy %>% clearShapes()
      return()
    }
    
    # Update Polygons
    proxy %>%
      clearShapes() %>%
      addPolygons(
        data = map_data,
        fillColor = ~pal(avg_val),
        fillOpacity = ifelse(map_data$name == input$selected_country | 
                               (map_view_mode() != "all" & map_data$name %in% all_extreme_names), 
                             0.85, 0.58),
        weight = ifelse(map_data$name == input$selected_country, 2.5, 
                        ifelse(map_data$name %in% all_extreme_names, 1.2, 0.4)),
        color = ifelse(map_data$name == input$selected_country, "#000000",
                       ifelse(map_data$name %in% all_extreme_names, "#666666", "#FFFFFF")),
        opacity = 1,
        label = ~paste0(name, ": ", round(avg_val, 2), " ", pm25_unit),
        highlightOptions = highlightOptions(weight = 3, color = "#222", fillOpacity = 0.9)
      )
    
    # --- FIXED ZOOM LOGIC ---
    # Only auto-zoom if we are in 'single' mode. 
    # Batch modes are zoomed by their own buttons. 
    # 'all' mode is zoomed by the Reset button.
    if (map_view_mode() == "single") {
      req(input$selected_country)
      if (input$selected_country != "none") {
        target_box <- map_data %>% filter(name == input$selected_country)
        if (nrow(target_box) > 0) {
          bbox <- st_bbox(target_box)
          proxy %>% flyToBounds(
            lng1 = as.numeric(bbox[["xmin"]]), lat1 = as.numeric(bbox[["ymin"]]),
            lng2 = as.numeric(bbox[["xmax"]]), lat2 = as.numeric(bbox[["ymax"]]),
            options = list(duration = 0.75, padding = c(40, 40))
          )
        }
      }
    }
  })
}

shinyApp(ui, server)