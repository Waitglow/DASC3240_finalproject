# ============================================================
# 0. Load Required Packages
# ============================================================

library(shiny)
library(tidyverse)
library(plotly)
library(leaflet)
library(scales)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


# ============================================================
# 1. Read Dataset
# ============================================================

data_path <- if (file.exists("data/world_air_quality.csv")) {
  "data/world_air_quality.csv"
} else {
  "../data/world_air_quality.csv"
}

air_raw <- readr::read_delim(
  data_path,
  delim = ";",
  show_col_types = FALSE
)


# ============================================================
# 2. Clean Dataset
# ============================================================

air_clean <- air_raw %>%
  rename(
    country = `Country Label`,
    country_code = `Country Code`
  ) %>%
  separate(
    Coordinates,
    into = c("lat", "lon"),
    sep = ",",
    convert = TRUE
  ) %>%
  mutate(
    Value = as.numeric(Value)
  ) %>%
  filter(
    !is.na(country),
    !is.na(Pollutant),
    !is.na(Value),
    !is.na(lat),
    !is.na(lon),
    Value >= 0
  )


# ============================================================
# 3. Define Pollutants
# ============================================================

pollutants_all <- c("PM2.5", "PM10", "NO2", "O3", "SO2", "CO")
other_pollutants <- c("PM10", "NO2", "O3", "SO2", "CO")


# ============================================================
# 4. Select Top 3 High and Top 3 Low PM2.5 Countries
# ============================================================

pm25_country <- air_clean %>%
  filter(Pollutant == "PM2.5") %>%
  group_by(country) %>%
  summarise(
    avg_pm25 = mean(Value, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  filter(n_obs >= 5)

top3_high <- pm25_country %>%
  arrange(desc(avg_pm25)) %>%
  slice_head(n = 3) %>%
  mutate(group = "High PM2.5")

top3_low <- pm25_country %>%
  arrange(avg_pm25) %>%
  slice_head(n = 3) %>%
  mutate(group = "Low PM2.5")

selected_countries <- bind_rows(top3_high, top3_low) %>%
  arrange(desc(avg_pm25)) %>%
  mutate(
    country_label = paste0(country, "<br>PM2.5=", round(avg_pm25, 1))
  )


# ============================================================
# 5. Prepare Heatmap Data
# ============================================================

heatmap_data <- air_clean %>%
  filter(
    country %in% selected_countries$country,
    Pollutant %in% other_pollutants
  ) %>%
  group_by(country, Pollutant) %>%
  summarise(
    avg_value = mean(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    selected_countries %>%
      select(country, avg_pm25, group, country_label),
    by = "country"
  ) %>%
  group_by(Pollutant) %>%
  mutate(
    log_value = log1p(avg_value),
    normalized_value = ifelse(
      max(log_value, na.rm = TRUE) == min(log_value, na.rm = TRUE),
      0.5,
      scales::rescale(log_value, to = c(0, 1))
    )
  ) %>%
  ungroup() %>%
  mutate(
    country = factor(country, levels = selected_countries$country),
    country_label = factor(country_label, levels = selected_countries$country_label),
    Pollutant = factor(Pollutant, levels = other_pollutants)
  )


# ============================================================
# 6. Prepare World Map Data
# ============================================================

all_country_pm25 <- air_clean %>%
  filter(Pollutant == "PM2.5") %>%
  group_by(country) %>%
  summarise(
    avg_pm25 = mean(Value, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  filter(n_obs >= 5)

all_map_points <- air_clean %>%
  filter(Pollutant == "PM2.5") %>%
  group_by(country, Location, lat, lon) %>%
  summarise(
    avg_pm25 = mean(Value, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  )

selected_map_points <- all_map_points %>%
  filter(country %in% selected_countries$country) %>%
  left_join(
    selected_countries %>% select(country, group),
    by = "country"
  )

world_shapes <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

all_country_shapes <- world_shapes %>%
  left_join(
    all_country_pm25,
    by = c("admin" = "country")
  )

selected_shapes <- world_shapes %>%
  left_join(
    selected_countries,
    by = c("admin" = "country")
  ) %>%
  filter(!is.na(avg_pm25))


# ============================================================
# 7. Define UI
# ============================================================

ui <- fluidPage(
  
  titlePanel("PM2.5 High vs Low Countries: Pollution Structure Explorer"),
  
  tabsetPanel(
    
    tabPanel(
      "1. Heatmap + Regression",
      br(),
      h4("Pollutant Structure Heatmap"),
      p("X-axis shows the six selected countries ranked by average PM2.5. Y-axis shows other pollutants."),
      p("The heatmap uses log-normalized values within each pollutant to avoid scale dominance."),
      p("Click any heatmap cell to update the PM2.5 relationship plot below."),
      plotlyOutput("structure_heatmap", height = "560px"),
      br(),
      h4("PM2.5 Relationship Plot"),
      plotlyOutput("scatter_plot", height = "600px")
    ),
    
    tabPanel(
      "2. World Map",
      br(),
      h4("World Map of PM2.5 Distribution"),
      p("All countries with PM2.5 data are colored by average PM2.5. The selected six countries are highlighted with stronger borders."),
      
      fluidRow(
        column(
          4,
          selectInput(
            "map_group",
            "Select country group:",
            choices = c("All selected countries", "High PM2.5", "Low PM2.5"),
            selected = "All selected countries"
          )
        ),
        column(
          4,
          selectInput(
            "map_country",
            "Select country:",
            choices = c("All countries", selected_countries$country),
            selected = "All countries"
          )
        )
      ),
      
      leafletOutput("pm25_map", height = "720px")
    )
  )
)


# ============================================================
# 8. Define Server
# ============================================================

server <- function(input, output, session) {
  
  # ------------------------------------------------------------
  # 8.1 Update country selector based on selected group
  # ------------------------------------------------------------
  
  observeEvent(input$map_group, {
    
    available_countries <- selected_countries %>%
      filter(
        input$map_group == "All selected countries" |
          group == input$map_group
      ) %>%
      pull(country)
    
    updateSelectInput(
      session,
      "map_country",
      choices = c("All countries", available_countries),
      selected = "All countries"
    )
  })
  
  
  # ------------------------------------------------------------
  # 8.2 Draw Heatmap
  # ------------------------------------------------------------
  
  output$structure_heatmap <- renderPlotly({
    
    plot_ly(
      data = heatmap_data,
      x = ~country_label,
      y = ~Pollutant,
      z = ~normalized_value,
      type = "heatmap",
      source = "heat_click",
      colorscale = list(
        c(0, "#f7f7f7"),
        c(0.25, "#d6e6f2"),
        c(0.5, "#92c5de"),
        c(0.75, "#4393c3"),
        c(1, "#2166ac")
      ),
      zmin = 0,
      zmax = 1,
      text = ~paste(
        "Country:", country,
        "<br>Group:", group,
        "<br>Average PM2.5:", round(avg_pm25, 1),
        "<br>Pollutant:", Pollutant,
        "<br>Raw average value:", round(avg_value, 2),
        "<br>Normalized value:", round(normalized_value, 2)
      ),
      hoverinfo = "text",
      colorbar = list(title = "Normalized value")
    ) %>%
      layout(
        title = "Normalized Pollutant Structure by Country",
        xaxis = list(
          title = "Countries Ranked by PM2.5",
          tickangle = -30
        ),
        yaxis = list(title = "Other Pollutants"),
        margin = list(l = 90, r = 40, b = 120, t = 80)
      )
  })
  
  
  # ------------------------------------------------------------
  # 8.3 Draw Country-Level Regression Plot
  # ------------------------------------------------------------
  
  output$scatter_plot <- renderPlotly({
    
    click <- event_data("plotly_click", source = "heat_click")
    
    selected_pollutant <- if (is.null(click)) {
      "PM10"
    } else {
      as.character(click$y)
    }
    
    scatter_data <- air_clean %>%
      filter(
        country %in% selected_countries$country,
        Pollutant %in% c("PM2.5", selected_pollutant)
      ) %>%
      group_by(country, Pollutant) %>%
      summarise(
        avg_value = mean(Value, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = Pollutant,
        values_from = avg_value
      ) %>%
      drop_na() %>%
      left_join(
        selected_countries %>% select(country, group),
        by = "country"
      )
    
    validate(
      need(nrow(scatter_data) >= 3, "Not enough country-level data for regression.")
    )
    
    cor_value <- cor(
      scatter_data$`PM2.5`,
      scatter_data[[selected_pollutant]],
      use = "complete.obs"
    )
    
    lm_model <- lm(scatter_data[[selected_pollutant]] ~ scatter_data$`PM2.5`)
    r_squared <- summary(lm_model)$r.squared
    mean_pm25 <- mean(scatter_data$`PM2.5`, na.rm = TRUE)
    
    p <- ggplot(
      scatter_data,
      aes(
        x = `PM2.5`,
        y = .data[[selected_pollutant]],
        color = group,
        text = paste(
          "Country:", country,
          "<br>Group:", group,
          "<br>Average PM2.5:", round(`PM2.5`, 2),
          paste0("<br>Average ", selected_pollutant, ": "),
          round(.data[[selected_pollutant]], 2)
        )
      )
    ) +
      geom_point(size = 5, alpha = 0.85) +
      geom_smooth(
        aes(group = 1),
        method = "lm",
        se = FALSE,
        color = "#E67E22",
        linewidth = 1.4
      ) +
      geom_smooth(
        aes(group = 1),
        method = "loess",
        se = FALSE,
        color = "#2C7FB8",
        linewidth = 1.2,
        linetype = "dashed"
      ) +
      geom_vline(
        xintercept = mean_pm25,
        linetype = "dotted",
        color = "black",
        linewidth = 0.8
      ) +
      scale_color_manual(
        values = c(
          "High PM2.5" = "#F8766D",
          "Low PM2.5" = "#00BFC4"
        )
      ) +
      labs(
        title = paste0("PM2.5 vs ", selected_pollutant, " across Six Selected Countries"),
        subtitle = paste0(
          "Correlation = ", round(cor_value, 3),
          " | Linear R² = ", round(r_squared, 3),
          " | Dotted line = mean PM2.5"
        ),
        x = "Average PM2.5",
        y = paste0("Average ", selected_pollutant),
        color = "Country group"
      ) +
      theme_minimal(base_size = 15)
    
    ggplotly(p, tooltip = "text")
  })
  
  
  # ------------------------------------------------------------
  # 8.4 Draw World Map
  # ------------------------------------------------------------
  
  output$pm25_map <- renderLeaflet({
    
    # Filter selected six countries by group
    filtered_selected_shapes <- selected_shapes %>%
      filter(
        input$map_group == "All selected countries" |
          group == input$map_group
      )
    
    # Default view: all global monitoring sites
    if (input$map_group == "All selected countries" &&
        input$map_country == "All countries") {
      
      filtered_points <- all_map_points %>%
        mutate(group = "All dataset countries")
      
    } else {
      
      filtered_points <- selected_map_points %>%
        filter(
          input$map_group == "All selected countries" |
            group == input$map_group
        )
    }
    
    # If one country is selected, keep only that country's shape and points
    if (input$map_country != "All countries") {
      
      filtered_selected_shapes <- selected_shapes %>%
        filter(admin == input$map_country | name == input$map_country)
      
      filtered_points <- selected_map_points %>%
        filter(country == input$map_country)
    }
    
    # Palette for all countries
    pal_all <- colorNumeric(
      palette = "YlOrRd",
      domain = all_country_pm25$avg_pm25,
      na.color = "#F2F2F2",
      reverse = FALSE
    )
    
    # Palette for selected countries
    pal_selected <- colorNumeric(
      palette = "YlOrRd",
      domain = all_country_pm25$avg_pm25,
      na.color = "#F2F2F2",
      reverse = FALSE
    )
    
    # Create base map
    m <- leaflet() %>%
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      
      # Layer 1: all countries with PM2.5 data
      addPolygons(
        data = all_country_shapes %>% filter(!is.na(avg_pm25)),
        fillColor = ~pal_all(avg_pm25),
        fillOpacity = 0.45,
        color = "#D0D0D0",
        weight = 0.5,
        opacity = 0.7,
        group = "All PM2.5 countries",
        popup = ~paste0(
          "<b>", admin, "</b><br>",
          "Dataset country average PM2.5: ", round(avg_pm25, 2), "<br>",
          "Observations: ", n_obs
        )
      ) %>%
      
      # Layer 2: selected six countries
      addPolygons(
        data = filtered_selected_shapes,
        fillColor = ~pal_selected(avg_pm25),
        fillOpacity = 0.78,
        color = "#2C7FB8",
        weight = 2.4,
        opacity = 1,
        group = "Selected six countries",
        popup = ~paste0(
          "<b>", admin, "</b><br>",
          "Group: ", group, "<br>",
          "Selected country average PM2.5: ", round(avg_pm25, 2), "<br>",
          "Observations: ", n_obs
        )
      ) %>%
      
      # Monitoring sites
      addCircleMarkers(
        data = filtered_points,
        lng = ~lon,
        lat = ~lat,
        radius = 2.6,
        fillColor = "#333333",
        fillOpacity = 0.45,
        color = "#111111",
        opacity = 0.6,
        weight = 0.25,
        group = "Monitoring sites",
        popup = ~paste0(
          "<b>", Location, "</b><br>",
          "Country: ", country, "<br>",
          "Group: ", group, "<br>",
          "Monitoring-site PM2.5: ", round(avg_pm25, 2), "<br>",
          "Observations: ", n_obs
        ),
        label = ~paste0(country, " | ", Location, " | PM2.5=", round(avg_pm25, 1))
      ) %>%
      
      addLegend(
        pal = pal_all,
        values = all_country_pm25$avg_pm25,
        title = "Country Avg PM2.5",
        opacity = 0.8,
        position = "bottomright"
      ) %>%
      
      addLayersControl(
        overlayGroups = c(
          "All PM2.5 countries",
          "Selected six countries",
          "Monitoring sites"
        ),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      
      hideGroup("Monitoring sites") %>%
      
      setMaxBounds(
        lng1 = -180, lat1 = -60,
        lng2 = 180,  lat2 = 85
      )
    
    # Zoom logic
    if (input$map_country != "All countries" && nrow(filtered_selected_shapes) > 0) {
      
      bbox <- tryCatch(
        sf::st_bbox(filtered_selected_shapes),
        error = function(e) NULL
      )
      
      if (!is.null(bbox) && all(is.finite(bbox))) {
        m <- m %>%
          fitBounds(
            lng1 = as.numeric(bbox["xmin"]),
            lat1 = as.numeric(bbox["ymin"]),
            lng2 = as.numeric(bbox["xmax"]),
            lat2 = as.numeric(bbox["ymax"])
          )
      } else {
        m <- m %>%
          setView(lng = 20, lat = 20, zoom = 2)
      }
      
    } else {
      
      m <- m %>%
        setView(lng = 20, lat = 20, zoom = 2)
    }
    
    m
  })
}


# ============================================================
# 9. Run App
# ============================================================

shinyApp(ui, server)