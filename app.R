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

map_points <- air_clean %>%
  filter(
    country %in% selected_countries$country,
    Pollutant == "PM2.5"
  ) %>%
  group_by(country, Location, lat, lon) %>%
  summarise(
    avg_pm25 = mean(Value, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  left_join(
    selected_countries %>%
      select(country, group, avg_pm25_country = avg_pm25),
    by = "country"
  )

world_shapes <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

selected_shapes <- world_shapes %>%
  filter(admin %in% selected_countries$country | name %in% selected_countries$country) %>%
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
      h4("World Map of Selected PM2.5 Countries"),
      p("Country polygons show national average PM2.5. Monitoring sites are hidden by default and can be turned on from the layer control."),
      
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
  # 8.3 Country-level Scatter Plot with Regression Lines
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
    
    filtered_shapes <- selected_shapes %>%
      filter(
        input$map_group == "All selected countries" |
          group == input$map_group
      )
    
    filtered_points <- map_points %>%
      filter(
        input$map_group == "All selected countries" |
          group == input$map_group
      )
    
    if (input$map_country != "All countries") {
      filtered_shapes <- filtered_shapes %>%
        filter(admin == input$map_country | name == input$map_country)
      
      filtered_points <- filtered_points %>%
        filter(country == input$map_country)
    }
    
    validate(
      need(nrow(filtered_shapes) > 0, "No country polygon data for this selection."),
      need(nrow(filtered_points) > 0, "No monitoring-site data for this selection.")
    )
    
    pal_country <- colorNumeric(
      palette = "YlOrRd",
      domain = selected_countries$avg_pm25
    )
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      addPolygons(
        data = filtered_shapes,
        fillColor = ~pal_country(avg_pm25),
        fillOpacity = 0.35,
        color = "#4A90E2",
        weight = 1.2,
        opacity = 0.9,
        group = "Country PM2.5 area",
        popup = ~paste0(
          "<b>", admin, "</b><br>",
          "Group: ", group, "<br>",
          "Country average PM2.5: ", round(avg_pm25, 2), "<br>",
          "Observations: ", n_obs
        )
      ) %>%
      
      addCircleMarkers(
        data = filtered_points,
        lng = ~lon,
        lat = ~lat,
        radius = 3.5,
        fillColor = "#4A4A4A",
        fillOpacity = 0.55,
        color = "#222222",
        opacity = 0.75,
        weight = 0.4,
        group = "Monitoring sites",
        popup = ~paste0(
          "<b>", Location, "</b><br>",
          "Country: ", country, "<br>",
          "Group: ", group, "<br>",
          "Monitoring-site PM2.5: ", round(avg_pm25, 2), "<br>",
          "Observations: ", n_obs
        ),
        label = ~paste0(Location, " | PM2.5=", round(avg_pm25, 1))
      ) %>%
      
      addLegend(
        pal = pal_country,
        values = selected_countries$avg_pm25,
        title = "Country Avg PM2.5",
        opacity = 0.8
      ) %>%
      
      addLayersControl(
        overlayGroups = c("Country PM2.5 area", "Monitoring sites"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      
      hideGroup("Monitoring sites") %>%
      
      fitBounds(
        lng1 = min(filtered_points$lon, na.rm = TRUE),
        lat1 = min(filtered_points$lat, na.rm = TRUE),
        lng2 = max(filtered_points$lon, na.rm = TRUE),
        lat2 = max(filtered_points$lat, na.rm = TRUE)
      )
  })
}


# ============================================================
# 9. Run App
# ============================================================

shinyApp(ui, server)