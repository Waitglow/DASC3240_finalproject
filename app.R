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
    country_label = paste0(country, "\nPM2.5=", round(avg_pm25, 1))
  )


# ============================================================
# 5. Prepare Country-Level Pollutant Profile
# ============================================================

country_profile <- air_clean %>%
  filter(
    country %in% selected_countries$country,
    Pollutant %in% pollutants_all
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
  left_join(
    selected_countries %>% select(country, group, avg_pm25, country_label),
    by = "country"
  )


# ============================================================
# 6. Prepare Normalized Heatmap Data
# ============================================================

heatmap_data <- country_profile %>%
  select(country, group, avg_pm25, country_label, all_of(other_pollutants)) %>%
  pivot_longer(
    cols = all_of(other_pollutants),
    names_to = "Pollutant",
    values_to = "raw_value"
  ) %>%
  group_by(Pollutant) %>%
  mutate(
    scaled_value = ifelse(
      max(raw_value, na.rm = TRUE) == min(raw_value, na.rm = TRUE),
      0.5,
      (raw_value - min(raw_value, na.rm = TRUE)) /
        (max(raw_value, na.rm = TRUE) - min(raw_value, na.rm = TRUE))
    )
  ) %>%
  ungroup() %>%
  mutate(
    country = factor(country, levels = selected_countries$country),
    Pollutant = factor(Pollutant, levels = rev(other_pollutants))
  )


# ============================================================
# 7. Prepare World Map Data
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
    selected_countries %>% select(country, group, avg_pm25_country = avg_pm25),
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
# 8. Define UI
# ============================================================

ui <- fluidPage(
  
  titlePanel("PM2.5 High vs Low Countries: Pollution Structure Explorer"),
  
  tabsetPanel(
    
    tabPanel(
      "1. Heatmap + Regression",
      br(),
      h4("Pollutant Structure Heatmap"),
      p("This heatmap shows normalized pollutant levels across the top 3 highest and top 3 lowest PM2.5 countries."),
      p("Click any heatmap cell to generate a PM2.5 relationship plot below."),
      plotlyOutput("structure_heatmap", height = "560px"),
      br(),
      h4("PM2.5 Relationship Plot"),
      plotlyOutput("scatter_plot", height = "540px")
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
# 9. Define Server
# ============================================================

server <- function(input, output, session) {
  
  # ------------------------------------------------------------
  # 9.1 Update country filter based on selected group
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
  # 9.2 Heatmap
  # ------------------------------------------------------------
  
  output$structure_heatmap <- renderPlotly({
    
    plot_ly(
      data = heatmap_data,
      x = ~country,
      y = ~Pollutant,
      z = ~scaled_value,
      type = "heatmap",
      source = "heat_click",
      colorscale = list(
        c(0, "#f7fbff"),
        c(0.25, "#c6dbef"),
        c(0.50, "#6baed6"),
        c(0.75, "#2171b5"),
        c(1, "#08306b")
      ),
      text = ~paste(
        "Country:", country,
        "<br>Group:", group,
        "<br>Average PM2.5:", round(avg_pm25, 2),
        "<br>Pollutant:", Pollutant,
        "<br>Raw average value:", round(raw_value, 2),
        "<br>Normalized heatmap value:", round(scaled_value, 2)
      ),
      hoverinfo = "text",
      colorbar = list(title = "Normalized value")
    ) %>%
      layout(
        title = "Normalized Pollutant Structure of High and Low PM2.5 Countries",
        xaxis = list(
          title = "Country with Average PM2.5",
          tickangle = -25,
          tickvals = selected_countries$country,
          ticktext = selected_countries$country_label
        ),
        yaxis = list(title = "Other Pollutants"),
        margin = list(l = 90, r = 40, b = 120, t = 80)
      )
  })
  
  
  # ------------------------------------------------------------
  # 9.3 Regression scatter plot triggered by heatmap click
  # ------------------------------------------------------------
  
  output$scatter_plot <- renderPlotly({
    
    click <- event_data("plotly_click", source = "heat_click")
    
    if (is.null(click)) {
      selected_pollutant <- "PM10"
      clicked_country <- selected_countries$country[1]
    } else {
      clicked_country <- as.character(click$x)
      selected_pollutant <- as.character(click$y)
    }
    
    scatter_data <- country_profile %>%
      filter(
        !is.na(`PM2.5`),
        !is.na(.data[[selected_pollutant]])
      )
    
    validate(
      need(nrow(scatter_data) >= 3, "Not enough country-level data for regression.")
    )
    
    cor_value <- cor(
      scatter_data$`PM2.5`,
      scatter_data[[selected_pollutant]],
      use = "complete.obs"
    )
    
    p <- ggplot(
      scatter_data,
      aes(
        x = `PM2.5`,
        y = .data[[selected_pollutant]],
        color = group,
        text = paste(
          "Country:", country,
          "<br>Group:", group,
          "<br>PM2.5:", round(`PM2.5`, 2),
          paste0("<br>", selected_pollutant, ": "), round(.data[[selected_pollutant]], 2)
        )
      )
    ) +
      geom_point(size = 4, alpha = 0.8) +
      geom_smooth(
        aes(group = 1),
        method = "lm",
        se = TRUE,
        color = "#f28e2b",
        linewidth = 1.3
      ) +
      geom_point(
        data = scatter_data %>% filter(country == clicked_country),
        aes(x = `PM2.5`, y = .data[[selected_pollutant]]),
        inherit.aes = FALSE,
        size = 6,
        shape = 21,
        fill = "yellow",
        color = "black",
        stroke = 1.2
      ) +
      labs(
        title = paste0("Country-level Relationship between PM2.5 and ", selected_pollutant),
        subtitle = paste0("Six selected countries | Correlation = ", round(cor_value, 3)),
        x = "Average PM2.5",
        y = paste0("Average ", selected_pollutant),
        color = "Country group"
      ) +
      theme_minimal(base_size = 15)
    
    ggplotly(p, tooltip = "text")
  })
  
  
  # ------------------------------------------------------------
  # 9.4 World map with country polygons and hidden monitoring sites
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
# 10. Run App
# ============================================================

shinyApp(ui, server)