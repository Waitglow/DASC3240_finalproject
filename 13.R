library(shiny)
library(tidyverse)
library(plotly)
library(leaflet)
library(sf)
library(rnaturalearth)
library(scales)
library(htmlwidgets)
library(shinyjs)

# 屏蔽 jsonlite 警告（解决控制台提示）
options(jsonlite.verbose.output = FALSE)

# ============================================================
# 1. Read Data
# ============================================================

air_raw <- readr::read_delim(
  "C:/Users/ZhangChuxi/OneDrive - HKUST Connect/桌面/finalproject/DASC3240_finalproject/data/world_air_quality.csv",
  delim = ";",
  show_col_types = FALSE
)

# ============================================================
# 2. Clean Data
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
    convert = TRUE,
    fill = "right",
    remove = FALSE
  ) %>%
  mutate(
    Value = as.numeric(Value),
    country_code = toupper(country_code),
    country = case_when(
      str_detect(str_to_lower(country), "taiwan") ~ "China",
      str_detect(str_to_lower(country), "hong kong") ~ "China",
      country_code %in% c("TW", "TWN", "HK", "HKG") ~ "China",
      TRUE ~ country
    ),
    country_code = case_when(
      country == "China" ~ "CN",
      TRUE ~ country_code
    )
  ) %>%
  filter(
    !is.na(country),
    !is.na(country_code),
    !is.na(Pollutant),
    !is.na(Value),
    Value >= 0
  )

pollutants_all <- c("PM2.5", "PM10", "NO2", "O3", "SO2", "CO")

# ============================================================
# 3. Country PM2.5 Ranking
# ============================================================

pollutant_coverage <- air_clean %>%
  filter(Pollutant %in% pollutants_all) %>%
  group_by(country) %>%
  summarise(
    n_pollutants_available = n_distinct(Pollutant),
    .groups = "drop"
  )

pm25_country <- air_clean %>%
  filter(Pollutant == "PM2.5") %>%
  group_by(country, country_code) %>%
  summarise(
    avg_pm25 = mean(Value, na.rm = TRUE),
    n_pm25 = n(),
    .groups = "drop"
  ) %>%
  left_join(pollutant_coverage, by = "country") %>%
  filter(
    n_pm25 >= 3,
    n_pollutants_available >= 4
  )

top3_high <- pm25_country %>%
  arrange(desc(avg_pm25)) %>%
  slice_head(n = 3) %>%
  mutate(pm25_group = "High PM2.5")

top3_low <- pm25_country %>%
  arrange(avg_pm25) %>%
  slice_head(n = 3) %>%
  mutate(pm25_group = "Low PM2.5")

selected_countries <- bind_rows(top3_high, top3_low)

# ============================================================
# 4. Radar Data
# ============================================================

radar_raw <- air_clean %>%
  filter(
    country %in% selected_countries$country,
    Pollutant %in% pollutants_all
  ) %>%
  group_by(country, Pollutant) %>%
  summarise(
    avg_value = mean(Value, na.rm = TRUE),
    .groups = "drop"
  )

radar_data <- expand_grid(
  country = selected_countries$country,
  Pollutant = pollutants_all
) %>%
  left_join(radar_raw, by = c("country", "Pollutant")) %>%
  left_join(
    selected_countries %>% select(country, pm25_group),
    by = "country"
  ) %>%
  mutate(
    avg_value = replace_na(avg_value, 0),
    log_value = log1p(avg_value),
    Pollutant = factor(Pollutant, levels = pollutants_all)
  ) %>%
  group_by(Pollutant) %>%
  mutate(
    pollutant_score = ifelse(
      max(log_value, na.rm = TRUE) == min(log_value, na.rm = TRUE),
      0.1,
      rescale(log_value, to = c(0.08, 1))
    )
  ) %>%
  ungroup() %>%
  group_by(country) %>%
  mutate(
    profile_score = ifelse(
      max(log_value, na.rm = TRUE) == min(log_value, na.rm = TRUE),
      0.1,
      rescale(log_value, to = c(0.08, 1))
    )
  ) %>%
  ungroup() %>%
  mutate(
    display_value = 0.65 * pollutant_score + 0.35 * profile_score
  )

# ============================================================
# 5. Map Data
# ============================================================

country_avg <- air_clean %>%
  filter(Pollutant == "PM2.5") %>%
  group_by(country, country_code) %>%
  summarise(
    pm25 = mean(Value, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  )

world_shapes <- tryCatch(
  {
    rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
  },
  error = function(e) {
    rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  }
) %>%
  mutate(
    iso_a2 = toupper(iso_a2),
    iso_a3 = toupper(iso_a3),
    admin = case_when(
      str_detect(str_to_lower(admin), "taiwan") ~ "China",
      str_detect(str_to_lower(admin), "hong kong") ~ "China",
      iso_a2 %in% c("TW", "HK") ~ "China",
      iso_a3 %in% c("TWN", "HKG") ~ "China",
      TRUE ~ admin
    ),
    name = case_when(
      str_detect(str_to_lower(name), "taiwan") ~ "China",
      str_detect(str_to_lower(name), "hong kong") ~ "China",
      iso_a2 %in% c("TW", "HK") ~ "China",
      iso_a3 %in% c("TWN", "HKG") ~ "China",
      TRUE ~ name
    ),
    iso_a2 = case_when(
      admin == "China" ~ "CN",
      TRUE ~ iso_a2
    ),
    iso_a3 = case_when(
      admin == "China" ~ "CHN",
      TRUE ~ iso_a3
    ),
    admin_lower = str_to_lower(admin),
    name_lower = str_to_lower(name)
  )

pm25_iso2 <- country_avg %>%
  filter(nchar(country_code) == 2) %>%
  rename(
    country_iso2 = country,
    pm25_iso2 = pm25,
    n_obs_iso2 = n_obs
  )

pm25_iso3 <- country_avg %>%
  filter(nchar(country_code) == 3) %>%
  rename(
    country_iso3 = country,
    pm25_iso3 = pm25,
    n_obs_iso3 = n_obs
  )

pm25_name <- country_avg %>%
  mutate(country_lower = str_to_lower(country)) %>%
  rename(
    country_name = country,
    pm25_name = pm25,
    n_obs_name = n_obs
  )

map_data <- world_shapes %>%
  left_join(pm25_iso2, by = c("iso_a2" = "country_code")) %>%
  left_join(pm25_iso3, by = c("iso_a3" = "country_code")) %>%
  left_join(pm25_name, by = c("admin_lower" = "country_lower")) %>%
  mutate(
    country = coalesce(country_iso2, country_iso3, country_name, admin),
    pm25 = coalesce(pm25_iso2, pm25_iso3, pm25_name),
    n_obs = coalesce(n_obs_iso2, n_obs_iso3, n_obs_name)
  ) %>%
  select(
    geometry, admin, name, iso_a2, iso_a3,
    country, pm25, n_obs
  )

china_pm25 <- country_avg %>%
  filter(country == "China") %>%
  summarise(
    pm25 = mean(pm25, na.rm = TRUE),
    n_obs = sum(n_obs, na.rm = TRUE),
    .groups = "drop"
  )

if (nrow(china_pm25) > 0 && is.finite(china_pm25$pm25[1])) {
  map_data <- map_data %>%
    mutate(
      is_china_shape = iso_a2 == "CN" |
        iso_a3 == "CHN" |
        str_detect(str_to_lower(admin), "china") |
        str_detect(str_to_lower(name), "china"),
      country = ifelse(is_china_shape, "China", country),
      pm25 = ifelse(is_china_shape, china_pm25$pm25[1], pm25),
      n_obs = ifelse(is_china_shape, china_pm25$n_obs[1], n_obs)
    ) %>%
    select(-is_china_shape)
}

selected_shapes <- map_data %>%
  filter(country %in% selected_countries$country) %>%
  left_join(
    selected_countries %>%
      select(country, pm25_group, avg_pm25, n_pm25, n_pollutants_available),
    by = "country"
  )

# ============================================================
# ✅ 最终UI：修复音乐 + 优化样式
# ============================================================

ui <- tagList(
  useShinyjs(),
  fluidPage(
    tags$head(
      tags$meta(charset = "UTF-8"),
      tags$title("Air Quality Dashboard"),
      tags$style(HTML('
        body {
          background-color: #f8f9ff;
          background-image: radial-gradient(circle, #e0e8ff 8%, transparent 8%),
                            radial-gradient(circle, #ffe8f0 8%, transparent 8%);
          background-size: 40px 40px;
          background-position: 0 0, 20px 20px;
          color: #2d3748;
          font-family: "Segoe UI", Roboto, sans-serif;
          margin: 0;
          padding: 0;
        }
        .dynamic-island {
          background: rgba(255,255,255,0.9);
          backdrop-filter: blur(16px);
          border-radius: 28px;
          padding: 20px 30px;
          margin: 20px auto 25px;
          box-shadow: 0 8px 30px rgba(0,0,0,0.08);
          text-align: center;
          width: 90%;
          max-width: 1000px;
          position: sticky;
          top: 12px;
          z-index: 99999;
          border: 1px solid #eee;
        }
        .dynamic-island h1 {
          font-size: 26px;
          font-weight: 600;
          margin: 0;
          color: #3a4a5f;
        }
        .dynamic-island .sub-title {
          font-size: 14px;
          color: #718096;
          margin-top: 4px;
        }
        .card {
          background: rgba(255,255,255,0.95);
          border-radius: 20px;
          padding: 24px;
          box-shadow: 0 4px 15px rgba(0,0,0,0.05);
          margin-bottom: 20px;
          border: 1px solid #e2e8f0;
          position: relative;
          z-index: 10;
        }
        .card-title {
          font-size: 18px;
          font-weight: 600;
          margin-bottom: 16px;
          color: #3a4a5f;
        }
        .control-row, .selectize-control, .btn, select {
          position: relative;
          z-index: 9999 !important;
        }
        .form-control, .selectize-input {
          background: #ffffff !important;
          border: 1px solid #e2e8f0 !important;
          color: #2d3748 !important;
          border-radius: 10px !important;
        }
        .btn {
          border-radius: 10px;
          border: none;
          z-index: 9999 !important;
          font-weight: 500;
        }
        .btn-primary { background-color: #4b9df2; color: white; }
        .btn-success { background-color: #60c5ba; color: white; }
        .btn-warning { background-color: #f9b395; color: white; }
        .btn-outline-light {
          background: white !important;
          border: 1px solid #ddd !important;
          color: #444 !important;
        }
        #pm25_map {
          background: #f0f5ff !important;
          border-radius: 16px;
          overflow: hidden;
          position: relative;
          z-index: 5;
        }
        #radar_chart {
          border-radius: 16px;
          overflow: hidden;
          position: relative;
          z-index: 5;
        }
        .lang-bar {
          display: flex;
          justify-content: flex-end;
          gap: 8px;
          margin-bottom: 12px;
        }
        .radar-buttons {
          margin-bottom: 12px;
        }
      '))
    ),
    
    # 标题栏
    div(class = "dynamic-island",
        h1(span(id="title_main", "Global Air Quality Research Platform")),
        div(class = "sub-title", id="title_sub", "PM2.5 Distribution & Pollutant Analysis")
    ),
    
    div(class = "container-fluid",
        # 语言 + 音乐
        div(class = "lang-bar",
            actionButton("lang_en", "English", class = "btn btn-outline-light btn-sm"),
            actionButton("lang_cn", "中文", class = "btn btn-outline-light btn-sm"),
            actionButton("music_play", "Play BGM", class = "btn btn-success btn-sm"),
            actionButton("music_pause", "Pause", class = "btn btn-warning btn-sm")
        ),
        
        # 控制面板
        div(class = "card control-row",
            div(class = "card-title", id="ctrl_title", "Map Control Panel"),
            fluidRow(
              column(4, 
                     div(id = "map_group_label",
                         selectInput("map_group", label = "Select Group",
                                     choices = c("All selected countries", "High PM2.5", "Low PM2.5"))
                     )
              ),
              column(4, 
                     div(id = "map_country_label",
                         selectInput("map_country", label = "Selected Country",
                                     choices = c("Click a highlighted country first", selected_countries$country))
                     )
              )
            )
        ),
        
        # 地图 + 雷达图
        fluidRow(
          column(8,
                 div(class = "card",
                     div(class = "card-title", id="map_card", "Global PM2.5 Concentration Distribution"),
                     leafletOutput("pm25_map", height = "700px")
                 )
          ),
          column(4,
                 div(class = "card",
                     div(class = "card-title", id="radar_card", "Pollutant Radar Profile"),
                     div(class = "radar-buttons",
                         actionButton("radar_zoom_in", "+", class = "btn btn-primary"),
                         actionButton("radar_zoom_out", "-", class = "btn btn-primary"),
                         actionButton("radar_reset", "Reset", class = "btn btn-secondary")
                     ),
                     plotlyOutput("radar_chart", height = "700px")
                 )
          )
        )
    ),
    
    # ✅ 修复版音乐（用本地安全音频）
    tags$audio(id="bgm", 
               src="bm_test1.mp3", 
               type="audio/mp3", loop=TRUE, preload="auto"),
    
    # 翻译
    tags$script(HTML('
      const translations = {
        en: {
          title_main: "Global Air Quality Research Platform",
          title_sub: "PM2.5 Distribution and Multi-Pollutant Radar Analysis",
          ctrl_title: "Map Control Panel",
          map_group_label: "Select Group",
          map_country_label: "Selected Country",
          map_card: "Global PM2.5 Distribution",
          radar_card: "Pollutant Radar Profile"
        },
        cn: {
          title_main: "全球空气质量研究平台",
          title_sub: "PM2.5 空间分布与多污染物雷达分析",
          ctrl_title: "地图控制面板",
          map_group_label: "选择分组",
          map_country_label: "选择国家",
          map_card: "全球PM2.5浓度分布图",
          radar_card: "污染物特征雷达图"
        }
      };
      document.getElementById("lang_en").onclick = () => {
        for(let k in translations.en) {
          let el = document.getElementById(k);
          if(el) el.innerText = translations.en[k];
        }
      };
      document.getElementById("lang_cn").onclick = () => {
        for(let k in translations.cn) {
          let el = document.getElementById(k);
          if(el) el.innerText = translations.cn[k];
        }
      };
    '))
  )
)

# ============================================================
# SERVER：修复音乐 + 所有交互
# ============================================================

server <- function(input, output, session) {
  
  # ✅ 修复版音乐控制（强制播放）
  observeEvent(input$music_play, {
    runjs("
    var audio = document.getElementById('bgm');
    audio.volume = 0.3;
    audio.currentTime = 0; // 从头播放
    audio.play().catch(function(e) {
      alert('请先点击一下页面空白处，再点播放按钮！');
    });
  ")
  })
  
  observeEvent(input$music_pause, {
    runjs("document.getElementById('bgm').pause()")
  })
  
  selected_now <- reactiveVal(NULL)
  radar_zoom <- reactiveVal(1)
  
  # 雷达图缩放
  observeEvent(input$radar_zoom_in, { radar_zoom(max(0.25, radar_zoom() * 0.75)) })
  observeEvent(input$radar_zoom_out, { radar_zoom(min(4, radar_zoom() * 1.25)) })
  observeEvent(input$radar_reset, { radar_zoom(1) })
  
  # 分组切换
  observeEvent(input$map_group, {
    selected_now(NULL)
    available_countries <- selected_countries %>%
      filter(input$map_group == "All selected countries" | pm25_group == input$map_group) %>%
      pull(country)
    updateSelectInput(session, "map_country",
                      choices = c("Click a highlighted country first", available_countries))
  })
  
  # 下拉框选择国家
  observeEvent(input$map_country, {
    if (!is.null(input$map_country) && input$map_country != "Click a highlighted country first") {
      selected_now(input$map_country)
    }
  }, ignoreInit = TRUE)
  
  # 地图点击国家
  observeEvent(input$pm25_map_shape_click, {
    clicked <- input$pm25_map_shape_click$id
    if (!is.null(clicked) && clicked %in% selected_countries$country) {
      selected_now(clicked)
      updateSelectInput(session, "map_country", selected = clicked)
    }
  })
  
  # 地图渲染
  output$pm25_map <- renderLeaflet({
    current_country <- selected_now()
    filtered_selected_shapes <- selected_shapes %>%
      filter(input$map_group == "All selected countries" | pm25_group == input$map_group)
    if (!is.null(current_country)) {
      filtered_selected_shapes <- selected_shapes %>% filter(country == current_country)
    }
    pal_pm25 <- colorNumeric(palette = "YlOrRd", domain = map_data$pm25, na.color = "#e2e8f0")
    
    m <- leaflet(options = leafletOptions(zoomControl = TRUE, backgroundColor = "#f0f5ff")) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = map_data %>% filter(!is.na(pm25)),
        fillColor = ~pal_pm25(pm25), fillOpacity = 0.6,
        color = "#cccccc", weight = 0.6, layerId = ~country,
        popup = ~paste0(country, "<br>PM2.5: ", round(pm25,2))
      ) %>%
      addPolygons(
        data = filtered_selected_shapes,
        layerId = ~country, fillColor = ~pal_pm25(pm25), fillOpacity = 0.9,
        color = "#4b9df2", weight = 4
      ) %>%
      addLegend(pal = pal_pm25, values = map_data$pm25, title = "PM2.5", position = "bottomright") %>%
      setView(lng=20, lat=20, zoom=2)
    
    if (!is.null(current_country) && nrow(filtered_selected_shapes)>0) {
      bbox <- st_bbox(filtered_selected_shapes)
      m <- m %>% fitBounds(bbox["xmin"],bbox["ymin"],bbox["xmax"],bbox["ymax"])
    }
    m
  })
  
  # 雷达图渲染
  output$radar_chart <- renderPlotly({
    current_country <- selected_now()
    req(current_country)
    
    df <- radar_data %>% filter(country == current_country) %>% arrange(Pollutant)
    req(nrow(df) > 0)
    
    color_now <- ifelse(unique(df$pm25_group)=="High PM2.5","#f9844d","#4b9df2")
    fill_now <- ifelse(unique(df$pm25_group)=="High PM2.5","rgba(249,132,77,0.25)","rgba(75,157,242,0.25)")
    max_r <- 1 * radar_zoom()
    
    steps <- seq(0.02,1,length.out=22)
    df_anim <- map_dfr(seq_along(steps), function(i){
      df %>% mutate(animated_value=display_value*steps[i], frame_id=i)
    })
    df_anim_closed <- df_anim %>% group_by(frame_id) %>% group_modify(~bind_rows(.x,.x[1,])) %>% ungroup()
    
    plot_ly(data=df_anim_closed, type="scatterpolar", mode="lines+markers",
            r=~animated_value, theta=~Pollutant, frame=~frame_id,
            fill="toself", fillcolor=fill_now,
            line=list(color=color_now, width=3), marker=list(color=color_now, size=7)) %>%
      layout(
        title=list(text=current_country, x=0.5, font=list(color="#3a4a5f")),
        paper_bgcolor="#ffffff", plot_bgcolor="#ffffff",
        polar=list(
          bgcolor="#ffffff",
          radialaxis=list(visible=T, range=c(0,max_r), tickfont=list(color="#718096")),
          angularaxis=list(tickfont=list(color="#3a4a5f"))
        ),
        showlegend=F, margin=list(l=10,r=10,t=50,b=10)
      ) %>%
      animation_opts(frame=60, transition=30) %>%
      animation_slider(hide=T) %>% config(displayModeBar=F)
  })
}

shinyApp(ui, server)