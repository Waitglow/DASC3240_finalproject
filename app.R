library(shiny)
library(tidyverse)
library(plotly)

# =========================
# 1. Read data
# =========================
# Change this path to your real file
data <- read.csv("data/student_dataset.csv")

# =========================
# 2. Data cleaning
# =========================
df <- data %>%
  select(
    Hours_Studied, Attendance, Sleep_Hours,
    Previous_Scores, Tutoring_Sessions, Final_Exam_Score,
    Motivation_Level, Parental_Involvement, Access_to_Resources
  ) %>%
  drop_na()

# Make categorical order stable
df$Motivation_Level <- factor(df$Motivation_Level, levels = c("Low", "Medium", "High"))
df$Parental_Involvement <- factor(df$Parental_Involvement, levels = c("Low", "Medium", "High"))
df$Access_to_Resources <- factor(df$Access_to_Resources, levels = c("Low", "Medium", "High"))

# =========================
# 3. PCA
# =========================
pca_vars <- df %>%
  select(
    Hours_Studied, Attendance, Sleep_Hours,
    Previous_Scores, Tutoring_Sessions, Final_Exam_Score
  )

pca_result <- prcomp(pca_vars, center = TRUE, scale. = TRUE)

pca_var_explained <- (pca_result$sdev^2) / sum(pca_result$sdev^2)

# =========================
# 4. K-means clustering
# =========================
set.seed(123)
km <- kmeans(scale(pca_vars), centers = 4, nstart = 25)

df <- df %>%
  mutate(
    PC1 = pca_result$x[, 1],
    PC2 = pca_result$x[, 2],
    cluster_num = km$cluster
  )

cluster_labels <- c(
  "1" = "Cluster 1",
  "2" = "Cluster 2",
  "3" = "Cluster 3",
  "4" = "Cluster 4"
)

df$cluster <- factor(
  cluster_labels[as.character(df$cluster_num)],
  levels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")
)

cluster_colors <- c(
  "Cluster 1" = "#F26B5B",
  "Cluster 2" = "#8FB339",
  "Cluster 3" = "#2BB3C0",
  "Cluster 4" = "#B46AE0"
)

# Fixed axis ranges so the animation does not jump
x_range <- range(df$PC1, na.rm = TRUE)
y_range <- range(df$PC2, na.rm = TRUE)

x_pad <- diff(x_range) * 0.12
y_pad <- diff(y_range) * 0.12

x_range <- c(x_range[1] - x_pad, x_range[2] + x_pad)
y_range <- c(y_range[1] - y_pad, y_range[2] + y_pad)

# =========================
# 5. UI
# =========================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .title-main {
        text-align: center;
        font-size: 34px;
        font-weight: 700;
        margin-bottom: 20px;
      }
      .side-box {
        background: #f7f7f7;
        border-radius: 12px;
        padding: 14px;
        margin-bottom: 14px;
      }
      .explain-box {
        background: #eef5ff;
        border: 1px solid #d6e6ff;
        border-radius: 12px;
        padding: 16px;
        margin-top: 20px;
      }
      .profile-box {
        background: #f7f7f7;
        border-radius: 12px;
        padding: 14px;
        margin-top: 10px;
      }
      .small-note {
        color: #555;
        font-size: 14px;
      }
    "))
  ),
  
  div(class = "title-main", "PCA Cluster Animation"),
  
  sidebarLayout(
    sidebarPanel(
      div(class = "side-box",
          selectInput(
            "anim_var",
            "Animation Variable:",
            choices = c("Motivation_Level", "Parental_Involvement", "Access_to_Resources"),
            selected = "Motivation_Level"
          ),
          
          sliderInput(
            "speed",
            "Playback Speed (ms per frame):",
            min = 400, max = 2500, value = 1200, step = 100
          ),
          
          selectInput(
            "cluster_filter",
            "Select Cluster:",
            choices = c("All", "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"),
            selected = "All"
          ),
          
          checkboxInput("show_hull", "Show Cluster Hull", TRUE),
          checkboxInput("show_labels", "Show Reference Lines", TRUE),
          checkboxInput("show_profile_text", "Show Student Profile Description", TRUE),
          
          div(class = "small-note",
              "The animation plays across the selected categorical variable, showing how different student groups are distributed in the PCA space.")
      ),
      
      conditionalPanel(
        condition = "input.show_profile_text == true",
        div(class = "profile-box",
            h4("Student Profile Description"),
            tags$p(style = "color:#F26B5B; font-weight:600;", "Cluster 1: High-performing"),
            tags$p(style = "color:#8FB339; font-weight:600;", "Cluster 2: Hard-working"),
            tags$p(style = "color:#2BB3C0; font-weight:600;", "Cluster 3: Low-engagement"),
            tags$p(style = "color:#B46AE0; font-weight:600;", "Cluster 4: Unstable")
        )
      )
    ),
    
    mainPanel(
      plotlyOutput("pca_anim", height = "700px"),
      
      div(class = "explain-box",
          h4("Interpretation"),
          tags$ul(
            tags$li("Each point represents a student, projected into a two-dimensional space using PCA based on six numerical variables."),
            tags$li("Colors indicate K-means clustering results, where each color represents a different student group."),
            tags$li("The animation is driven by the selected categorical variable, showing how distributions change across subgroups."),
            tags$li("This helps reveal how student clusters overlap, separate, and shift under different conditions.")
          )
      )
    )
  )
)

# =========================
# 6. Server
# =========================
server <- function(input, output, session) {
  
  filtered_df <- reactive({
    d <- df
    if (input$cluster_filter != "All") {
      d <- d %>% filter(cluster == input$cluster_filter)
    }
    d
  })
  
  hull_df <- reactive({
    d <- filtered_df()
    frame_var <- input$anim_var
    
    out <- d %>%
      group_by(.data[[frame_var]], cluster) %>%
      group_modify(~{
        if (nrow(.x) < 3) return(.x[0, ])
        h <- chull(.x$PC1, .x$PC2)
        hull <- .x[h, ]
        hull <- bind_rows(hull, hull[1, ])
        hull
      }) %>%
      ungroup()
    
    out
  })
  
  output$pca_anim <- renderPlotly({
    d <- filtered_df()
    frame_var <- input$anim_var
    
    if (frame_var == "Motivation_Level") {
      d[[frame_var]] <- factor(d[[frame_var]], levels = c("Low", "Medium", "High"))
    }
    if (frame_var == "Parental_Involvement") {
      d[[frame_var]] <- factor(d[[frame_var]], levels = c("Low", "Medium", "High"))
    }
    if (frame_var == "Access_to_Resources") {
      d[[frame_var]] <- factor(d[[frame_var]], levels = c("Low", "Medium", "High"))
    }
    
    p <- plot_ly()
    
    if (input$show_hull) {
      hd <- hull_df()
      
      if (nrow(hd) > 0) {
        split_list <- split(hd, list(hd[[frame_var]], hd$cluster), drop = TRUE)
        
        for (nm in names(split_list)) {
          tmp <- split_list[[nm]]
          if (nrow(tmp) < 4) next
          
          p <- p %>%
            add_trace(
              data = tmp,
              x = ~PC1,
              y = ~PC2,
              frame = tmp[[frame_var]],
              type = "scatter",
              mode = "lines",
              fill = "toself",
              fillcolor = paste0(cluster_colors[as.character(tmp$cluster[1])], "33"),
              line = list(color = cluster_colors[as.character(tmp$cluster[1])], width = 1),
              hoverinfo = "skip",
              showlegend = FALSE,
              inherit = FALSE
            )
        }
      }
    }
    
    for (cl in levels(d$cluster)) {
      tmp <- d %>% filter(cluster == cl)
      
      if (nrow(tmp) == 0) next
      
      p <- p %>%
        add_trace(
          data = tmp,
          x = ~PC1,
          y = ~PC2,
          frame = ~.data[[frame_var]],
          type = "scatter",
          mode = "markers",
          name = cl,
          marker = list(
            size = 10,
            color = cluster_colors[cl],
            opacity = 0.85,
            line = list(color = "white", width = 0.8)
          ),
          text = ~paste(
            "Cluster:", cluster,
            "<br>Motivation Level:", Motivation_Level,
            "<br>Parental Involvement:", Parental_Involvement,
            "<br>Access to Resources:", Access_to_Resources,
            "<br>Hours Studied:", round(Hours_Studied, 2),
            "<br>Attendance:", round(Attendance, 2),
            "<br>Sleep Hours:", round(Sleep_Hours, 2),
            "<br>Previous Scores:", round(Previous_Scores, 2),
            "<br>Tutoring Sessions:", round(Tutoring_Sessions, 2),
            "<br>Final Exam Score:", round(Final_Exam_Score, 2)
          ),
          hoverinfo = "text"
        )
    }
    
    shp <- list()
    if (input$show_labels) {
      shp <- list(
        list(
          type = "line",
          x0 = 0, x1 = 0,
          y0 = y_range[1], y1 = y_range[2],
          line = list(color = "gray60", dash = "dash")
        ),
        list(
          type = "line",
          x0 = x_range[1], x1 = x_range[2],
          y0 = 0, y1 = 0,
          line = list(color = "gray60", dash = "dash")
        )
      )
    }
    
    p %>%
      layout(
        title = list(
          text = "PCA Cluster Dynamic Visualization",
          x = 0.5,
          xanchor = "center",
          font = list(size = 24)
        ),
        xaxis = list(
          title = paste0("PC1 (", round(pca_var_explained[1] * 100, 1), "%)"),
          range = x_range,
          zeroline = FALSE
        ),
        yaxis = list(
          title = paste0("PC2 (", round(pca_var_explained[2] * 100, 1), "%)"),
          range = y_range,
          zeroline = FALSE
        ),
        plot_bgcolor = "#ffffff",
        paper_bgcolor = "#ffffff",
        legend = list(title = list(text = "<b>Cluster</b>")),
        shapes = shp
      ) %>%
      animation_opts(
        frame = input$speed,
        transition = round(input$speed * 0.35),
        redraw = FALSE
      ) %>%
      animation_slider(
        currentvalue = list(
          prefix = "Current Frame: ",
          font = list(size = 18)
        )
      ) %>%
      animation_button(
        x = 1,
        xanchor = "right",
        y = 1.12,
        yanchor = "top"
      )
  })
}

# =========================
# 7. Run app
# =========================
shinyApp(ui, server)