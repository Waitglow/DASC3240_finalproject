# DASC3240 Final Project

## 🌍 Global Air Quality Research Platform: PM2.5 Distribution and Pollutant Analysis

------------------------------------------------------------------------

## 1. Project Overview

This project develops an interactive **Shiny web application** to explore global air quality patterns, with a particular focus on **PM2.5** and its relationship with other air pollutants.

PM2.5 refers to fine particulate matter with a diameter of 2.5 micrometers or smaller. Because these particles are small enough to enter the human respiratory system, PM2.5 is closely related to environmental quality, public health, urban development, industrial activity, and transportation.

The application guides users through a complete analytical workflow:

1.  Data cleaning and quality control
2.  Pollutant correlation analysis
3.  PM2.5 country ranking
4.  Country-level PM2.5 pollutant profiles
5.  Global PM2.5 map visualization
6.  GDP and geography radar comparison
7.  Final public-awareness call-to-action page

The goal of the project is not only to visualize air pollution data, but also to help users understand why PM2.5 matters and how pollution patterns differ across countries.

### Research Questions

> What is the relationship between PM2.5 and other air pollutants?

> Which countries show the highest and lowest average PM2.5 levels after data cleaning?

> How do GDP, geographic location, PM2.5 records, and pollutant diversity differ among selected high-PM2.5 and low-PM2.5 countries?

> How can interactive visualization help users better understand global air pollution patterns?

------------------------------------------------------------------------

## 2. Shiny App Link and Running Instructions

### 2.1 Online Shiny App Link

The deployed Shiny app is available here:

<https://havoc0101.shinyapps.io/3240finaltest/>

and *(If the backup amount is insufficient)*

<https://waitglow.shinyapps.io/DASC3240_finalproject/>

### 2.2 GitHub Repository

The GitHub repository is available here:

<https://github.com/Havoc0101/3240finaltest>

### 2.3 Run the App from GitHub in RStudio Console

If users want to run the app locally from GitHub, they can copy and run the following code in the RStudio Console:

``` r
shiny::runGitHub(
  repo = "3240finaltest",
  username = "Havoc0101"
)
```

and *(If the backup amount is insufficient)*

```r

shiny::runGitHub(
  repo = "DASC3240_finalproject",
  username = "Waitglow",
  ref = "main"
)

```

### 2.4 Run the App Locally from Downloaded Files

Users can also download or clone the GitHub repository and run the app locally in RStudio.

First, install the required packages in the RStudio Console:

``` r
install.packages(c(
  "shiny",
  "tidyverse",
  "plotly",
  "leaflet",
  "sf",
  "rnaturalearth",
  "rnaturalearthdata",
  "htmlwidgets",
  "shinyjs",
  "markdown",
  "readr",
  "dplyr",
  "tidyr",
  "ggplot2",
  "stringr",
  "purrr",
  "scales"
))
```

Then open the project folder in RStudio and run:

``` r
shiny::runApp()
```

or:

``` r
shiny::runApp("app.R")
```

### 2.5 Deployment Note

This project is deployed using **shinyapps.io**, so users can directly open the online Shiny app link in a web browser.

The app can also be run locally from GitHub using `shiny::runGitHub()`. This provides an alternative way to reproduce the app in a local RStudio environment.

A Shinylive browser-only version was tested, but the full app is not ideal for Shinylive because it uses spatial and interactive packages such as `sf`, `leaflet`, `rnaturalearth`, and `plotly`, which can be memory-intensive in a browser-only WebAssembly environment.

------------------------------------------------------------------------

## 3. Required R Packages

Before running the app, users may need to install the following packages manually in the RStudio Console:

``` r
install.packages(c(
  "shiny",
  "tidyverse",
  "plotly",
  "leaflet",
  "sf",
  "rnaturalearth",
  "rnaturalearthdata",
  "htmlwidgets",
  "shinyjs",
  "markdown",
  "readr",
  "dplyr",
  "tidyr",
  "ggplot2",
  "stringr",
  "purrr",
  "scales"
))
```

Then load the required packages by running the app. The main package calls are included at the beginning of `app.R`.

------------------------------------------------------------------------

## 4. Dataset Description

### 4.1 Dataset 1: World Air Quality Data 2024

- Dataset name: **World Air Quality Data 2024 (Updated)**
- Source: [World Air Quality Data 2024 (Updated)](https://www.kaggle.com/datasets/kanchana1990/world-air-quality-data-2024-updated/data)
- File used in this project: `data/world_air_quality.csv`

### Description

This dataset provides global air quality measurements from monitoring stations around the world. It includes pollutant types, pollutant values, units, location information, country labels, source names, and update timestamps.

### Key Variables

- `Country Code`
- `City`
- `Location`
- `Coordinates`
- `Pollutant`
- `Source Name`
- `Unit`
- `Value`
- `Last Updated`
- `Country Label`

### 4.2 Dataset 2: World Bank GDP Ranking Dataset 2024

- Dataset name: **World Bank GDP Ranking Dataset 2024**
- Source: [World Bank GDP Ranking Dataset 2024](https://www.kaggle.com/datasets/mdmahfuzsumon/world-bank-gdp-ranking-dataset-2024)
- File used in this project: `data/GDP.csv`

### Description

This dataset provides 2024 GDP information for countries and economies. It is used to support the GDP and geography radar comparison page in the Shiny app.

### Key Variables

- Country or economy code
- Country or economy name
- GDP in 2024, measured in millions of US dollars

------------------------------------------------------------------------

## 5. Data License and Attribution

Both datasets were obtained from Kaggle.

### License

- Source platform: Kaggle
- License type: CC BY 4.0, where applicable
- Usage: The datasets can be used, modified, and distributed for academic and research purposes.
- Attribution: Proper acknowledgment should be given to the original data providers on Kaggle.

### Image Attribution

The final call-to-action page uses three air-pollution-related images from iStock. Image credits and links are included inside the Shiny app.

The image links are:

1.  Industrial emissions:\
    <https://www.istockphoto.com/hk/%E7%85%A7%E7%89%87/%E7%94%9F%E6%85%8B%E7%81%BD%E9%9B%A3-gm1141520118-305861595>

2.  Human exposure:\
    <https://www.istockphoto.com/hk/%E7%85%A7%E7%89%87/asian-woman-wearing-an-n95-mask-for-protect-bad-air-pollution-gm2150501657-571664864>

3.  Traffic and haze:\
    <https://www.istockphoto.com/hk/%E7%85%A7%E7%89%87/cars-at-rush-hour-driving-through-thick-smog-gm174655376-8276806>

------------------------------------------------------------------------

## 6. Data Preparation and Cleaning

The original air quality dataset was cleaned before visualization. The main cleaning steps include:

1.  Renaming columns into standardized variable names
2.  Removing missing country names, pollutant names, and pollutant values
3.  Converting pollutant values into numeric format
4.  Removing negative pollutant values
5.  Focusing on PM2.5 data for country-level ranking
6.  Removing countries with fewer than three PM2.5 observations
7.  Removing countries whose PM2.5 values are all zero
8.  Creating a final clean dataset for visual analysis

These steps help reduce unreliable results caused by missing values, invalid records, extremely small sample sizes, or all-zero PM2.5 readings.

------------------------------------------------------------------------

## 7. Application Structure and Storytelling

The Shiny app follows a structured storytelling design.

### 7.1 Opening Page

The opening page introduces PM2.5 through a typewriter-style animation. It explains why PM2.5 matters and welcomes users to the platform.

### 7.2 Cleaning Page

This page shows the data-cleaning process. It includes:

- Observation counts before and after cleaning
- A table of countries removed by cleaning rules
- Multilingual explanation cards

### 7.3 Correlation Page

This page shows a pollutant-by-pollutant correlation heatmap. Users can click non-diagonal cells to generate regression plots between selected pollutants.

### 7.4 Ranking Page

This page ranks countries by average PM2.5 concentration after cleaning. The top three and bottom three countries are highlighted for further analysis.

### 7.5 Country Profile Page

This page compares within-country correlations between PM2.5 and other pollutants for the selected top and bottom PM2.5 countries.

### 7.6 Map Page

This page visualizes global average PM2.5 concentration on an interactive Leaflet map. Users can sort countries, select a specific country, or view groups of high-PM2.5 and low-PM2.5 countries.

### 7.7 GDP and Geography Radar Page

This page compares selected countries using a radar chart based on:

1.  Average PM2.5
2.  GDP in 2024
3.  Average latitude
4.  Average longitude
5.  Number of PM2.5 records
6.  Number of pollutant types

The radar indicators are normalized to make variables with different units comparable.

### 7.8 Final Call-to-Action Page

The final page uses three visual examples, including industrial emissions, human exposure, and traffic haze, to remind users that air pollution is not distant from daily life.

------------------------------------------------------------------------

## 8. Interactivity and Animation

The app includes multiple interactive and animated features:

- Typewriter animation on the opening page
- Multilingual Markdown explanations in English, Chinese, and Japanese
- Navigation buttons with active-page highlighting
- Interactive Plotly heatmaps
- Click-triggered regression plots
- Interactive Leaflet maps
- Country zoom and quick-jump map controls
- Animated radar chart
- Background music controls
- Polaroid-style final image cards

These features make the app more engaging and allow users to explore air quality patterns from multiple perspectives.

------------------------------------------------------------------------

## 9. Visualization Design

The visualization design follows these principles:

- Clear titles and subtitles
- Consistent PM2.5 units
- Color gradients that reflect pollutant intensity or correlation strength
- Interactive tooltips for detailed information
- Highlighting of selected countries
- Separation between global analysis and country-level analysis
- Use of maps, heatmaps, rankings, radar charts, and final visual storytelling

The design aims to balance analytical clarity with user engagement.

------------------------------------------------------------------------

## 10. Project Structure

The project repository is organized as follows:

``` text
DASC3240_finalproject/
├── app.R
├── README.md
├── LICENSE
├── .gitignore
├── data/
│   ├── world_air_quality.csv
│   └── GDP.csv
├── about/
│   ├── opening_en.md
│   ├── opening_zh.md
│   ├── opening_ja.md
│   ├── cleaning_en.md
│   ├── cleaning_zh.md
│   ├── cleaning_ja.md
│   ├── correlation_en.md
│   ├── correlation_zh.md
│   ├── correlation_ja.md
│   ├── ranking_en.md
│   ├── ranking_zh.md
│   ├── ranking_ja.md
│   ├── profile_en.md
│   ├── profile_zh.md
│   ├── profile_ja.md
│   ├── map_en.md
│   ├── map_zh.md
│   ├── map_ja.md
│   ├── radar_en.md
│   ├── radar_zh.md
│   ├── radar_ja.md
│   ├── final_en.md
│   ├── final_zh.md
│   └── final_ja.md
└── www/
    ├── bgm.mp3
    ├── istockphoto-1141520118-1024x1024.jpg
    ├── istockphoto-2150501657-1024x1024.jpg
    └── istockphoto-174655376-1024x1024.jpg
```

### Folder Description

- **app.R**\
  Contains the main Shiny application, including UI, server logic, data processing, visualization, interaction, and deployment-ready code.

- **data/**\
  Stores the datasets used in the project.

- **about/**\
  Stores Markdown explanation files for different pages and languages.

- **www/**\
  Stores static assets such as images and background music used by the Shiny app.

- **README.md**\
  Provides project documentation, dataset description, running instructions, and team contribution information.

------------------------------------------------------------------------

## 11. Workflow Diagram

The project workflow can be summarized as follows:

1.  **Dataset Collection**\
    Obtain air quality and GDP datasets from Kaggle.

2.  **Data Cleaning and Preprocessing**\
    Clean invalid observations and remove unreliable PM2.5 country records.

3.  **Exploratory Data Analysis**\
    Calculate country-level pollutant averages and PM2.5 rankings.

4.  **Visualization Design**\
    Create ranking plots, heatmaps, maps, radar charts, and final visual storytelling pages.

5.  **Shiny App Development**\
    Build an interactive application with multilingual Markdown support and animated components.

6.  **Deployment and Sharing**\
    Deploy the app through shinyapps.io and share the GitHub repository.

------------------------------------------------------------------------

## 12. Team Contributions

| Member | Responsibility |
|------------------------------------|------------------------------------|
| ZHANG, Chuxi | Project Lead: Overall coordination, conceptualization, UI design strategy, README development inspiration for the image and music modules, and final integration. |
| HE, Bingyi | Data cleaning, PM2.5 analysis, map/radar/final page design, README development, Fix UI and teammate code bugs. |
| LIN, Zirong | Heatmap and correlation analysis |
| YANG, Taoming | Data cleaning and map drawing Add map filter country interaction |
| CHENG, Wing Him | Map drawing and bug fixing/adding information to maps |

All members contributed through GitHub using a structured version-control workflow, including branch-based development and regular commits.

------------------------------------------------------------------------

## 13. AI Declaration

Generative AI tools, including **ChatGPT (OpenAI GPT-5.5)** and **Gemini 3.1**, were used in this project for limited assistance, including:

- Debugging R and Shiny code
- Improving code comments and documentation clarity
- Refining README structure and wording
- Assisting with multilingual Markdown explanation drafts
- Suggesting visualization and UI layout improvements

All core academic work, including dataset selection, data cleaning, data analysis, visualization design, interpretation of results, and Shiny application development, was completed and reviewed by the group members.

All AI-assisted outputs were carefully checked, edited, and validated by the group. The group takes full responsibility for the originality, accuracy, and academic integrity of the final submission.
