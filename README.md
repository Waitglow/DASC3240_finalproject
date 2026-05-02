---
editor_options: 
  markdown: 
    wrap: 72
---

# DASC3240_finalproject

## 🌍 Global Air Quality Analysis 2024: Pollutants and PM2.5

------------------------------------------------------------------------

## 1. Project Overview

This project develops an interactive Shiny application to explore
**global air quality** patterns in 2024, analyze the distribution
characteristics of key pollutants, identify the three countries with the
**highest PM2.5 concentrations** and the three countries with the
**lowest PM2.5 concentrations**, create individual **heatmaps** for
these two groups(global pollution and 6 countries PM2.5 VS their other
pollutions), explore the **correlation between PM2.5 and other
pollutants** , comparing the **GPA** among all countries, and finally
reveal the **factors that cause PM2.5 concentration differences**.

### Research Question

> What is the correlation between PM2.5 and other pollutions?
>
> GDA causes PM2.5?

## 2. Dataset Description

### 2.1 Background

- Dataset1: World Air Quality Data 2024 (Updated)

- Source: [World Air Quality Data 2024
  (Updated)](https://www.kaggle.com/datasets/kanchana1990/world-air-quality-data-2024-updated/data)

- Description: This dataset provides up-to-date air quality measurements
  from monitoring stations around the world, including major pollutants,
  air quality index (AQI), geographic information, and time records.

- Structure:

  - Number of observations: **54255**
  - Number of variables: **10**
  - Key variables:
  - country code
  - city
  - Latitude
  - Longitude
  - Location
  - Pollutant
  - Source Name
  - Unit
  - Value
  - Last Updated
  - Country Label
  - AQI (Air Quality Index)

- Dataset2: World Bank GDP Ranking Dataset 2024

- Source:
  <https://www.kaggle.com/datasets/mdmahfuzsumon/world-bank-gdp-ranking-dataset-2024>

- Description: This dataset provides comprehensive 2024 gross domestic
  product (GDP) rankings and economic indicators for countries and
  economies worldwide, sourced directly from the World Bank's official
  World Development Indicators. enabling cross-country economic
  comparisons at market exchange rates and adjusted for cost of living
  differences.

- Structure:

  - Number of observations: **217**
  - Number of variables: **3**
  - Key variables:
  - Ranking
  - Economy
  - Country
  - GDP (millions of US dollars)

### 2.2 Data Collection

Data is aggregated from global public monitoring networks and official
environmental agencies, standardized for cross‑country comparison.
Suitable for environmental analysis, mapping, and public health
research.

------------------------------------------------------------------------

### 2.3 License

- Source platform: Kaggle

- License type: CC BY 4.0

- Usage: The dataset can be freely used, modified, and distributed for
  academic and research purposes.

- Attribution: Proper acknowledgment should be given to the original
  dataset provider on Kaggle.

- Restrictions: The dataset is provided "as is" without warranty, and
  users should comply with the terms of the CC BY 4.0

------------------------------------------------------------------------

## 3. Data Preparation and Cleaning

The following preprocessing steps were performed:

- Removed missing or incomplete records where necessary
- Delete the observation points that are less than three
- Checked for invalid or unrealistic values
- Converted variables into appropriate formats (numeric / categorical)
- Selected relevant variables for analysis and visualization

These steps ensured that the dataset is clean, consistent, and suitable
for analysis.

------------------------------------------------------------------------

## 4. Storytelling and Insight(我随便写的例子 到时候看我们要咋做 要改)

### 4.1 Narrative Structure

The application follows a structured analytical flow:

1.  **Introduction** – Dataset background and research objective
2.  **Exploration** – Interactive visualizations of study habits
3.  **Analysis** – Identification of key patterns and relationships
4.  **Conclusion** – Interpretation and insights

------------------------------------------------------------------------

### 4.2 Key Insights

- Students who consistently spend more time studying tend to achieve
  higher exam scores
- Attendance is positively correlated with academic performance
- Lifestyle factors such as insufficient sleep and excessive social
  media usage may negatively affect exam results

------------------------------------------------------------------------

### So What?

> These findings suggest that academic performance is not solely
> determined by study time, but also influenced by broader lifestyle
> habits. Maintaining consistent study routines and healthy daily
> behaviors can significantly improve academic outcomes.

------------------------------------------------------------------------

## 5. Visualization Design(例子到时候还得改我们用啥可视化了)

- Clear titles, axis labels, and units are provided
- Consistent and readable color schemes are used
- No misleading visual representations are included
- Visualizations are designed for clarity and interpretability

------------------------------------------------------------------------

## 6. Interactivity and Animation(例子到时候还得改我们用啥了)

### Features

- Interactive filtering (e.g., by study hours or demographic groups)
- Hover tooltips for detailed inspection
- Zoom functionality for focused exploration

### Justification

Interactivity enhances: - User engagement - Data exploration
flexibility - Understanding of relationships across subgroups

------------------------------------------------------------------------

## 7. Code Quality and Reproducibility

- The application runs without errors
- Code is modular and well-structured
- Clear variable naming and comments are provided
- The project is reproducible with the included instructions

------------------------------------------------------------------------

## 8. Project Structure

### 📁 Description

- **app.R** Contains the main Shiny application, including both UI and
  server logic.

- **data** Stores the dataset used for analysis.

- **README.md & LICENSE** Provide project documentation and licensing
  information.

- **.gitignore** Ensures unnecessary files are excluded from version
  control.

------------------------------------------------------------------------

## 9. Team Contributions

| Member          | Responsibility                                       |
|-----------------|------------------------------------------------------|
| ZHANG, Chuxi    | Project leader, overall design and coordination      |
| HE, Bingyi      | eg: Data analysis, visualization, README development |
| LIN, Zirong     | eg: Shiny application implementation(自己改)         |
| YANG, Taoming   | eg: Data preprocessing and cleaning(自己改)          |
| CHENG, Wing Him | eg: Documentation and reporting(自己改)              |

All members contributed through GitHub using a structured version
control workflow, including branch-based development and regular
commits.

------------------------------------------------------------------------

## 10. Workflow Diagram

The workflow of this project can be summarized as follows:

1.  **Dataset Collection**\
    Obtain the dataset from Kaggle

2.  **Data Cleaning & Preprocessing**\
    Handle missing values and prepare variables

3.  **Exploratory Data Analysis**\
    Identify patterns and relationships

4.  **Visualization Design**\
    Create clear and interpretable plots

5.  **Shiny App Development**\
    Build an interactive application

6.  **Insight Generation**\
    Extract key findings

7.  **Deployment & Sharing**\
    Publish and share the application

------------------------------------------------------------------------

## 11. AI Declaration(需要改最后完事的)

Generative AI tools (e.g., [MODEL NAME, e.g., ChatGPT (OpenAI
GPT-5)]自己写用了啥模型) were used in this project for limited
assistance, including: - 自己写干了啥 - 自己写干了啥 - 自己写干了啥

All core academic work, including dataset selection, data cleaning, data
analysis, visualization design, interpretation of results, and Shiny
application development, was completed by the group members.

All AI-assisted outputs were carefully reviewed, edited, and validated.
The group takes full responsibility for the originality, accuracy, and
academic integrity of the final submission.
