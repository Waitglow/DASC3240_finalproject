# DASC3240_finalproject

## 🧠 Academic Performance Analysis: The Impact of Study Habits on Exam Scores

------------------------------------------------------------------------

## 1. Project Overview

This project develops an **interactive Shiny application** to explore and explain how students’ study habits affect their academic performance.

Rather than only visualizing the data, the application is designed to provide **clear storytelling and meaningful insights**, helping users understand key factors influencing exam scores.

### Research Question

> How do study habits and lifestyle factors influence students’ exam performance?

------------------------------------------------------------------------

## 2. Dataset Description

### 2.1 Background

-   Dataset: Student Exam Scores and Study Habits Dataset\

-   Source: <https://www.kaggle.com/datasets/robiulhasanjisan/student-exam-scores-and-study-habits-dataset>\

-   Description:\
    This dataset contains information on students’ academic performance along with various behavioral and lifestyle factors, such as study hours, attendance, sleep patterns, and social habits.

-   Structure:

    -   Number of observations: **\~1000（你打开CSV确认一下，改成准确数字）**
    -   Number of variables: **\~10–15（根据你实际列数改）**
    -   Key variables:
        -   Exam Score
        -   Study Hours
        -   Attendance
        -   Sleep Hours
        -   Social Media Usage
        -   Gender / Other demographic variables

------------------------------------------------------------------------

### 2.2 Data Collection

The dataset is publicly available on Kaggle and represents typical student study behaviors and performance outcomes. It is suitable for educational analysis and visualization purposes.

------------------------------------------------------------------------

### 2.3 License

-   Source platform: Kaggle\
-   Usage: Public dataset for educational and analytical use\
-   Note: Users should refer to the Kaggle dataset page for detailed licensing and attribution requirements

------------------------------------------------------------------------

## 3. Data Preparation and Cleaning

The following preprocessing steps were performed:

-   Removed missing or incomplete records where necessary\
-   Checked for invalid or unrealistic values\
-   Converted variables into appropriate formats (numeric / categorical)\
-   Selected relevant variables for analysis and visualization\
-   [这里补：是否做了标准化 / 分组 / 新变量构造，比如 high vs low study hours]

These steps ensured that the dataset is clean, consistent, and suitable for analysis.

------------------------------------------------------------------------

## 4. Storytelling and Insight

### 4.1 Narrative Structure

The application follows a structured analytical flow:

1.  **Introduction** – Dataset background and research objective\
2.  **Exploration** – Interactive visualizations of study habits\
3.  **Analysis** – Identification of key patterns and relationships\
4.  **Conclusion** – Interpretation and insights

------------------------------------------------------------------------

### 4.2 Key Insights

-   Students who consistently spend more time studying tend to achieve higher exam scores\
-   Attendance is positively correlated with academic performance\
-   Lifestyle factors such as insufficient sleep and excessive social media usage may negatively affect exam results

------------------------------------------------------------------------

### So What?

> These findings suggest that academic performance is not solely determined by study time, but also influenced by broader lifestyle habits. Maintaining consistent study routines and healthy daily behaviors can significantly improve academic outcomes.

------------------------------------------------------------------------

## 5. Visualization Design

-   Clear titles, axis labels, and units are provided\
-   Consistent and readable color schemes are used\
-   No misleading visual representations are included\
-   Visualizations are designed for clarity and interpretability

------------------------------------------------------------------------

## 6. Interactivity and Animation

### Features

-   Interactive filtering (e.g., by study hours or demographic groups)\
-   Hover tooltips for detailed inspection\
-   Zoom functionality for focused exploration

### Justification

Interactivity enhances: - User engagement\
- Data exploration flexibility\
- Understanding of relationships across subgroups

------------------------------------------------------------------------

## 7. Code Quality and Reproducibility

-   The application runs without errors\
-   Code is modular and well-structured\
-   Clear variable naming and comments are provided\
-   The project is reproducible with the included instructions

------------------------------------------------------------------------

## 8. Project Structure

DASC3240_finalproject/
├── app.R
├── about.md
├── insight.md
├── README.md
├── LICENSE
├── .gitignore
├── DASC3240_finalproject.Rproj
├── data/
│   └── student_dataset.csv
└── test.Rmd

### 📁 Description

-   **app.R**\
    Contains the main Shiny application, including both UI and server logic.

-   **about.md**\
    Provides background information about the dataset and project context.

-   **insight.md**\
    Summarizes key insights and interpretations derived from the analysis.

-   **data/**\
    Stores the dataset used for analysis.

-   **test.Rmd** *(optional)*\
    Used for exploratory analysis and testing during development.

-   **README.md & LICENSE**\
    Provide project documentation and licensing information.

-   **.gitignore**\
    Ensures unnecessary files are excluded from version control.

------------------------------------------------------------------------

## 9. Team Contributions

| Member          | Responsibility                                       |
|-----------------|------------------------------------------------------|
| ZHANG, Chuxi    | Project leader, overall design and coordination      |
| HE, Bingyi      | eg: Data analysis, visualization, README development |
| LIN, Zirong     | eg: Shiny application implementation(自己改)         |
| YANG, Taoming   | eg: Data preprocessing and cleaning(自己改)          |
| CHENG, Wing Him | eg: Documentation and reporting(自己改)              |

All members contributed through GitHub using a structured version control workflow, including branch-based development and regular commits.

------------------------------------------------------------------------

## 10. Workflow Diagram

\`\`\`mermaid flowchart LR A[Dataset] --\> B[Data Cleaning] B --\> C[Exploratory Analysis] C --\> D[Visualization Design] D --\> E[Shiny App Development] E --\> F[Insight Generation] F --\> G[Deployment]
