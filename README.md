# DASC3240_finalproject

## 🧠 Academic Performance Analysis: The Impact of Study Habits on Exam Scores

------------------------------------------------------------------------

## 1. Project Overview(改 等做完的改)

This project develops an **interactive Shiny application** to explore and explain how students’ study habits affect their academic performance.

Rather than only visualizing the data, the application is designed to provide **clear storytelling and meaningful insights**, helping users understand key factors influencing exam scores.

### Research Question

> How do study habits and lifestyle factors influence students’ exam performance?

------------------------------------------------------------------------

## 2. Dataset Description

### 2.1 Background

- Dataset: Student Exam Scores and Study Habits Dataset

- Source: <https://www.kaggle.com/datasets/robiulhasanjisan/student-exam-scores-and-study-habits-dataset>

- Description: This dataset contains information on students’ academic performance along with various behavioral and lifestyle factors, such as study hours, attendance, sleep patterns, and social habits.

- Structure:

  - Number of observations: **6,607**

  - Number of variables: **11**

  - Key variables:(我感觉把我们用的 写出来 我把variables + units全写了)

  - Final_Exam_Score (Final exam score)

  - Hours_Studied (Study hours per week)

  - Attendance (Class attendance rate, %)

  - Sleep_Hours (Average sleep hours per night)

  - Previous_Scores (Previous exam scores)

  - Tutoring_Sessions (Number of tutoring sessions attended)

  - Parental_Involvement (Level of parental involvement: Low / Medium / High)

  - Access_to_Resources (Availability of learning resources: Low / Medium / High)

  - Extracurricular_Activities (Participation in extracurricular activities: Yes / No)

  - Motivation_Level (Level of learning motivation: Low / Medium / High)

  - 

    ## Internet_Access (Access to internet: Yes / No)

### 2.2 Data Collection

The dataset is publicly available on Kaggle and represents typical student study behaviors and performance outcomes. It is suitable for educational analysis and visualization purposes.

------------------------------------------------------------------------

### 2.3 License

- Source platform: Kaggle

- License type: MIT License

- Usage: The dataset can be freely used, modified, and distributed for academic and research purposes.

- Attribution: Proper acknowledgment should be given to the original dataset provider on Kaggle.

- Restrictions: The dataset is provided "as is" without warranty, and users should comply with the terms of the MIT License.

------------------------------------------------------------------------

## 3. Data Preparation and Cleaning

The following preprocessing steps were performed:

- Removed missing or incomplete records where necessary
- Checked for invalid or unrealistic values
- Converted variables into appropriate formats (numeric / categorical)
- Selected relevant variables for analysis and visualization
- [这里补：是否做了标准化 / 分组 / 新变量构造，比如 high vs low study hours]

These steps ensured that the dataset is clean, consistent, and suitable for analysis.

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

- Students who consistently spend more time studying tend to achieve higher exam scores
- Attendance is positively correlated with academic performance
- Lifestyle factors such as insufficient sleep and excessive social media usage may negatively affect exam results

------------------------------------------------------------------------

### So What?

> These findings suggest that academic performance is not solely determined by study time, but also influenced by broader lifestyle habits. Maintaining consistent study routines and healthy daily behaviors can significantly improve academic outcomes.

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

Interactivity enhances: - User engagement - Data exploration flexibility - Understanding of relationships across subgroups

------------------------------------------------------------------------

## 7. Code Quality and Reproducibility

- The application runs without errors
- Code is modular and well-structured
- Clear variable naming and comments are provided
- The project is reproducible with the included instructions

------------------------------------------------------------------------

## 8. Project Structure

### 📁 Description

- **app.R** Contains the main Shiny application, including both UI and server logic.

- **about.md(L20说的我不确定fianl project需要不)** Provides background information about the dataset and project context.

- **insight.md(L20说的我不确定fianl project需要不)** Summarizes key insights and interpretations derived from the analysis.

- **data** Stores the dataset used for analysis.

- **test.Rmd** *(optional目前我测试的 代码 我不确定老师 啥要求 文件)* Used for exploratory analysis and testing during development.

- **README.md & LICENSE** Provide project documentation and licensing information.

- **.gitignore** Ensures unnecessary files are excluded from version control.

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

Generative AI tools (e.g., [MODEL NAME, e.g., ChatGPT (OpenAI GPT-5)]自己写用了啥模型) were used in this project for limited assistance, including: - 自己写干了啥 - 自己写干了啥 - 自己写干了啥

All core academic work, including dataset selection, data cleaning, data analysis, visualization design, interpretation of results, and Shiny application development, was completed by the group members.

All AI-assisted outputs were carefully reviewed, edited, and validated. The group takes full responsibility for the originality, accuracy, and academic integrity of the final submission.
