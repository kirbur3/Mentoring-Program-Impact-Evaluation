# Mentoring-Program-Impact-Evaluation

This project is listed on my resume as "Mentoring Program Impact Evaluation."

Real-world team project: statistical evaluation of the ECCA Community-School Partnership mentoring program at Washougal High School using 2024-25 academic year data (~30,000 records across five source files, merged into WHS_cleaned_6.csv).

Tools (mine): R, dplyr, ggplot2, gridExtra, mgcv
Tools (team): R, Python, pandas, mgcv, mgcViz

## Exploratory Data Analysis (STAT409_Project.Rmd) - My Contribution

This file centers on exploratory analysis of mentoring participation at Washougal High School. Using a cleaned and merged dataset of weekly student-course records, I examined how mentored students were distributed across courses, subjects, and semesters, explored enrollment patterns, identified where mentored students clustered, and compared participation across different academic areas. Visualizations built with dplyr, ggplot2, and gridExtra helped reveal trends and concentrations that informed later modeling.

- Mentored vs. non-mentored student counts broken down by semester, course description, subject, and grade year
- Unique mentored student counts across both semesters, and per-semester splits
- Weekly cumulative mentoring session totals by student and by class, across both semesters
- Weekly attendance trend visualizations by grade level (Semesters 1 and 2 separately)
- Custom skewness function (Bowley-style: 3*(mean-median)/sd) applied to grade percentage distributions by week, split by mentored vs. non-mentored course status
- Same custom skewness function applied to absence distributions over time
- All visualizations built with ggplot2; multi-panel figures composed with gridExtra

## Statistical Modeling and Visualization (STAT409_Project_Modeling.R) - Team Contribution
- Single-class geometry plot: student-level Z-score trajectories within a specific class (MAT522), comparing mentored vs. unmentored students week by week, with IQR ribbon for the non-mentored group and red X markers for attended mentoring sessions
- GAM (gam_model_grades) fit using mgcv on mentored students only: smooth terms for week-within-semester (k=15), running class mentoring total (k=6), and total absences (k=6); interaction terms for mentor course status x subject category and mentor course status x grade year; random effects for student intercepts and student-level week slopes
- Prediction function (f.predmat): simulates grade Z-score trajectories with 95% CIs for a typical 10th-grade student across 5 subject categories and mentoring-gap scenarios (every 1-4 weeks), excluding random effects for population-level estimates

## Key Finding
Consistent, early mentoring, especially biweekly sessions, was associated with sustained grade improvements and fewer absences, with spillover effects into non-mentored classes. Findings featured in The Columbian (August 2025).

## Deliverables
- STAT409_Project.Rmd - EDA file
- STAT409_Project_Modeling.R - modeling script
- ECCA Mentoring Program at Washougal High Presentation.pptx - team presentation
- ECCA Mentoring Program Report - project report
- WHS_cleaned_6.csv - merged and cleaned dataset

## Source Data Files
These files were merged, cleaned, and manipulated to create WHS_cleaned_6.csv:
- Mentoring 24-25 - SEMESTER 2 STUDENT PARTICIPANTS - 2024-25 - Mentoring 24-25 - SEMESTER 2 STUDENT PARTICIPANTS - 2024-25.csv
- WHS Mentoring Data Weeks 2-10.csv
- WHS Mentoring Data Weeks 11-20.csv
- WHS Mentoring Data Weeks 21-30.csv
- WHS Mentoring Data Weeks 31-34.csv
