# Harvard-PISA-project

This report documents a predictive modeling project conducted as part of the HarvardX program in Data Science.

PISA  – Predicting School and Student Success
This project explores the determinants of educational success using the **LearningTower dataset**, which compiles international data from the OECD PISA studies at the school and student levels. 
The research work on PISA survey data was made possible thanks to the great work of the MIT team:
Wang K, Yacobellis P, Siregar E, Romanes S, Fitter K, Dalla Riva G, Cook D, Tierney N, Dingorkar P, Sai Subramanian S, Chen G (2024). learningtower: OECD PISA Datasets from 2000-2022 in an Easy-to-Use Format. R package version 1.1.0, https://github.com/kevinwang09/learningtower, https://kevinwang09.github.io/learningtower/.

The projects also use those database:

**UNESCO School Gender Enrollment Index**
Source: UNESCO, World Bank Data 360
Description:
This dataset contains enrollment indices by gender for secondary and upper secondary education, sourced from UNESCO and curated for this project. It provides an overview of the relative school participation rates for males and females, allowing for the analysis of gender parity and educational equality across countries and over time.

**UNESCO Government Expenditure on Education**
Source: UNESCO, World Bank Data 360
Description:
This table reports the percentage of total government expenditure allocated to education for each country. It is based on UNESCO data and provides insight into national education priorities and funding trends, which are key contextual factors in the analysis of educational outcomes.

**ILO/World Bank Country Groupings (Region, Subregion, and Income Level)**
Source: ILOSTAT Country Groupings
Description:
This classification table groups countries by ILO region, subregion, and World Bank income classification (low, lower-middle, upper-middle, and high income). It enables regional and socioeconomic stratification of educational indicators, supporting comparative and equity-focused analysis.

**GDP per capita (PPP) /the OECD and Eurostat databases**
Source: World Bank Data 360
Description:
A country's gross domestic product (GDP) at purchasing power parity (PPP) per capita is the PPP value of all final goods and services produced within an economy in a given year, divided by the average (or mid-year) population for the same year. This is similar to nominal GDP per capita but adjusted for the cost of living in each country.

The primary goals of the project are:

1\. To estimate, for each student, the probability of “success”, by incorporating information about the student’s school, family  environment, and individual profile.

2\. To identify and interpret the key drivers of student success, and to suggest targeted policy recommendations.

To analyze and interpret the main drivers of educational achievement, offering actionable insights for educators and policymakers.

The project includes:

Data preparation and cleaning of multi-level educational data.

Exploratory data analysis and visualization.

Predictive modeling using machine learning algorithms.

Evaluation of model performance (e.g., RMSE).

Discussion of key findings and policy recommendations.

All code is written in R and leverages tidyverse for data processing and visualization.

author: Julie COLLEONI

Special thanks:

I would like to express my sincere gratitude to Professor Rafael Irizarry for his outstanding teaching and inspiring courses in Data Science at Harvard. His passion for the subject and dedication to sharing knowledge have been instrumental in shaping my understanding and appreciation of the field.
I would like to acknowledge also his books, which served as a valuable references throughout this project. Thanks to his Data Science program, my skills and understanding of the subject have improved tremendously.


Irizarry, R. A. (2022). *Introduction to Data Science: Data Analysis and
Prediction Algorithms with R (Part I)*. Self-published.
https://rafalab.dfci.harvard.edu/dsbook-part-1/

Irizarry, R. A. (2022). *Introduction to Data Science: Data Analysis and
Prediction Algorithms with R (Part II)*. Self-published.
https://rafalab.dfci.harvard.edu/dsbook-part-2/
