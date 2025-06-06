## ----PISA-dataset-overview-1-download-data,------------------------------------

# Load necessary packages
if (!require(tidyverse)) 
  install.packages("tidyverse", repos = "https://cloud.r-project.org/")

if(!require(caret)) 
  install.packages("caret", repos = "http://cran.us.r-project.org")

if (!require(janitor)) 
  install.packages("janitor", repos = "https://cloud.r-project.org/")

if (!require(DataExplorer)) 
  install.packages("DataExplorer", repos = "https://cloud.r-project.org/")

if (!require(corrplot)) 
  install.packages("corrplot", repos = "https://cloud.r-project.org/")

if (!require(xgboost)) 
  install.packages("xgboost", repos = "https://cloud.r-project.org/")

if (!require(Matrix)) 
  install.packages("Matrix", repos = "https://cloud.r-project.org/")

if (!require(ranger)) 
  install.packages("ranger", repos = "https://cloud.r-project.org/")

# For the PISA dataset
if (!require(learningtower)) 
  install.packages("learningtower", repos = "https://cloud.r-project.org/")

library(learningtower)
library(dplyr)
library(tidyverse)
library(caret)
library(tidyr)
library(ggplot2)
library(kableExtra)
library(janitor)
library(DataExplorer)   
library(corrplot)
library(ggrepel)
library(scales)
library(stringr)
library(xgboost)
library(Matrix)
library(ranger)

#load the entire student data
student <- load_student("all")

# loading the school data
data(school)

# loading the countrycode data
data(countrycode)
head(countrycode)


student_summary <- student |>
  group_by(year) |>
  tally(name = "Number of Students")

school_summary <- school |>
  group_by(year) |>
  tally(name = "Number of Schools")

combined_summary <- full_join(student_summary, school_summary, by = "year")

knitr::kable(combined_summary)



## ----PISA-dataset-additional-database, -----------------------------------------

# URLs to the raw CSV files in my GitHub repo
url_gender <- "https://raw.githubusercontent.com/ghjulia01/Harvard-PISA-project/main/UNESCO_School_gender_enrollment_index.csv"
url_expenditure <- "https://raw.githubusercontent.com/ghjulia01/Harvard-PISA-project/main/UNESCO-Government-expenditure-on-education.csv"
url_region <- "https://raw.githubusercontent.com/ghjulia01/Harvard-PISA-project/main/Region-subregion-OIT.csv"
url_gdpperppp <- "https://raw.githubusercontent.com/ghjulia01/Harvard-PISA-project/main/2022_GDP_per_Capita_PPP_in_constant_2021_international_dollar.csv"

# Read the datasets 

gender_index <- read_csv(url(url_gender))
expenditure <- read_csv(url(url_expenditure))
region_income <- read_csv(url(url_region))
gdp_perppp <- read_csv(url(url_gdpperppp))

# Check the column names
colnames(gender_index)
colnames(expenditure)
colnames(region_income)
colnames(gdp_perppp)



## ----PISA-data-set-unique-school-id, -------------------------------------------

# Remove leading zeros in school_id for SCHOOL
school <- school %>%
  mutate(school_id = as.character(as.integer(school_id)))

# SCHOOL dataset
school <- school %>%
  mutate(school_id = as.character(as.integer(school_id)),
         school_uid = paste(country, year, school_id, sep = "_"))

# STUDENT dataset
student <- student %>%
  mutate(school_id = as.character(as.integer(school_id)),
         school_uid = paste(country, year, school_id, sep = "_"))


## ----PISA-datatrain-glimpse, ---------------------------------------------------

# For the school dataset
glimpse(student)
glimpse(school)



## ----PISA-data-cleaning-uniqueness, --------------------------------------------


# Check uniqueness of school_uid in the school dataset
sum(duplicated(school$school_uid)) # should return 0

# Check for missing school_uids in the student dataset
sum(is.na(student$school_uid)) # should return 0



## ----PISA-data-cleaning-assessment, --------------------------------------------

# For student_train
student_na <- sapply(student, function(x) mean(is.na(x))) * 100
student_na <- round(student_na, 2)
student_na_df <- data.frame(
  Variable = names(student_na),
  Percent_Missing = student_na
) %>% arrange(desc(Percent_Missing))

knitr::kable(student_na_df, caption = "Percentage of Missing Values (student)")

# For school_train
school_na <- sapply(school, function(x) mean(is.na(x))) * 100
school_na <- round(school_na, 2)
school_na_df <- data.frame(
  Variable = names(school_na),
  Percent_Missing = school_na
) %>% arrange(desc(Percent_Missing))

knitr::kable(school_na_df, caption = "Percentage of Missing Values (school)")



## ----PISA-data-cleaning-assessment-correction, ---------------------------------


# Remove students without an ESCS Index (Economic, Social and Cultural Status)
student <- student %>% filter(!is.na(escs))

# Remove students without a math score 
student <- student %>% filter(!is.na(math))

# Remove students without a litteracy score 
student <- student %>% filter(!is.na(read))

# Remove students without a science score 
student <- student %>% filter(!is.na(science))

# Remove students without a gender variable 
student <- student %>% filter(!is.na(gender))

# Verification for student_train
student_na <- sapply(student, function(x) mean(is.na(x))) * 100
student_na <- round(student_na, 2)
student_na_df <- data.frame(
  Variable = names(student_na),
  Percent_Missing = student_na
) %>% arrange(desc(Percent_Missing))

knitr::kable(student_na_df, caption = "Percentage of Missing Values (student)")




## ----PISA-data-cleaning-harmonization, -----------------------------------------


# Convert all factors to character type
student <- student %>%
  mutate(across(where(is.factor), as.character))

school <- school %>%
  mutate(across(where(is.factor), as.character))



## ----PISA-data-cleaning-outlier-detection, -------------------------------------

# Retrieve summary statistics as vectors
math_summary <- summary(student$math)
read_summary <- summary(student$read)
science_summary <- summary(student$science)

# Names of statistics (Min, 1st Qu., Median, Mean, 3rd Qu., Max)
stats_names <- names(math_summary)

# Create a data frame
summary_df <- data.frame(
  Statistic = stats_names,
  Math = as.numeric(math_summary),
  Reading = as.numeric(read_summary),
  Science = as.numeric(science_summary)
)

# Display as a table with nice formatting
summary_df %>%
  kbl(
    caption = "Summary Statistics for Math, Reading, and Science Scores",
    digits = 2,
    align = "lccc",
    booktabs = TRUE
  ) %>%
  kable_styling(latex_options = "striped", font_size = 9)



## ----PISA-data-cleaning-outlier-correction, ------------------------------------



# Remove extreme values outside plausible PISA range
student <- student %>%
  filter(between(math, 50, 1000),
         between(read, 50, 1000),
         between(science, 50, 1000))

# Retrieve summary statistics as vectors
math_summary <- summary(student$math)
read_summary <- summary(student$read)
science_summary <- summary(student$science)

# Names of statistics (Min, 1st Qu., Median, Mean, 3rd Qu., Max)
stats_names <- names(math_summary)

# Create a data frame
summary_df <- data.frame(
  Statistic = stats_names,
  Math = as.numeric(math_summary),
  Reading = as.numeric(read_summary),
  Science = as.numeric(science_summary)
)

# Display as a table with nice formatting
summary_df %>%
  kbl(
    caption = "Summary Statistics for Math, Reading, and Science Scores",
    digits = 2,
    align = "lccc",
    booktabs = TRUE
  ) %>%
  kable_styling(latex_options = "striped", font_size = 10)



## ----PISA-data-cleaning-standardization, ---------------------------------------


# Check unique values for consistency
# Count of each category (with NA)
public_private_table <- as.data.frame(table(school$public_private, useNA = "always"))
colnames(public_private_table) <- c("School Type", "Count")

knitr::kable(
  public_private_table,
  caption = "Number of Schools by Type ",
  align = "lc",
  booktabs = TRUE
)


## ----PISA-data-cleaning-standardization-private-public, ------------------------

# Check for 'private' when fund_gov > 80
inconsistent_private_n <- school %>%
  filter(fund_gov > 80 & public_private == "private") %>%
  nrow()
# Check for 'public' when fund_gov < 50
inconsistent_public_n <- school %>%
  filter(fund_gov < 50 & public_private == "public") %>%
  nrow()

consistency_df <- data.frame(
  "Check" = c(
    "'private' with fund_gov > 80",
    "'public' with fund_gov < 50"
  ),
  "Number of inconsistent cases" = c(
    inconsistent_private_n,
    inconsistent_public_n
  )
)

knitr::kable(
  consistency_df,
  caption = "Number of Schools with Inconsistent 'public_private' Label and Government Funding",
  align = "lc",
  booktabs = TRUE
)



## ----PISA-data-cleaning-standardization-correction, ----------------------------

school <- school %>%
  mutate(
    public_private = case_when(
      fund_gov > 80 ~ "public",
      fund_gov < 50 ~ "private",
      TRUE ~ public_private
    )
  )

# Re-run the table to check new distribution
# Check for 'private' when fund_gov > 80
inconsistent_private_n <- school %>%
  filter(fund_gov > 80 & public_private == "private") %>%
  nrow()
# Check for 'public' when fund_gov < 50
inconsistent_public_n <- school %>%
  filter(fund_gov < 50 & public_private == "public") %>%
  nrow()

consistency_df <- data.frame(
  "Check" = c(
    "'private' with fund_gov > 80",
    "'public' with fund_gov < 50"
  ),
  "Number of inconsistent cases" = c(
    inconsistent_private_n,
    inconsistent_public_n
  )
)

knitr::kable(
  consistency_df,
  caption = "Number of Schools with Inconsistent 'public_private' Label and Government Funding",
  align = "lc",
  booktabs = TRUE
)



## ----PISA-data-cleaning-average-performance, -----------------------------------


# Create an average performance score for each student
student <- student %>%
  mutate(avg_score = rowMeans(select(., math, read, science), na.rm = TRUE))



## ----PISA-data-cleaning-sampling-weights, --------------------------------------


# Weighted mean of average scores
weighted.mean(student$avg_score, student$stu_wgt, na.rm = TRUE)



## ----PISA-data-cleaning-country-code-correction-------------------------------


# Harmonize case before recoding
student <- student %>% mutate(country = toupper(country))
school  <- school  %>% mutate(country = toupper(country))

# Always convert to character and capitalize before recoding
student <- student %>%
  mutate(
    country = as.character(country),
    country = toupper(country)
  )
school <- school %>%
  mutate(
    country = as.character(country),
    country = toupper(country)
  )

country_recode <- c(
  "TAP" = "TWN",
  "QES" = "ESP",
  "KSV" = "KOS",
  "QAZ" = "AZE",
  "QCI" = "CHN",
  "QCN" = "CHN",
  "QCH" = "CHN",
  "QRT" = "RUS",
  "YUG" = "BIH",
  "QUR" = "UKR",
  "QTN" = "IND",
  "QVE" = "VEN",
  "QMR" = "RUS",
  "QUE" = "USA",
  "QRS" = "RUS",
  "QUC" = "USA",
  "QAR" = "ARG",
  "QHP" = "IND",
  "QUD" = "PRI",
  "ROM" = "ROU"
)

# For the student dataset
student <- student %>%
  mutate(country = recode(country, !!!country_recode))

# For the school dataset
school <- school %>%
  mutate(country = recode(country, !!!country_recode))

# Check the remaining codes
#print("Remaining country codes in student :")
#print(sort(unique(student$country)))
#print("Remaining country codes in school :")
#print(sort(unique(school$country)))


# Codes present in the datasets but absent from the recoding
#setdiff(unique(student$country), country_recode)
#setdiff(unique(school$country), country_recode)



## ----PISA-data-cleaning-joining-region-income-and-student---------------------


# Join the region_income and student database

student <- student %>%
  left_join(region_income %>% select(country, ilo_subregion, country_name, world_bank_income_group), by = "country")%>%
  clean_names()

# Join the region_income and school database
school <- school %>%
  left_join(region_income %>% select(country, ilo_subregion, country_name, world_bank_income_group), by = "country")%>%
  clean_names()

# Verify the regions are correct (if blank all the changes were made)
student %>%
  filter(is.na(ilo_subregion)) %>%
  count(country) %>%
  arrange(desc(n))




## ----PISA-data-cleaning-eda-average-score-student-----------------------------


# Plot the distribution of the PISA scores
ggplot(student, aes(x = avg_score)) +
  geom_histogram(bins = 30, fill = "#a4c6f2", color = "white", width = 0.8) +
  theme_minimal() +
  ggtitle("Distribution of the average score  of math, read and science in PISA the students are expressed in millions") +
  scale_y_continuous(
    labels = label_number(scale = 1e-6, accuracy = 0.1, suffix = "M")
  )



## ----PISA-data-cleaning-eda-gender-student------------------------------------


# Plot the gender distribution

ggplot(student, aes(x = gender)) + geom_bar(fill = "#a4c6f2") + theme_minimal() + ggtitle("Gender distribution")



## ----PISA-data-cleaning-students-participation ------------------------------------


# Plot the student participation distribution

students_by_country <- student %>%
  group_by(country_name) %>%
  summarise(n_students = n_distinct(student_id)) %>%
  arrange(desc(n_students))

ggplot(students_by_country, aes(x = reorder(country_name, n_students), y = n_students)) +
  geom_bar(stat = "identity", fill = "#a4c6f2", color = "white", width = 0.8) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Number of students who participated in PISA by country",
    x = "Country",
    y = "Number of students"
  ) +
  scale_y_continuous(labels = label_number(scale = 1e-3, accuracy = 0.1, suffix = "K")) +
  coord_flip() + 
  theme(
    axis.text.y = element_text(size = 7) 
  )



## ----PISA-data-cleaning-schools-participation------------------------------------


# Plot the student participation distribution

schools_by_country <- student %>%
  group_by(country_name) %>%
  summarise(n_schools = n_distinct(school_id)) %>%
  arrange(desc(n_schools)) %>%
  mutate(country_name = str_trunc(country_name, 22))  

# Graph
ggplot(schools_by_country, aes(x = reorder(country_name, n_schools), y = n_schools)) +
  geom_bar(stat = "identity", fill = "#a4c6f2", color = "white", width = 0.8) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Number of schools participating in PISA by country",
    x = "Country",
    y = "Number of schools"
  ) +
  scale_y_continuous(labels = label_number(scale = 1, accuracy = 1)) +
  coord_flip() +
  theme(
    axis.text.y = element_text(size = 7)
  )


## ----PISA-data-exploration, message=FALSE------------------------------------


# Weighted Mean of Average Student Score by Year and Subregion: year, ilo_subregion, score, stu_wgt

weighted_means <- student %>%
  group_by(year, ilo_subregion) %>%
  summarise(
    weighted_mean = weighted.mean(avg_score, stu_wgt, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) 

ggplot(weighted_means, aes(x = year, y = weighted_mean, color = ilo_subregion, group = ilo_subregion)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Weighted Mean of Average Student Score by Year and Subregion",
    x = "Year",
    y = "Weighted Mean Average Score",
    color = "Subregion"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")



## ----PISA-data-exploration-weighted-mean-region-table ------------------------------------


# 1. Filter for 2009 and 2022 only
wm_09_22 <- weighted_means %>%
  filter(year %in% c(2009, 2022))

# 2. Keep only subregions with both years available
regions_both_years <- wm_09_22 %>%
  group_by(ilo_subregion) %>%
  filter(n() == 2) %>%
  pull(ilo_subregion) %>%
  unique()

wm_09_22 <- wm_09_22 %>%
  filter(ilo_subregion %in% regions_both_years)

# 3. Convert to wide format (one column per year)
wm_wide <- wm_09_22 %>%
  select(ilo_subregion, year, weighted_mean) %>%
  pivot_wider(
    names_from = year,
    values_from = weighted_mean,
    names_prefix = "score_"
  )

# 4. Round the scores to integers and calculate percent change
wm_wide <- wm_wide %>%
  mutate(
    score_2009 = round(score_2009, 0),
    score_2022 = round(score_2022, 0),
    variation_perc = round((score_2022 - score_2009) / score_2009 * 100, 1)
  ) %>%
  arrange(desc(variation_perc))

# 5. Rename columns for readability, using multi-line headers

if (knitr::is_latex_output()) {
  # Pour PDF/LaTeX
  colnames(wm_wide) <- c(
    "Region",
    "Weighted Mean 2009",
    "Weighted Mean 2022",
    "Change (%)"
  )
  wm_wide %>%
    kbl(
      caption = "Change in Weighted Mean Average Score by Region (2009–2022)",
      align = c("l", "c", "c", "c"),
      format = "latex",
      booktabs = TRUE,
      escape = TRUE
    ) %>%
    kable_styling(
      latex_options = c("striped", "hold_position"),
      font_size = 9
    )}




## ----PISA-data-exploration-classification -countries-PISA-world-bank, ------------------------------------

student <- student %>%
  clean_names() %>%
  select(-starts_with("world_bank_income_group_"))

# Create and store the summary table in a variable
wm_income <- student %>%
  filter(!is.na(world_bank_income_group)) %>%
  group_by(world_bank_income_group) %>%
  summarise(
    country_count = n_distinct(country),
    countries = paste(sort(unique(country)), collapse = ", ")
  ) %>%
  arrange(match(world_bank_income_group, c("Low income", "Lower-middle income", "Upper-middle income", "High income")))

# List all unique World Bank countries by income group
wb_summary <- region_income %>%
  filter(!is.na(world_bank_income_group)) %>%       # Remove entries with missing income group
  group_by(world_bank_income_group) %>%              # Group by income group
  summarise(
    wb_country_count = n_distinct(country),          # Count number of unique countries
    wb_countries = paste(sort(unique(country)), collapse = ", ")  # #List countries alphabetically
  )

# The number of PISA countries by income group is already calculated in wm_income
# Need to rename columns to match for joining
pisa_summary <- wm_income %>%
  rename(
    world_bank_income_group = world_bank_income_group,    
    pisa_country_count = country_count,                   
    pisa_countries = countries                            
  )

# Join both summaries on income group
coverage_summary <- wb_summary %>%
  left_join(pisa_summary, by = "world_bank_income_group") %>%    # Merge on income group
  mutate(
    # If there are no PISA countries for an income group, set count to 0
    pisa_country_count = ifelse(is.na(pisa_country_count), 0, pisa_country_count),
    # Calculate PISA coverage percentage in this income group
    coverage_pct = round(100 * pisa_country_count / wb_country_count, 1)
  )

# Prepare the table for display (select and rename columns)
coverage_summary_final <- coverage_summary %>%
  select(
    "Income group" = world_bank_income_group,
    "WB countries" = wb_country_count,
    "PISA countries" = pisa_country_count,
    "Coverage (%)" = coverage_pct,
 #   "PISA country list" = pisa_countries,
 #  "WB Country list" = wb_countries
  )

#Add an ordering variable, then arrange by that variable
coverage_summary_final <- coverage_summary_final %>%
  mutate(
    income_order = match(`Income group`, c(
      "Low income", "Lower-middle income", "Upper-middle income", "High income"
    ))
  ) %>%
  arrange(income_order) %>%
  select(-income_order)  # remove the helper column

# Display the final summary table with kable and custom styling
coverage_summary_final %>%
  kbl(
    caption = "PISA Country Coverage by World Bank Income Group",
    align = c("l", "c", "c", "c"),
    booktabs = TRUE
  ) %>%
  kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed")
  ) %>%
  row_spec(
    0,
    bold = TRUE,
    background = "#dbeafe",
    color = "#000000"
  ) %>%
  footnote(
    general = "WB Countries: World Bank Countries classification",
    general_title = "Note:",
    footnote_as_chunk = FALSE
  )


## ----PISA-data-exploration-escs-grades-quartiles ------------------------------------



# 1. Calculate the quartiles for every country and every year
escs_quartiles <- student %>%
  filter(!is.na(escs), !is.na(country), !is.na(year)) %>%
  group_by(country, year) %>%
  summarise(
    q25 = quantile(escs, 0.25, na.rm = TRUE),
    q50 = quantile(escs, 0.50, na.rm = TRUE),
    q75 = quantile(escs, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Join these quantiles to the original student dataset
student_with_quartiles <- student %>%
  left_join(escs_quartiles, by = c("country", "year")) %>%
  mutate(
    escs_quartile = case_when(
      escs <= q25 ~ "Q1 (Bottom 25%)",
      escs > q25 & escs <= q50 ~ "Q2 (25-50%)",
      escs > q50 & escs <= q75 ~ "Q3 (50-75%)",
      escs > q75 ~ "Q4 (Top 25%)",
      TRUE ~ NA_character_
    )
  )
# Now 'student_with_quantiles' contains a 'group' variable for all available years/countries

# Calculate quartiles for all variables
quartiles_all_vars <- student %>%
  filter(!is.na(country), !is.na(year)) %>%
  group_by(country, year) %>%
  summarise(
    escs_q25 = quantile(escs, 0.25, na.rm = TRUE),
    escs_q50 = quantile(escs, 0.50, na.rm = TRUE),
    escs_q75 = quantile(escs, 0.75, na.rm = TRUE),
    math_q25 = quantile(math, 0.25, na.rm = TRUE),
    math_q50 = quantile(math, 0.50, na.rm = TRUE),
    math_q75 = quantile(math, 0.75, na.rm = TRUE),
    read_q25 = quantile(read, 0.25, na.rm = TRUE),
    read_q50 = quantile(read, 0.50, na.rm = TRUE),
    read_q75 = quantile(read, 0.75, na.rm = TRUE),
    science_q25 = quantile(science, 0.25, na.rm = TRUE),
    science_q50 = quantile(science, 0.50, na.rm = TRUE),
    science_q75 = quantile(science, 0.75, na.rm = TRUE),
    avg_score_q25 = quantile(avg_score, 0.25, na.rm = TRUE),
    avg_score_q50 = quantile(avg_score, 0.50, na.rm = TRUE),
    avg_score_q75 = quantile(avg_score, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# Add the quartile bounds to each student
student_with_all_quartiles <- student %>%
  left_join(quartiles_all_vars, by = c("country", "year"))

#Assign the quartile to each student for each variable
student_with_all_quartiles <- student_with_all_quartiles %>%
  mutate(
    escs_quartile = case_when(
      escs <= escs_q25 ~ "Q1 (Bottom 25%)",
      escs > escs_q25 & escs <= escs_q50 ~ "Q2 (25-50%)",
      escs > escs_q50 & escs <= escs_q75 ~ "Q3 (50-75%)",
      escs > escs_q75 ~ "Q4 (Top 25%)",
      TRUE ~ NA_character_
    ),
    math_quartile = case_when(
      math <= math_q25 ~ "Q1 (Bottom 25%)",
      math > math_q25 & math <= math_q50 ~ "Q2 (25-50%)",
      math > math_q50 & math <= math_q75 ~ "Q3 (50-75%)",
      math > math_q75 ~ "Q4 (Top 25%)",
      TRUE ~ NA_character_
    ),
    read_quartile = case_when(
      read <= read_q25 ~ "Q1 (Bottom 25%)",
      read > read_q25 & read <= read_q50 ~ "Q2 (25-50%)",
      read > read_q50 & read <= read_q75 ~ "Q3 (50-75%)",
      read > read_q75 ~ "Q4 (Top 25%)",
      TRUE ~ NA_character_
    ),
    science_quartile = case_when(
      science <= science_q25 ~ "Q1 (Bottom 25%)",
      science > science_q25 & science <= science_q50 ~ "Q2 (25-50%)",
      science > science_q50 & science <= science_q75 ~ "Q3 (50-75%)",
      science > science_q75 ~ "Q4 (Top 25%)",
      TRUE ~ NA_character_
    ),
    avg_score_quartile = case_when(
      avg_score <= avg_score_q25 ~ "Q1 (Bottom 25%)",
      avg_score > avg_score_q25 & avg_score <= avg_score_q50 ~ "Q2 (25-50%)",
      avg_score > avg_score_q50 & avg_score <= avg_score_q75 ~ "Q3 (50-75%)",
      avg_score > avg_score_q75 ~ "Q4 (Top 25%)",
      TRUE ~ NA_character_
    )
  )



## ----PISA-data-exploration-bottom-escs-top-grades-quartiles ------------------------------------


# 1. Calculate proportions
result_proportion <- student_with_all_quartiles %>%
  filter(
    !is.na(world_bank_income_group),
    !is.na(year),
    !is.na(escs_quartile),
    !is.na(avg_score_quartile),
    year != 2000
  ) %>%
  group_by(year, world_bank_income_group) %>%
  summarise(
    n_total = n(),
    n_bottom_escs_top_avg = sum(
      escs_quartile == "Q1 (Bottom 25%)" &
      avg_score_quartile == "Q4 (Top 25%)",
      na.rm = TRUE
    ),
    prop_bottom_escs_top_avg = round(100 * n_bottom_escs_top_avg / n_total, 2),
    .groups = "drop"
  ) %>%
  left_join(
    student_with_all_quartiles %>%
      filter(
        !is.na(world_bank_income_group),
        !is.na(year),
        avg_score_quartile == "Q4 (Top 25%)"
      ) %>%
      group_by(year, world_bank_income_group) %>%
      summarise(
        top25_mean = round(mean(avg_score, na.rm = TRUE), 0),
        .groups = "drop"
      ),
    by = c("year", "world_bank_income_group")
  ) %>%
  mutate(
    n_total_fmt = format(n_total, big.mark = " "),
    n_bottom_fmt = format(n_bottom_escs_top_avg, big.mark = " ")
  ) %>%
  rename(
    Year = year,
    IncomeGroup = world_bank_income_group,
    ProportionPercent = prop_bottom_escs_top_avg,
    Top25MeanPISA = top25_mean
  )

# 2. Create the table
kbl_table <- result_proportion %>%
  select(
    Year, 
    IncomeGroup,
    TotalStudents = n_total_fmt,
    Bottom25ESCSTop25PISA = n_bottom_fmt,
    ProportionPercent,
    Top25MeanPISA
  )

# 3. Define column names 
colnames_pretty_safe <- c(
  "Year",
  "Income Group",
  "Total Students", 
  "Students BottomESCS TopPISA",
  "Proportion Perc.",
  "Mean Score Top Quartile"
)

kbl_table %>%
  kbl(
    caption = "Educational Mobility: ESCS Bottom Quartile to Top Quartile PISA",
    align = c("l", "l", "c", "c", "c", "c"),
    escape = FALSE,
    booktabs = TRUE,
    col.names = colnames_pretty_safe  
  ) %>%
  kable_styling(
    latex_options = c("striped", "hold_position"),
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(4, width = "3.5cm") %>%
  column_spec(6, width = "3cm")



## ----PISA-data-exploration-bottom-escs-top-grades-quartiles-graph ------------------------------------


# Plot


ggplot(result_proportion %>% filter(Year != 2000), 
       aes(x = Year, y = ProportionPercent, color = IncomeGroup, group = IncomeGroup)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Educational Mobility: % of Students in Bottom 25% ESCS & Top 25% PISA score",
    x = "Year",
    y = "Proportion (%)",
    color = "World Bank Income Group"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+
  theme(legend.position = "bottom")



## ----PISA-data-exploration-escs-low-middle-income-------------------------------------------------


 
# 1. Data Preparation: Clean Codes and Names Everywhere
 
region_income <- region_income %>%
  mutate(
    country = toupper(trimws(as.character(country))),
    country_name = trimws(as.character(country_name))
  )

student_with_all_quartiles <- student_with_all_quartiles %>%
  mutate(
    country = toupper(trimws(as.character(country)))
  )

# 2. Compute Weighted Means for Each Social Group
 
means_lm <- student_with_all_quartiles %>%
  filter(
    world_bank_income_group == "Lower-middle income",
    escs_quartile %in% c("Q1 (Bottom 25%)", "Q4 (Top 25%)"),
    !is.na(avg_score), !is.na(stu_wgt)
  ) %>%
  group_by(country, year, escs_quartile) %>%
  summarise(
    weighted_mean = weighted.mean(avg_score, stu_wgt, na.rm = TRUE),
    .groups = "drop"
  )

segments_lm <- means_lm %>%
  pivot_wider(
    id_cols = c(country, year),
    names_from = escs_quartile,
    values_from = weighted_mean
  ) %>%
  filter(
    !is.na(country),
    !is.na(`Q1 (Bottom 25%)`),
    !is.na(`Q4 (Top 25%)`)
  ) %>%
  left_join(region_income %>% select(country, country_name), by = "country") %>%
  mutate(
    country_name = ifelse(is.na(country_name), country, country_name)
  ) %>%
  rename(
    `Bottom 25%` = `Q1 (Bottom 25%)`,
    `Top 25%` = `Q4 (Top 25%)`
  ) %>%
  mutate(across(everything(), as.character)) # Set all columns to character

 
# 3. Calculate Average for Lower-middle Income Group
 
avg_segments <- segments_lm %>%
  group_by(year) %>%
  summarise(
    `Bottom 25%` = mean(as.numeric(`Bottom 25%`), na.rm = TRUE),
    `Top 25%` = mean(as.numeric(`Top 25%`), na.rm = TRUE)
  ) %>%
  mutate(
    country = "Average (Lower-middle income)",
    country_name = "Average (Lower-middle income)"
  ) %>%
  mutate(across(everything(), as.character))

# 4. Calculate Average for All Countries
 
means_all <- student_with_all_quartiles %>%
  filter(
    year %in% c(2003, 2018, 2022),
    escs_quartile %in% c("Q1 (Bottom 25%)", "Q4 (Top 25%)"),
    !is.na(avg_score), !is.na(stu_wgt)
  ) %>%
  group_by(country, year, escs_quartile) %>%
  summarise(
    weighted_mean = weighted.mean(avg_score, stu_wgt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(country, year),
    names_from = escs_quartile,
    values_from = weighted_mean
  ) %>%
  filter(
    !is.na(country),
    !is.na(`Q1 (Bottom 25%)`),
    !is.na(`Q4 (Top 25%)`)
  ) %>%
  group_by(year) %>%
  summarise(
    `Bottom 25%` = mean(as.numeric(`Q1 (Bottom 25%)`), na.rm = TRUE),
    `Top 25%` = mean(as.numeric(`Q4 (Top 25%)`), na.rm = TRUE)
  ) %>%
  mutate(
    country = "Average (All countries)",
    country_name = "Average (All countries)"
  ) %>%
  mutate(across(everything(), as.character))
 
# 5. Harmonize and Check Columns Before Merging
 
columns_needed <- c("country", "year", "Bottom 25%", "Top 25%", "country_name")
segments_lm <- segments_lm[, columns_needed]
avg_segments <- avg_segments[, columns_needed]
means_all <- means_all[, columns_needed]

 
# 6. Bind All Dataframes for Plotting
 
segments_lm_plot <- bind_rows(segments_lm, avg_segments, means_all)

 
# 7. Final Correction: Fill NA country_name with Code if Needed
 
segments_lm_plot$country_name <- as.character(segments_lm_plot$country_name)
segments_lm_plot <- segments_lm_plot %>%
  mutate(
    country_name = ifelse(is.na(country_name) | country_name == "NA", country, country_name)
  )

 
# 8. Diagnostic: List Countries with Missing Names
 
#Only if needed
#print(segments_lm_plot %>% filter(is.na(year) | is.na(country_name)))

 
# 9. Set Order for Plotting
 
countries_order_2022 <- segments_lm_plot %>%
  filter(year == "2022", !grepl("Average", country_name)) %>%
  arrange(as.numeric(`Bottom 25%`)) %>%
  pull(country_name) %>%
  unique()

all_specials <- c("Average (Lower-middle income)", "Average (All countries)")

other_countries <- segments_lm_plot %>%
  filter(!country_name %in% c(countries_order_2022, all_specials)) %>%
  pull(country_name) %>%
  unique()

country_levels <- c(countries_order_2022, other_countries, all_specials)

segments_lm_plot$country_name <- factor(segments_lm_plot$country_name, levels = country_levels)

segments_lm_plot <- segments_lm_plot %>%
  filter(year %in% c(2003, 2018, 2022))

 
# 10. Plot Results
 

ggplot(segments_lm_plot, aes(y = country_name)) +
  geom_segment(
    aes(x = as.numeric(`Bottom 25%`), xend = as.numeric(`Top 25%`), yend = country_name, color = as.factor(year)),
    size = 0.7,
    data = segments_lm_plot %>% filter(!is.na(`Bottom 25%`), !is.na(`Top 25%`))
  ) +
  geom_point(
    aes(x = as.numeric(`Bottom 25%`), color = as.factor(year)),
    size = 3, shape = 16
  ) +
  geom_point(
    aes(x = as.numeric(`Top 25%`), color = as.factor(year)),
    size = 3, shape = 18
  ) +
  labs(
    title = "Change in Mean Scores by Social Background \n(Lower-middle income countries)",
    x = "Weighted Mean Score",
    y = NULL,
    color = "Year"
  ) +
  scale_color_manual(
    values = c("2003" = "#FFD700", "2018" = "#23395d", "2022" = "#F577e0"),
    labels = c("2003" = "Year 2003", "2018" = "Year 2018", "2022" = "Year 2022")
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

 
# 11. Debugging: List any unmatched country codes
 

missing_codes <- anti_join(
  segments_lm %>% select(country) %>% distinct(),
  region_income %>% select(country) %>% distinct(),
  by = "country"
)
#print(missing_codes)  #print if problems encountered



## ----PISA-data-exploration-escs-upper-lower-income-table ----------------------------------------------


# 1. Prepare the summary data (weighted means per country/year/quartile) 

means_by_quartile <- student_with_all_quartiles %>%
  filter(
    world_bank_income_group == "Lower-middle income",
    escs_quartile %in% c("Q1 (Bottom 25%)", "Q4 (Top 25%)"),
    !is.na(avg_score), !is.na(stu_wgt),
    year %in% c(2003, 2018, 2022)
  ) %>%
  group_by(country, year, escs_quartile) %>%
  summarise(
    mean_score = weighted.mean(avg_score, stu_wgt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = escs_quartile,
    values_from = mean_score
  ) %>%
  filter(!is.na(`Q1 (Bottom 25%)`), !is.na(`Q4 (Top 25%)`))

# 2. For each country, pick the most recent year available ---
latest_year <- means_by_quartile %>%
  group_by(country) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  ungroup()

# 3. Add country names ---
latest_year <- latest_year %>%
  left_join(region_income %>% select(country, country_name), by = "country") %>%
  mutate(
    country_name = ifelse(is.na(country_name), country, country_name)
  )

# 4. Compute the difference and format columns ---
summary_table <- latest_year %>%
  transmute(
    Country = country_name,
    `Q1 Mean Score (Bottom 25% ESCS)` = round(`Q1 (Bottom 25%)`, 0),
    `Q4 Mean Score (Top 25% ESCS)` = round(`Q4 (Top 25%)`, 0),
    `Difference (Q4 - Q1)` = round(`Q4 (Top 25%)` - `Q1 (Bottom 25%)`, 0),
    Year = year
  ) %>%
  arrange(desc(`Difference (Q4 - Q1)`))

# 5. Display the table
knitr::kable(
  summary_table,
  caption = "Difference in Mean Scores between Top and Bottom ESCS Quartiles (Most Recent Year, Lower-middle Income Countries)",
  align = "lcccl"
)



## ----PISA-data-exploration-escs-upper-middle-income ---------------------------------------------

 
# 1. Data Preparation: Clean Codes and Names Everywhere
 
region_income <- region_income %>%
  mutate(
    country = toupper(trimws(as.character(country))),
    country_name = trimws(as.character(country_name))
  )

student_with_all_quartiles <- student_with_all_quartiles %>%
  mutate(
    country = toupper(trimws(as.character(country)))
  )

 
# 2. Compute Weighted Means for Each Social Group
 
means_lm <- student_with_all_quartiles %>%
  filter(
    world_bank_income_group == "Upper-middle income",
    escs_quartile %in% c("Q1 (Bottom 25%)", "Q4 (Top 25%)"),
    !is.na(avg_score), !is.na(stu_wgt)
  ) %>%
  group_by(country, year, escs_quartile) %>%
  summarise(
    weighted_mean = weighted.mean(avg_score, stu_wgt, na.rm = TRUE),
    .groups = "drop"
  )

segments_lm <- means_lm %>%
  pivot_wider(
    id_cols = c(country, year),
    names_from = escs_quartile,
    values_from = weighted_mean
  ) %>%
  filter(
    !is.na(country),
    !is.na(`Q1 (Bottom 25%)`),
    !is.na(`Q4 (Top 25%)`)
  ) %>%
  left_join(region_income %>% select(country, country_name), by = "country") %>%
  mutate(
    country_name = ifelse(is.na(country_name), country, country_name)
  ) %>%
  rename(
    `Bottom 25%` = `Q1 (Bottom 25%)`,
    `Top 25%` = `Q4 (Top 25%)`
  ) %>%
  mutate(across(everything(), as.character)) # Set all columns to character

 
# 3. Calculate Average for Upper-middle Income Group
 
avg_segments <- segments_lm %>%
  group_by(year) %>%
  summarise(
    `Bottom 25%` = mean(as.numeric(`Bottom 25%`), na.rm = TRUE),
    `Top 25%` = mean(as.numeric(`Top 25%`), na.rm = TRUE)
  ) %>%
  mutate(
    country = "Average (Upper-middle income)",
    country_name = "Average (Upper-middle income)"
  ) %>%
  mutate(across(everything(), as.character))

 
# 4. Calculate Average for All Countries
 
means_all <- student_with_all_quartiles %>%
  filter(
    year %in% c(2003, 2018, 2022),
    escs_quartile %in% c("Q1 (Bottom 25%)", "Q4 (Top 25%)"),
    !is.na(avg_score), !is.na(stu_wgt)
  ) %>%
  group_by(country, year, escs_quartile) %>%
  summarise(
    weighted_mean = weighted.mean(avg_score, stu_wgt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(country, year),
    names_from = escs_quartile,
    values_from = weighted_mean
  ) %>%
  filter(
    !is.na(country),
    !is.na(`Q1 (Bottom 25%)`),
    !is.na(`Q4 (Top 25%)`)
  ) %>%
  group_by(year) %>%
  summarise(
    `Bottom 25%` = mean(as.numeric(`Q1 (Bottom 25%)`), na.rm = TRUE),
    `Top 25%` = mean(as.numeric(`Q4 (Top 25%)`), na.rm = TRUE)
  ) %>%
  mutate(
    country = "Average (All countries)",
    country_name = "Average (All countries)"
  ) %>%
  mutate(across(everything(), as.character))

 
# 5. Harmonize and Check Columns Before Merging
 
columns_needed <- c("country", "year", "Bottom 25%", "Top 25%", "country_name")
segments_lm <- segments_lm[, columns_needed]
avg_segments <- avg_segments[, columns_needed]
means_all <- means_all[, columns_needed]

 
# 6. Bind All Dataframes for Plotting
 
segments_lm_plot <- bind_rows(segments_lm, avg_segments, means_all)

 
# 7. Final Correction: Fill NA country_name with Code if Needed
 
segments_lm_plot$country_name <- as.character(segments_lm_plot$country_name)
segments_lm_plot <- segments_lm_plot %>%
  mutate(
    country_name = ifelse(is.na(country_name) | country_name == "NA", country, country_name)
  )

 
# 8. Diagnostic: List Countries with Missing Names
 
#Only if needed
#print(segments_lm_plot %>% filter(is.na(year) | is.na(country_name)))

 
# 9. Set Order for Plotting
 
countries_order_2022 <- segments_lm_plot %>%
  filter(year == "2022", !grepl("Average", country_name)) %>%
  arrange(as.numeric(`Bottom 25%`)) %>%
  pull(country_name) %>%
  unique()

all_specials <- c("Average (Upper-middle income)", "Average (All countries)")

other_countries <- segments_lm_plot %>%
  filter(!country_name %in% c(countries_order_2022, all_specials)) %>%
  pull(country_name) %>%
  unique()

country_levels <- c(countries_order_2022, other_countries, all_specials)

segments_lm_plot$country_name <- factor(segments_lm_plot$country_name, levels = country_levels)

segments_lm_plot <- segments_lm_plot %>%
  filter(year %in% c(2003, 2018, 2022))

 
# 10. Plot Results
 

ggplot(segments_lm_plot, aes(y = country_name)) +
  geom_segment(
    aes(x = as.numeric(`Bottom 25%`), xend = as.numeric(`Top 25%`), yend = country_name, color = as.factor(year)),
    size = 0.7,
    data = segments_lm_plot %>% filter(!is.na(`Bottom 25%`), !is.na(`Top 25%`))
  ) +
  geom_point(
    aes(x = as.numeric(`Bottom 25%`), color = as.factor(year)),
    size = 3, shape = 16
  ) +
  geom_point(
    aes(x = as.numeric(`Top 25%`), color = as.factor(year)),
    size = 3, shape = 18
  ) +
  labs(
    title = "Change in Mean Scores by Social Background \n(Upper-middle income countries)",
    x = "Weighted Mean Score",
    y = NULL,
    color = "Year"
  ) +
  scale_color_manual(
    values = c("2003" = "#FFD700", "2018" = "#23395d", "2022" = "#F577e0"),
    labels = c("2003" = "Year 2003", "2018" = "Year 2018", "2022" = "Year 2022")
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

 
# 11. Debugging: List any unmatched country codes
 

missing_codes <- anti_join(
  segments_lm %>% select(country) %>% distinct(),
  region_income %>% select(country) %>% distinct(),
  by = "country"
)
#print(missing_codes)  #print if problems encountered


## ----PISA-data-exploration-escs-upper-middle-income-table--------------------------------------------


# 1. Prepare the summary data (weighted means per country/year/quartile) 

means_by_quartile <- student_with_all_quartiles %>%
  filter(
    world_bank_income_group == "Upper-middle income",
    escs_quartile %in% c("Q1 (Bottom 25%)", "Q4 (Top 25%)"),
    !is.na(avg_score), !is.na(stu_wgt),
    year %in% c(2003, 2018, 2022)
  ) %>%
  group_by(country, year, escs_quartile) %>%
  summarise(
    mean_score = weighted.mean(avg_score, stu_wgt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = escs_quartile,
    values_from = mean_score
  ) %>%
  filter(!is.na(`Q1 (Bottom 25%)`), !is.na(`Q4 (Top 25%)`))

# 2. For each country, pick the most recent year available ---
latest_year <- means_by_quartile %>%
  group_by(country) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  ungroup()

# 3. Add country names 
latest_year <- latest_year %>%
  left_join(region_income %>% select(country, country_name), by = "country") %>%
  mutate(
    country_name = ifelse(is.na(country_name), country, country_name)
  )

# 4. Compute the difference and format columns 
summary_table <- latest_year %>%
  transmute(
    Country = country_name,
    `Q1 Mean Score (Bottom 25% ESCS)` = round(`Q1 (Bottom 25%)`, 0),
    `Q4 Mean Score (Top 25% ESCS)` = round(`Q4 (Top 25%)`, 0),
    `Difference (Q4 - Q1)` = round(`Q4 (Top 25%)` - `Q1 (Bottom 25%)`, 0),
    Year = year
  ) %>%
  arrange(desc(`Difference (Q4 - Q1)`))

# 5. Display the table 
knitr::kable(
  summary_table,
  caption = "Difference in Mean Scores between Top and Bottom ESCS Quartiles (Most Recent Year, Upper-middle Income Countries)",
  align = "lcccl"
)




## ----PISA-data-exploration-escs-High-income -----------------------------------------


 
# 1. Data Preparation: Clean Codes and Names Everywhere
 
region_income <- region_income %>%
  mutate(
    country = toupper(trimws(as.character(country))),
    country_name = trimws(as.character(country_name))
  )

student_with_all_quartiles <- student_with_all_quartiles %>%
  mutate(
    country = toupper(trimws(as.character(country)))
  )

 
# 2. Compute Weighted Means for Each Social Group
 
means_lm <- student_with_all_quartiles %>%
  filter(
    world_bank_income_group == "High income",
    escs_quartile %in% c("Q1 (Bottom 25%)", "Q4 (Top 25%)"),
    !is.na(avg_score), !is.na(stu_wgt)
  ) %>%
  group_by(country, year, escs_quartile) %>%
  summarise(
    weighted_mean = weighted.mean(avg_score, stu_wgt, na.rm = TRUE),
    .groups = "drop"
  )

segments_lm <- means_lm %>%
  pivot_wider(
    id_cols = c(country, year),
    names_from = escs_quartile,
    values_from = weighted_mean
  ) %>%
  filter(
    !is.na(country),
    !is.na(`Q1 (Bottom 25%)`),
    !is.na(`Q4 (Top 25%)`)
  ) %>%
  left_join(region_income %>% select(country, country_name), by = "country") %>%
  mutate(
    country_name = ifelse(is.na(country_name), country, country_name)
  ) %>%
  rename(
    `Bottom 25%` = `Q1 (Bottom 25%)`,
    `Top 25%` = `Q4 (Top 25%)`
  ) %>%
  mutate(across(everything(), as.character)) # Set all columns to character

 
# 3. Calculate Average for High Income Group
 
avg_segments <- segments_lm %>%
  group_by(year) %>%
  summarise(
    `Bottom 25%` = mean(as.numeric(`Bottom 25%`), na.rm = TRUE),
    `Top 25%` = mean(as.numeric(`Top 25%`), na.rm = TRUE)
  ) %>%
  mutate(
    country = "Average (High income)",
    country_name = "Average (High income)"
  ) %>%
  mutate(across(everything(), as.character))

 
# 4. Calculate Average for All Countries
 
means_all <- student_with_all_quartiles %>%
  filter(
    year %in% c(2003, 2018, 2022),
    escs_quartile %in% c("Q1 (Bottom 25%)", "Q4 (Top 25%)"),
    !is.na(avg_score), !is.na(stu_wgt)
  ) %>%
  group_by(country, year, escs_quartile) %>%
  summarise(
    weighted_mean = weighted.mean(avg_score, stu_wgt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(country, year),
    names_from = escs_quartile,
    values_from = weighted_mean
  ) %>%
  filter(
    !is.na(country),
    !is.na(`Q1 (Bottom 25%)`),
    !is.na(`Q4 (Top 25%)`)
  ) %>%
  group_by(year) %>%
  summarise(
    `Bottom 25%` = mean(as.numeric(`Q1 (Bottom 25%)`), na.rm = TRUE),
    `Top 25%` = mean(as.numeric(`Q4 (Top 25%)`), na.rm = TRUE)
  ) %>%
  mutate(
    country = "Average (All countries)",
    country_name = "Average (All countries)"
  ) %>%
  mutate(across(everything(), as.character))

 
# 5. Harmonize and Check Columns Before Merging
 
columns_needed <- c("country", "year", "Bottom 25%", "Top 25%", "country_name")
segments_lm <- segments_lm[, columns_needed]
avg_segments <- avg_segments[, columns_needed]
means_all <- means_all[, columns_needed]

 
# 6. Bind All Dataframes for Plotting
 
segments_lm_plot <- bind_rows(segments_lm, avg_segments, means_all)

 
# 7. Final Correction: Fill NA country_name with Code if Needed
 
segments_lm_plot$country_name <- as.character(segments_lm_plot$country_name)
segments_lm_plot <- segments_lm_plot %>%
  mutate(
    country_name = ifelse(is.na(country_name) | country_name == "NA", country, country_name)
  )

 
# 8. Diagnostic: List Countries with Missing Names
 
#Only if needed
#print(segments_lm_plot %>% filter(is.na(year) | is.na(country_name)))

 
# 9. Set Order for Plotting
 
countries_order_2022 <- segments_lm_plot %>%
  filter(year == "2022", !grepl("Average", country_name)) %>%
  arrange(as.numeric(`Bottom 25%`)) %>%
  pull(country_name) %>%
  unique()

all_specials <- c("Average (High income)", "Average (All countries)")

other_countries <- segments_lm_plot %>%
  filter(!country_name %in% c(countries_order_2022, all_specials)) %>%
  pull(country_name) %>%
  unique()

country_levels <- c(countries_order_2022, other_countries, all_specials)

segments_lm_plot$country_name <- factor(segments_lm_plot$country_name, levels = country_levels)

segments_lm_plot <- segments_lm_plot %>%
  filter(year %in% c(2003, 2018, 2022))

 
# 10. Plot Results
 

ggplot(segments_lm_plot, aes(y = country_name)) +
  geom_segment(
    aes(x = as.numeric(`Bottom 25%`), xend = as.numeric(`Top 25%`), yend = country_name, color = as.factor(year)),
    size = 0.7,
    data = segments_lm_plot %>% filter(!is.na(`Bottom 25%`), !is.na(`Top 25%`))
  ) +
  geom_point(
    aes(x = as.numeric(`Bottom 25%`), color = as.factor(year)),
    size = 3, shape = 16
  ) +
  geom_point(
    aes(x = as.numeric(`Top 25%`), color = as.factor(year)),
    size = 3, shape = 18
  ) +
  labs(
    title = "Change in Mean Scores by Social Background \n(High income countries)",
    x = "Weighted Mean Score",
    y = NULL,
    color = "Year"
  ) +
  scale_color_manual(
    values = c("2003" = "#FFD700", "2018" = "#23395d", "2022" = "#F577e0"),
    labels = c("2003" = "Year 2003", "2018" = "Year 2018", "2022" = "Year 2022")
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

 
# 11. Debugging: List any unmatched country codes
 

missing_codes <- anti_join(
  segments_lm %>% select(country) %>% distinct(),
  region_income %>% select(country) %>% distinct(),
  by = "country"
)
#print(missing_codes)  #print if problems encountered


## ----PISA-data-exploration-escs-high-income-table----------------------------------------------


# 1. Prepare the summary data (weighted means per country/year/quartile) 

means_by_quartile <- student_with_all_quartiles %>%
  filter(
    world_bank_income_group == "High income",
    escs_quartile %in% c("Q1 (Bottom 25%)", "Q4 (Top 25%)"),
    !is.na(avg_score), !is.na(stu_wgt),
    year %in% c(2003, 2018, 2022)
  ) %>%
  group_by(country, year, escs_quartile) %>%
  summarise(
    mean_score = weighted.mean(avg_score, stu_wgt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = escs_quartile,
    values_from = mean_score
  ) %>%
  filter(!is.na(`Q1 (Bottom 25%)`), !is.na(`Q4 (Top 25%)`))

# 2. For each country, pick the most recent year available ---
latest_year <- means_by_quartile %>%
  group_by(country) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  ungroup()

# 3. Add country names ---
latest_year <- latest_year %>%
  left_join(region_income %>% select(country, country_name), by = "country") %>%
  mutate(
    country_name = ifelse(is.na(country_name), country, country_name)
  )

# 4. Compute the difference and format columns ---
summary_table <- latest_year %>%
  transmute(
    Country = country_name,
    `Q1 Mean Score (Bottom 25% ESCS)` = round(`Q1 (Bottom 25%)`, 0),
    `Q4 Mean Score (Top 25% ESCS)` = round(`Q4 (Top 25%)`, 0),
    `Difference (Q4 - Q1)` = round(`Q4 (Top 25%)` - `Q1 (Bottom 25%)`, 0),
    Year = year
  ) %>%
  arrange(desc(`Difference (Q4 - Q1)`))

# 5. Display the table ---
knitr::kable(
  summary_table,
  caption = "Difference in Mean Scores between Top and Bottom ESCS Quartiles \n illustrates the educational equity gap \n (Most Recent Year, High Income Countries)",
  align = "lcccl"
)




## ----PISA-data-exploration-correlation-percentage-gdp-education-pisa-lower-middle-income ------------------------------------


# 1. Prepare Education Expenditure Data (latest available up to 2022)

edu_exp_latest <- expenditure %>%
  filter(!is.na(percentage_education_on_gdp), year <= 2022) %>%
  group_by(country) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  select(country, expenditure_year = year, percentage_education_on_gdp)


# 2. Calculate Weighted Average Mean Score by Country-Year

mean_scores <- student %>%
  filter(
    !is.na(avg_score),
    !is.na(stu_wgt),
    !is.na(world_bank_income_group),
    world_bank_income_group == "Lower-middle income" # change as needed
  ) %>%
  group_by(country, year) %>%
  summarise(
    weighted_avg_score = weighted.mean(avg_score, stu_wgt, na.rm = TRUE),
    .groups = "drop"
  )


# 3. Merge Scores with Expenditure Data (latest available year)

# Get the latest available PISA score per country (up to 2022)
mean_scores_latest <- mean_scores %>%
  filter(year <= 2022) %>%
  group_by(country) %>%
  filter(year == max(year)) %>%
  ungroup()

# Merge the two datasets
scores_exp <- mean_scores_latest %>%
  left_join(edu_exp_latest, by = "country") %>%
  filter(!is.na(percentage_education_on_gdp))

# Add country names for better labels
scores_exp <- scores_exp %>%
  left_join(region_income %>% select(country, country_name), by = "country") %>%
  mutate(country_name = ifelse(is.na(country_name), country, country_name))


# 4. Plot: % Education Expenditure vs Weighted Average Mean Score


ggplot(scores_exp, aes(x = percentage_education_on_gdp, y = weighted_avg_score, label = country_name)) +
  geom_point(size = 2.5, color = "#23395d", alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, level = 0.8, color = "#F577e0", linetype = "dashed") +
  geom_text_repel(size = 2.7, max.overlaps = 15) +
  labs(
    title = "Education Spending (% of GDP)\nand Weighted Average Mean Score in PISA\n(Lower-middle Income Countries, latest year up to 2022)",
    x = "Education Spending (% of GDP) (latest available year up to 2022)",
    y = "Weighted Average Mean Score in PISA in 2022"
  ) +
  theme_minimal(base_size = 13)



## ----PISA-data-exploration-correlation-percentage-gdp-education-pisa-upper-middle-income-------------------------------------



# 1. Prepare Education Expenditure Data (latest available up to 2022)

edu_exp_latest <- expenditure %>%
  filter(!is.na(percentage_education_on_gdp), year <= 2022) %>%
  group_by(country) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  select(country, expenditure_year = year, percentage_education_on_gdp)


# 2. Calculate Weighted Average Mean Score by Country-Year

mean_scores <- student %>%
  filter(
    !is.na(avg_score),
    !is.na(stu_wgt),
    !is.na(world_bank_income_group),
    world_bank_income_group == "Upper-middle income" 
  ) %>%
  group_by(country, year) %>%
  summarise(
    weighted_avg_score = weighted.mean(avg_score, stu_wgt, na.rm = TRUE),
    .groups = "drop"
  )


# 3. Merge Scores with Expenditure Data (latest available year)

# Get the latest available PISA score per country (up to 2022)
mean_scores_latest <- mean_scores %>%
  filter(year <= 2022) %>%
  group_by(country) %>%
  filter(year == max(year)) %>%
  ungroup()

# Merge the two datasets
scores_exp <- mean_scores_latest %>%
  left_join(edu_exp_latest, by = "country") %>%
  filter(!is.na(percentage_education_on_gdp))

# Add country names for better labels
scores_exp <- scores_exp %>%
  left_join(region_income %>% select(country, country_name), by = "country") %>%
  mutate(country_name = ifelse(is.na(country_name), country, country_name))


# 4. Plot: % Education Expenditure vs Weighted Average Mean Score


ggplot(scores_exp, aes(x = percentage_education_on_gdp, y = weighted_avg_score, label = country_name)) +
  geom_point(size = 2.5, color = "#23395d", alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, level = 0.8, color = "#F577e0", linetype = "dashed") +
  geom_text_repel(size = 2.7, max.overlaps = 15) +
  labs(
    title = "Education Spending (% of GDP)\nand Weighted Average Mean Score in PISA\n(Upper-middle Income Countries, latest year up to 2022)",
    x = "Education Spending (% of GDP) (latest available year up to 2022)",
    y = "Weighted Average Mean Score in PISA in 2022"
  ) +
  theme_minimal(base_size = 13)



## ----PISA-data-exploration-correlation-percentage-gdp-education-pisa-high-income ------------------------------------



# 1. Prepare Education Expenditure Data (latest available up to 2022)

edu_exp_latest <- expenditure %>%
  filter(!is.na(percentage_education_on_gdp), year <= 2022) %>%
  group_by(country) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  select(country, expenditure_year = year, percentage_education_on_gdp)


# 2. Calculate Weighted Average Mean Score by Country-Year

mean_scores <- student %>%
  filter(
    !is.na(avg_score),
    !is.na(stu_wgt),
    !is.na(world_bank_income_group),
    world_bank_income_group == "High income" # change as needed
  ) %>%
  group_by(country, year) %>%
  summarise(
    weighted_avg_score = weighted.mean(avg_score, stu_wgt, na.rm = TRUE),
    .groups = "drop"
  )


# 3. Merge Scores with Expenditure Data (latest available year)
 
# Get the latest available PISA score per country (up to 2022)
mean_scores_latest <- mean_scores %>%
  filter(year <= 2022) %>%
  group_by(country) %>%
  filter(year == max(year)) %>%
  ungroup()

# Merge the two datasets
scores_exp <- mean_scores_latest %>%
  left_join(edu_exp_latest, by = "country") %>%
  filter(!is.na(percentage_education_on_gdp))

# Add country names for better labels
scores_exp <- scores_exp %>%
  left_join(region_income %>% select(country, country_name), by = "country") %>%
  mutate(country_name = ifelse(is.na(country_name), country, country_name))


# 4. Plot: % Education Expenditure vs Weighted Average Mean Score


ggplot(scores_exp, aes(x = percentage_education_on_gdp, y = weighted_avg_score, label = country_name)) +
  geom_point(size = 2.5, color = "#23395d", alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, level = 0.5, color = "#F577e0", linetype = "dashed") +
  geom_text_repel(size = 2.7, max.overlaps = 15) +
  labs(
    title = "Education Spending (% of GDP)\nand Weighted Average Mean Score in PISA\n(High Income Countries, latest year up to 2022)",
    x = "Education Spending (% of GDP) (latest available year up to 2022)",
    y = "Weighted Average Mean Score in PISA in 2022"
  ) +
  theme_minimal(base_size = 13)



## ----PISA-data-exploration-economic-prosperity-pisa-score ------------------------------------



# 1. Prepare the Weighted Average Mean Score for 2022

mean_scores_2022 <- student %>%
  filter(!is.na(avg_score), !is.na(stu_wgt), year == 2022) %>%
  group_by(country) %>%
  summarise(
    weighted_avg_score = weighted.mean(avg_score, stu_wgt, na.rm = TRUE),
    .groups = "drop"
  )


# 2. Prepare GDP per PPP Data

gdp_perppp <- gdp_perppp %>%
  rename(GDP_per_PPP_2022 = GDP_per_PPP_2022) %>%
  mutate(country = toupper(trimws(as.character(country))))


# 3. Merge the two dataframes

plot_data <- mean_scores_2022 %>%
  left_join(gdp_perppp %>% select(country, GDP_per_PPP_2022), by = "country") %>%
  left_join(region_income %>% select(country, country_name), by = "country") %>%
  mutate(country_name = ifelse(is.na(country_name), country, country_name))

# Filter out missing GDP values if needed
plot_data <- plot_data %>% filter(!is.na(GDP_per_PPP_2022))


# 4. Plot: Mean Score vs. GDP per PPP (Log scale)


ggplot(plot_data, aes(x = GDP_per_PPP_2022, y = weighted_avg_score, label = country_name)) +
  geom_point(size = 2.5, color = "#23395d", alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE, level = 0.8, color = "#F577e0", linetype = "dashed") +
  geom_text_repel(size = 2.8, max.overlaps = 15) +
  scale_x_log10(labels = scales::comma_format(accuracy = 1)) +  # Log scale, nice formatting
  labs(
    title = "Weighted Average Mean Score (2022) vs GDP per Capita (PPP, 2022)",
    x = "GDP per Capita (PPP, 2022, log scale)",
    y = "Weighted Average Mean Score (2022)"
  ) +
  theme_minimal(base_size = 13)


## ----PISA-data-exploration-student-teacher-ratio ------------------------------------


# 1. Prepare the Weighted Average Mean Score for 2022

mean_scores_2022 <- student %>%
  filter(!is.na(avg_score), !is.na(stu_wgt), year == 2022) %>%
  group_by(country) %>%
  summarise(
    weighted_avg_score = weighted.mean(avg_score, stu_wgt, na.rm = TRUE),
    .groups = "drop"
  )


# 2. Calculate the Average Student-Teacher Ratio (stratio) per Country in 2022

stratio_2022 <- school %>%
  filter(!is.na(stratio), !is.na(country), year == 2022) %>%
  group_by(country) %>%
  summarise(
    avg_stratio = mean(stratio, na.rm = TRUE),
    .groups = "drop"
  )


# 3. Merge the Two DataFrames

plot_data <- mean_scores_2022 %>%
  left_join(stratio_2022, by = "country") %>%
  left_join(region_income %>% select(country, country_name), by = "country") %>%
  mutate(country_name = ifelse(is.na(country_name), country, country_name))

plot_data <- plot_data %>% filter(!is.na(avg_stratio))


# 4. Plot: Weighted Mean Score vs. Student-Teacher Ratio


ggplot(plot_data, aes(x = avg_stratio, y = weighted_avg_score, label = country_name)) +
  geom_point(size = 2.5, color = "#23395d", alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE, level = 0.8, color = "#F577e0", linetype = "dashed") +
  geom_text_repel(size = 2.8, max.overlaps = 15) +
  labs(
    title = "Weighted Average Mean Score (2022) vs Student-Teacher Ratio (2022)",
    x = "Student-Teacher Ratio (2022)",
    y = "Weighted Average Mean Score (2022)"
  ) +
  theme_minimal(base_size = 13)


## ----PISA-data-exploration-public-vs-private-school-lower-middle-countries --------------------------------


# 1. Join student and school data to get sector for each student
student_school <- student %>%
  left_join(
    school %>% select(school_id, country, public_private),
    by = c("school_id", "country") 
  ) %>%
  clean_names() # Makes column names snake_case


# 2. Filter for valid rows
student_school <- student_school %>%
  filter(
    public_private %in% c("public", "private"),
    !is.na(avg_score),
    !is.na(stu_wgt),
    year == 2022,
    world_bank_income_group == "Lower-middle income"
  )

# 3. Calculate weighted mean score per country and sector
wm_sector <- student_school %>%
  group_by(country, public_private, country_name) %>%
  summarise(
    weighted_mean_score = weighted.mean(avg_score, stu_wgt, na.rm = TRUE),
    .groups = "drop"
  )

# 4. Reshape and compute difference
wm_wide <- wm_sector %>%
  pivot_wider(
    names_from = public_private,
    values_from = weighted_mean_score
  ) %>%
  filter(!is.na(public), !is.na(private)) %>% # Only keep countries with both sectors
  mutate(
    diff_private_public = private - public
  )

# 5. Sort by difference and set country order for plotting
wm_wide <- wm_wide %>%
  arrange(diff_private_public) %>%
  mutate(country_name = forcats::fct_inorder(country_name))

# 6. Plot
ggplot(wm_wide, aes(x = country_name)) +
  geom_point(aes(y = public, color = "Public"), size = 3, shape = 16) +
  geom_point(aes(y = private, color = "Private"), size = 3, shape = 17) +
  geom_segment(aes(y = public, yend = private, xend = country_name), linetype = "dashed", color = "grey60") +
  labs(
    title = "2022 Weighted Mean Score by School Type \n(Lower-middle income countries)",
    x = "Countries",
    y = "Weighted Mean Score",
    color = "School Type"
  ) +
  scale_color_manual(values = c("Public" = "#23395d", "Private" = "#F577e0")) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  ,legend.position = "bottom")


## ----PISA-data-exploration-public-vs-private-school-upper-middle-countries ---------------------------------



# 1. Join student and school data to get sector for each student
student_school <- student %>%
  left_join(
    school %>% select(school_id, country, public_private),
    by = c("school_id", "country") 
  ) %>%
  clean_names() # Makes column names snake_case

# 2. Filter for valid rows
student_school <- student_school %>%
  filter(
    public_private %in% c("public", "private"),
    !is.na(avg_score),
    !is.na(stu_wgt),
    year == 2022,
    world_bank_income_group == "Upper-middle income"
  )

# 3. Calculate weighted mean score per country and sector
wm_sector <- student_school %>%
  group_by(country, public_private, country_name) %>%
  summarise(
    weighted_mean_score = weighted.mean(avg_score, stu_wgt, na.rm = TRUE),
    .groups = "drop"
  )

# 4. Reshape and compute difference
wm_wide <- wm_sector %>%
  pivot_wider(
    names_from = public_private,
    values_from = weighted_mean_score
  ) %>%
  filter(!is.na(public), !is.na(private)) %>% # Only keep countries with both sectors
  mutate(
    diff_private_public = private - public
  )

# 5. Sort by difference and set country order for plotting
wm_wide <- wm_wide %>%
  arrange(diff_private_public) %>%
  mutate(country_name = forcats::fct_inorder(country_name))

# 6. Plot
ggplot(wm_wide, aes(x = country_name)) +
  geom_point(aes(y = public, color = "Public"), size = 3, shape = 16) +
  geom_point(aes(y = private, color = "Private"), size = 3, shape = 17) +
  geom_segment(aes(y = public, yend = private, xend = country_name), linetype = "dashed", color = "grey60") +
  labs(
    title = "2022 Weighted Mean Score by School Type \n(Upper-middle income countries)",
    x = "Countries",
    y = "Weighted Mean Score",
    color = "School Type"
  ) +
  scale_color_manual(values = c("Public" = "#23395d", "Private" = "#F577e0")) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )
#


## ----PISA-data-exploration-public-vs-private-school-high-income-countries  --------------------------

# 1. Join student and school data to get sector for each student
student_school <- student %>%
  left_join(
    school %>% select(school_id, country, public_private),
    by = c("school_id", "country") 
  ) %>%
  clean_names() # Makes column names snake_case

# 2. Filter for valid rows
student_school <- student_school %>%
  filter(
    public_private %in% c("public", "private"),
    !is.na(avg_score),
    !is.na(stu_wgt),
    year == 2022,
    world_bank_income_group == "High income"
  )

# 3. Calculate weighted mean score per country and sector
wm_sector <- student_school %>%
  group_by(country, public_private, country_name) %>%
  summarise(
    weighted_mean_score = weighted.mean(avg_score, stu_wgt, na.rm = TRUE),
    .groups = "drop"
  )

# 4. Reshape and compute difference
wm_wide <- wm_sector %>%
  pivot_wider(
    names_from = public_private,
    values_from = weighted_mean_score
  ) %>%
  filter(!is.na(public), !is.na(private)) %>% 
  mutate(
    diff_private_public = private - public
  )

# 5. Sort by difference and set country order for plotting
wm_wide <- wm_wide %>%
  arrange(diff_private_public) %>%
  mutate(country_name = forcats::fct_inorder(country_name))

# 6. Plot
ggplot(wm_wide, aes(x = country_name)) +
  geom_point(aes(y = public, color = "Public"), size = 3, shape = 16) +
  geom_point(aes(y = private, color = "Private"), size = 3, shape = 17) +
  geom_segment(aes(y = public, yend = private, xend = country_name), linetype = "dashed", color = "grey60") +
  labs(
    title = "2022 Weighted Mean Score by School Type \n(High income countries)",
    x = "Countries",
    y = "Weighted Mean Score",
    color = "School Type"
  ) +
  scale_color_manual(values = c("Public" = "#23395d", "Private" = "#F577e0")) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )



## ----PISA-data-exploration-boys-vs-girls-school-lower-middle-countries -------------------------------------


# 1. Standardize gender, filter for valid data (Lower-middle income, valid gender, valid score, 2022)
gender_gap <- student %>%
  mutate(
    gender = tolower(trimws(gender))
  ) %>%
  filter(
    gender %in% c("male", "female"),
    !is.na(avg_score),
    !is.na(stu_wgt),
    year == 2022,
    world_bank_income_group == "Lower-middle income"
  )

# 3. Calculate weighted mean score by country and gender
wm_gender <- gender_gap %>%
  group_by(country, gender, country_name) %>%
  summarise(
    weighted_mean_score = weighted.mean(avg_score, stu_wgt, na.rm = TRUE),
    .groups = "drop"
  )

# 4. Pivot wider and compute gender gap (female - male)
wm_gender_wide <- wm_gender %>%
  pivot_wider(
    names_from = gender,
    values_from = weighted_mean_score
  ) %>%
  filter(!is.na(male), !is.na(female)) %>%
  mutate(
    gender_gap = female - male # Positive: girls score higher
  )

# 5. Sort by gender gap and set country order for plotting
wm_gender_wide <- wm_gender_wide %>%
  arrange(gender_gap) %>%
  mutate(country_name = forcats::fct_inorder(country_name))

# 6. Plot
ggplot(wm_gender_wide, aes(x = country_name)) +
  geom_point(aes(y = male, color = "Male"), size = 3, shape = 16) +
  geom_point(aes(y = female, color = "Female"), size = 3, shape = 17) +
  geom_segment(aes(y = male, yend = female, xend = country_name), linetype = "dashed", color = "grey60") +
  labs(
    title = "2022 Weighted Mean Global Score by Gender \n(Lower-middle income countries)",
    x = "Countries",
    y = "Weighted Mean Score",
    color = "Gender"
  ) +
  scale_color_manual(values = c("Male" = "#23395d", "Female" = "#F577e0")) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )



## ----PISA-data-exploration-boys-vs-girls-school-lower-middle-countries-math -------------------------------------


# 1. Standardize gender, filter for valid data (Lower-middle income, valid gender, valid score, 2022)
gender_gap <- student %>%
  mutate(
    gender = tolower(trimws(gender))
  ) %>%
  filter(
    gender %in% c("male", "female"),
    !is.na(avg_score),
    !is.na(stu_wgt),
    year == 2022,
    world_bank_income_group == "Lower-middle income"
  )

# 2. Calculate weighted mean score by country and gender
wm_gender <- gender_gap %>%
  group_by(country, gender, country_name) %>%
  summarise(
    weighted_mean_math_score = weighted.mean(math, stu_wgt, na.rm = TRUE),
    .groups = "drop"
  )

# 4. Pivot wider and compute gender gap (female - male)
wm_gender_wide <- wm_gender %>%
  pivot_wider(
    names_from = gender,
    values_from = weighted_mean_math_score
  ) %>%
  filter(!is.na(male), !is.na(female)) %>%
  mutate(
    gender_gap = female - male # Positive: girls score higher
  )

# 5. Sort by gender gap and set country order for plotting
wm_gender_wide <- wm_gender_wide %>%
  arrange(gender_gap) %>%
  mutate(country_name = forcats::fct_inorder(country_name))

# 6. Plot
ggplot(wm_gender_wide, aes(x = country_name)) +
  geom_point(aes(y = male, color = "Male"), size = 3, shape = 16) +
  geom_point(aes(y = female, color = "Female"), size = 3, shape = 17) +
  geom_segment(aes(y = male, yend = female, xend = country_name), linetype = "dashed", color = "grey60") +
  labs(
    title = "2022 Weighted Mean Math Score by Gender \n(Lower middle income countries)",
    x = "Countries",
    y = "Weighted Mean Math Score",
    color = "Gender"
  ) +
  scale_color_manual(values = c("Male" = "#23395d", "Female" = "#F577e0")) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )



## ----PISA-data-exploration-boys-vs-girls-school-upper-middle-countries -----------------------------


# 1. Standardize gender, filter for valid data (Upper-middle income, valid gender, valid score, 2022)
gender_gap <- student %>%
  mutate(
    gender = tolower(trimws(gender))
  ) %>%
  filter(
    gender %in% c("male", "female"),
    !is.na(avg_score),
    !is.na(stu_wgt),
    year == 2022,
    world_bank_income_group == "Upper-middle income"
  )

# 3. Calculate weighted mean score by country and gender
wm_gender <- gender_gap %>%
  group_by(country, gender, country_name) %>%
  summarise(
    weighted_mean_score = weighted.mean(avg_score, stu_wgt, na.rm = TRUE),
    .groups = "drop"
  )

# 4. Pivot wider and compute gender gap (female - male)
wm_gender_wide <- wm_gender %>%
  pivot_wider(
    names_from = gender,
    values_from = weighted_mean_score
  ) %>%
  filter(!is.na(male), !is.na(female)) %>%
  mutate(
    gender_gap = female - male # Positive: girls score higher
  )

# 5. Sort by gender gap and set country order for plotting
wm_gender_wide <- wm_gender_wide %>%
  arrange(gender_gap) %>%
  mutate(country_name = forcats::fct_inorder(country_name))

# 6. Plot
ggplot(wm_gender_wide, aes(x = country_name)) +
  geom_point(aes(y = male, color = "Male"), size = 3, shape = 16) +
  geom_point(aes(y = female, color = "Female"), size = 3, shape = 17) +
  geom_segment(aes(y = male, yend = female, xend = country_name), linetype = "dashed", color = "grey60") +
  labs(
    title = "2022 Weighted Mean Global Score by Gender \n(Upper-middle income countries)",
    x = "Countries",
    y = "Weighted Mean Score",
    color = "Gender"
  ) +
  scale_color_manual(values = c("Male" = "#23395d", "Female" = "#F577e0")) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )



## ----PISA-data-exploration-boys-vs-girls-school-upper-middle-countries-math --------------------------------------


# 1. Standardize gender, filter for valid data (Upper-middle income, valid gender, valid score, 2022)
gender_gap <- student %>%
  mutate(
    gender = tolower(trimws(gender))
  ) %>%
  filter(
    gender %in% c("male", "female"),
    !is.na(avg_score),
    !is.na(stu_wgt),
    year == 2022,
    world_bank_income_group == "Upper-middle income"
  )

# 2. Calculate weighted mean score by country and gender
wm_gender <- gender_gap %>%
  group_by(country, gender, country_name) %>%
  summarise(
    weighted_mean_math_score = weighted.mean(math, stu_wgt, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Pivot wider and compute gender gap (female - male)
wm_gender_wide <- wm_gender %>%
  pivot_wider(
    names_from = gender,
    values_from = weighted_mean_math_score
  ) %>%
  filter(!is.na(male), !is.na(female)) %>%
  mutate(
    gender_gap = female - male # Positive: girls score higher
  )

# 4. Sort by gender gap and set country order for plotting
wm_gender_wide <- wm_gender_wide %>%
  arrange(gender_gap) %>%
  mutate(country_name = forcats::fct_inorder(country_name))

# 5. Plot
ggplot(wm_gender_wide, aes(x = country_name)) +
  geom_point(aes(y = male, color = "Male"), size = 3, shape = 16) +
  geom_point(aes(y = female, color = "Female"), size = 3, shape = 17) +
  geom_segment(aes(y = male, yend = female, xend = country_name), linetype = "dashed", color = "grey60") +
  labs(
    title = "2022 Weighted Mean Math Score by Gender \n(Upper middle income countries)",
    x = "Countries",
    y = "Weighted Mean Math Score",
    color = "Gender"
  ) +
  scale_color_manual(values = c("Male" = "#23395d", "Female" = "#F577e0")) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )



## ----PISA-data-exploration-boys-vs-girls-school-high-income-countries ------------------------------------

# 1. Standardize gender, filter for valid data (High income, valid gender, valid score, 2022)
gender_gap <- student %>%
  mutate(
    gender = tolower(trimws(gender))
  ) %>%
  filter(
    gender %in% c("male", "female"),
    !is.na(avg_score),
    !is.na(stu_wgt),
    year == 2022,
    world_bank_income_group == "High income"
  )

# 2. Calculate weighted mean score by country and gender
wm_gender <- gender_gap %>%
  group_by(country, gender, country_name) %>%
  summarise(
    weighted_mean_score = weighted.mean(avg_score, stu_wgt, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Pivot wider and compute gender gap (female - male)
wm_gender_wide <- wm_gender %>%
  pivot_wider(
    names_from = gender,
    values_from = weighted_mean_score
  ) %>%
  filter(!is.na(male), !is.na(female)) %>%
  mutate(
    gender_gap = female - male # Positive: girls score higher
  )

# 4. Sort by gender gap and set country order for plotting
wm_gender_wide <- wm_gender_wide %>%
  arrange(gender_gap) %>%
  mutate(country_name = forcats::fct_inorder(country_name))

# 5. Plot
ggplot(wm_gender_wide, aes(x = country_name)) +
  geom_point(aes(y = male, color = "Male"), size = 3, shape = 16) +
  geom_point(aes(y = female, color = "Female"), size = 3, shape = 17) +
  geom_segment(aes(y = male, yend = female, xend = country_name), linetype = "dashed", color = "grey60") +
  labs(
    title = "2022 Weighted Mean Global Score by Gender \n(High income countries)",
    x = "Countries",
    y = "Weighted Mean Score",
    color = "Gender"
  ) +
  scale_color_manual(values = c("Male" = "#23395d", "Female" = "#F577e0")) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )



## ----PISA-data-exploration-boys-vs-girls-school-high-countries-math ---------------------------------------


# 1. Standardize gender, filter for valid data (High income, valid gender, valid score, 2022)
gender_gap <- student %>%
  mutate(
    gender = tolower(trimws(gender))
  ) %>%
  filter(
    gender %in% c("male", "female"),
    !is.na(avg_score),
    !is.na(stu_wgt),
    year == 2022,
    world_bank_income_group == "High income"
  )

# 2. Calculate weighted mean score by country and gender
wm_gender <- gender_gap %>%
  group_by(country, gender, country_name) %>%
  summarise(
    weighted_mean_math_score = weighted.mean(math, stu_wgt, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Pivot wider and compute gender gap (female - male)
wm_gender_wide <- wm_gender %>%
  pivot_wider(
    names_from = gender,
    values_from = weighted_mean_math_score
  ) %>%
  filter(!is.na(male), !is.na(female)) %>%
  mutate(
    gender_gap = female - male # Positive: girls score higher
  )

# 4. Sort by gender gap and set country order for plotting
wm_gender_wide <- wm_gender_wide %>%
  arrange(gender_gap) %>%
  mutate(country_name = forcats::fct_inorder(country_name))

# 5. Plot
ggplot(wm_gender_wide, aes(x = country_name)) +
  geom_point(aes(y = male, color = "Male"), size = 3, shape = 16) +
  geom_point(aes(y = female, color = "Female"), size = 3, shape = 17) +
  geom_segment(aes(y = male, yend = female, xend = country_name), linetype = "dashed", color = "grey60") +
  labs(
    title = "2022 Weighted Mean Math Score by Gender \n(High income countries)",
    x = "Countries",
    y = "Weighted Mean Math Score",
    color = "Gender"
  ) +
  scale_color_manual(values = c("Male" = "#23395d", "Female" = "#F577e0")) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )



## ----PISA-data-exploration-boys-vs-girls-parity-index -------------------------------------

# Prepare your gender performance gap for 2018
# Calculate weighted mean PISA score gap by gender, per country, for 2018
gender_gap_2018 <- student %>%
  mutate(
    gender = tolower(trimws(gender))
  ) %>%
  filter(
    gender %in% c("male", "female"),
    !is.na(avg_score),
    !is.na(stu_wgt),
    year == 2018
  ) %>%
  group_by(country, gender, country_name, world_bank_income_group) %>%
  summarise(
    weighted_mean_score = weighted.mean(avg_score, stu_wgt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = gender,
    values_from = weighted_mean_score
  ) %>%
  filter(!is.na(male), !is.na(female)) %>%
  mutate(
    gender_gap = female - male # Positive: girls score higher
  )

#Prepare the UNICEF gender parity index for 2018, secondary school

gender_index_sec_2018 <- gender_index %>%
  filter(
    year == 2018,
    tolower(school_level) == "secondary",
    !is.na(gender_parity_index)
  ) %>%
  select(country, gender_parity_index)

# Merge the two datasets
gap_vs_index <- gender_gap_2018 %>%
  left_join(
    gender_index_sec_2018,
    by = "country"
  ) %>%
  filter(!is.na(gender_parity_index)) # Keep only countries with available index
  
#Visualize the relationship

# Define ymin and ymax for the background shading
ymin <- min(-10, min(gap_vs_index$gender_gap, na.rm = TRUE))
ymax <- max(gap_vs_index$gender_gap, na.rm = TRUE)

ggplot(gap_vs_index, aes(x = gender_parity_index, y = gender_gap, label = country_name)) +
  # Blue zone for y < 0 (boys > girls)
  annotate("rect",
           xmin = -Inf, xmax = Inf, ymin = ymin, ymax = 0,
           fill = "#a4c6f2", alpha = 0.13) +
  # Pink zone for y > 0 (girls > boys)
  annotate("rect",
           xmin = -Inf, xmax = Inf, ymin = 0, ymax = ymax,
           fill = "#f8bbd0", alpha = 0.13) +
  # Gray rectangle for parity zone [0.97, 1.03]
  annotate("rect",
           xmin = 0.97, xmax = 1.03, ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "grey50") +
  # Vertical dashed lines for the parity bounds
  geom_vline(xintercept = c(0.97, 1.03), linetype = "dashed", color = "grey30") +
  # Scatter points, colored by income group
  geom_point(aes(color = world_bank_income_group), size = 3) +
  # Regression line
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "solid") +
  # Country names
  geom_text_repel(
  size = 3,
  box.padding = 0.25,
  max.overlaps = 60 
)+
  # Adjust axes
  scale_x_continuous(
    name = "UNICEF Gender Parity Index (Secondary, 2018)",
    breaks = seq(0.90, 1.15, by = 0.05)
  ) +
  coord_cartesian(ylim = c(-10, max(gap_vs_index$gender_gap, na.rm = TRUE))) +
  # Titles, subtitle, legend, and long caption
  labs(
    title = "Relation between Gender Parity Index (UNICEF) and PISA Gender Score Gap (2018, Secondary School)",
    subtitle = "Each point is a country. Color indicates World Bank income group.",
    x = "UNICEF Gender Parity Index (Secondary, 2018)\n  below 1 there are less girls than boys, abose 1 there are more boys than girls int he secondary",
    y = "PISA Gender Performance Gap (Girls - Boys, 2018)\n when the number is positive the girls overperform the boys",
    color = "Income Group",
    caption = paste(
      "Note:",
      "At 0.90 gender parity index, there are 90 girls studying in secondary for every 100 boys.\n",
      "Between 0.97 and 1.03 (grey band), it is considered that there is parity between boys and girls.\n",
      "Below 1, more boys than girls; above 1, more girls than boys in secondary school.\n",
      "A gender gap of +10 means girls outperform boys by 10 points on average; negative values mean boys score higher.\n"
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(size = 10, hjust = 0),
    legend.position = "bottom"
  )



## ----PISA-data-exploration-boys-vs-girls-parity-index-math --------------------------------------


# Prepare your gender performance gap for 2018
# Calculate weighted mean PISA score gap by gender, per country, for 2018
gender_gap_2018 <- student %>%
  mutate(
    gender = tolower(trimws(gender))
  ) %>%
  filter(
    gender %in% c("male", "female"),
    !is.na(math),
    !is.na(stu_wgt),
    year == 2018
  ) %>%
  group_by(country, gender, country_name, world_bank_income_group) %>%
  summarise(
    weighted_mean_score = weighted.mean(math, stu_wgt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = gender,
    values_from = weighted_mean_score
  ) %>%
  filter(!is.na(male), !is.na(female)) %>%
  mutate(
    gender_gap = female - male # Positive: girls score higher
  )

#Prepare the UNICEF gender parity index for 2018, secondary school

gender_index_sec_2018 <- gender_index %>%
  filter(
    year == 2018,
    tolower(school_level) == "secondary",
    !is.na(gender_parity_index)
  ) %>%
  select(country, gender_parity_index)

# Merge the two datasets
gap_vs_index <- gender_gap_2018 %>%
  left_join(
    gender_index_sec_2018,
    by = "country"
  ) %>%
  filter(!is.na(gender_parity_index)) # Keep only countries with available index
  
#Visualize the relationship

# Define ymin and ymax for the background shading
ymin <- min(-25, min(gap_vs_index$gender_gap, na.rm = TRUE))
ymax <- max(gap_vs_index$gender_gap, na.rm = TRUE)

ggplot(gap_vs_index, aes(x = gender_parity_index, y = gender_gap, label = country_name)) +
  # Blue zone for y < 0 (boys > girls)
  annotate("rect",
           xmin = -Inf, xmax = Inf, ymin = ymin, ymax = 0,
           fill = "#a4c6f2", alpha = 0.13) +
  # Pink zone for y > 0 (girls > boys)
  annotate("rect",
           xmin = -Inf, xmax = Inf, ymin = 0, ymax = ymax,
           fill = "#f8bbd0", alpha = 0.13) +
  # Gray rectangle for parity zone [0.97, 1.03]
  annotate("rect",
           xmin = 0.97, xmax = 1.03, ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "grey50") +
  # Vertical dashed lines for the parity bounds
  geom_vline(xintercept = c(0.97, 1.03), linetype = "dashed", color = "grey30") +
  # Scatter points, colored by income group
  geom_point(aes(color = world_bank_income_group), size = 3) +
  # Regression line
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "solid") +
  # Country names
  geom_text_repel(
  size = 3,
  box.padding = 0.25,
  max.overlaps = 60 
)+
  # Adjust axes
  scale_x_continuous(
    name = "UNICEF Gender Parity Index (Secondary, 2018)",
    breaks = seq(0.90, 1.15, by = 0.05)
  ) +
  coord_cartesian(ylim = c(-25, max(gap_vs_index$gender_gap, na.rm = TRUE))) +
  # Titles, subtitle, legend, and long caption
  labs(
    title = "Relation between Gender Parity Index (UNICEF) and PISA Gender Math Score Gap (2018, Secondary School)",
    subtitle = "Each point is a country. Color indicates World Bank income group.",
    x = "UNICEF Gender Parity Index (Secondary, 2018)\n  below 1 there are less girls than boys, abose 1 there are more boys than girls int he secondary",
    y = "PISA Math Gender Performance Gap (Girls - Boys, 2018)\n when the number is positive the girls overperform the boys",
    color = "Income Group",
    caption = paste(
      "Note:",
      "At 0.90 gender parity index, there are 90 girls studying in secondary for every 100 boys.\n",
      "Between 0.97 and 1.03 (grey band), it is considered that there is parity between boys and girls.\n",
      "Below 1, more boys than girls; above 1, more girls than boys in secondary school.\n",
      "A gender gap of +10 means girls outperform boys by 10 points on average; negative values mean boys score higher.\n"
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(size = 10, hjust = 0),
    legend.position = "bottom"
  )




## ----PISA-dataset-create-initial-database------------------------------------------------------

# 1. Filer the years 2022
student_sub <- student %>%
  filter(year %in% c(2022))

# 2. Select school columns
school_sub <- school %>%
  select(
    school_uid,
    enrol_boys,
    enrol_girls,
    stratio,
    public_private,
    staff_shortage,
    fund_gov,
    school_size
  )

# 3. Joint school and student
student_enriched <- student_sub %>%
  left_join(school_sub, by = "school_uid")

# 4. Add GDP per country 
student_enriched <- student_enriched %>%
  left_join(gdp_perppp %>% select(country, GDP_per_PPP_2022), by = "country")

# 5. Add gender index 
gender_index_secondary <- gender_index %>%
  filter(school_level == "Secondary", !is.na(gender_parity_index)) %>%
  group_by(country) %>%
  slice_max(order_by = year, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(country, gender_parity_index, year) 

# 6. Add education expenditure (% of GDP)
student_enriched <- student_enriched %>%
  left_join(
    edu_exp_latest %>% 
      select(country, percentage_education_on_gdp),
    by = "country"
  )

# Merge
student_enriched <- student_enriched %>%
  left_join(gender_index_secondary %>% select(country, gender_parity_index), by = "country")

# Check
glimpse(student_enriched)


## ----PISA-dataset-correlations,-----------------------------------------------------------


num_vars <- student_enriched %>%
  mutate(percentage_education_on_gdp = as.numeric(percentage_education_on_gdp)) %>%
  select(
    avg_score,
    escs,
    staff_shortage,
    fund_gov,
    school_size,
    stratio,
    GDP_per_PPP_2022,
    gender_parity_index,
    percentage_education_on_gdp
  ) %>%
  na.omit()


cor_matrix <- cor(num_vars)
# Renamme for better lisibility
colnames(cor_matrix) <- rownames(cor_matrix) <- c(
  "PISA Score",
  "ESCS",
  "Staff Shortage",
  "School Gov.Funding (%)",
  "School Size",
  "Student-Teacher Ratio",
  "GDP per capita (PPP)",
  "Gender Parity Index",
  "Education Spending (% of GDP)"
)
corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  col = colorRampPalette(c("red", "white", "green"))(200),
  tl.cex = 0.8,
  number.cex = 0.6,
  cl.cex = 0.8,
  mar = c(0,0,1,0)
)


## ----PISA-dataset-train-split-test, ------------------------------------------------------


set.seed(1, sample.kind = "Rounding") # pour la reproductibilité

# 1. Creation of a test index (10% of students)
test_index <- createDataPartition(y = student_enriched$school_uid, times = 1, p = 0.1, list = FALSE)

student_train <- student_enriched[-test_index, ]
temp <- student_enriched[test_index, ]

# 2. Safety: Keep in the test only students whose school also exists on the train
student_test <- temp %>%
  semi_join(student_train, by = "school_uid")

# 3. If students are excluded from the test because of a school not being on the train, they are put back on the train.
removed <- anti_join(temp, student_test)
student_train <- bind_rows(student_train, removed)

# 4. Useful checks
cat("Number of unique school_uid in train:", n_distinct(student_train$school_uid), "\n")
cat("Number of unique school_uid in test:", n_distinct(student_test$school_uid), "\n")
cat("All test schools are in train:", all(unique(student_test$school_uid) %in% unique(student_train$school_uid)), "\n")
cat("Number of students in train:", nrow(student_train), "\n")
cat("Number of students in test:", nrow(student_test), "\n")


## ----PISA-dataset-naive-base, -----------------------------------------------------------------


baseline_pred <- mean(student_train$avg_score)
rmse_baseline <- RMSE(rep(baseline_pred, nrow(student_test)), student_test$avg_score)
cat("Baseline RMSE:", rmse_baseline, "\n")



## ----PISA-dataset-predictive-models, ---------------------------------------------------------


## List of factor or character columns with a single level
which(sapply(student_train, function(x) is.factor(x) && length(unique(x[!is.na(x)])) < 2))

# Delete all factor columns with a single level
one_level_factors <- sapply(student_train, function(x) is.factor(x) && length(unique(x[!is.na(x)])) < 2)
student_train <- student_train[ , !one_level_factors]
student_test  <- student_test[ , !one_level_factors]

# Remove columns with a single level or all NA in the train
valid_cols <- sapply(student_train, function(x)
  !(is.factor(x) && length(unique(x[!is.na(x)])) < 2) && !all(is.na(x))
)
student_train <- student_train[, valid_cols]
student_test  <- student_test[, valid_cols]

vars_to_use <- c("avg_score", "escs", "staff_shortage", "fund_gov", "school_size", "stratio" , "GDP_per_PPP_2022", "gender_parity_index","percentage_education_on_gdp")
student_train2 <- student_train %>%
  select(any_of(vars_to_use)) %>%
  filter(complete.cases(.)) # enlève les lignes avec NA

student_test2 <- student_test %>%
  select(any_of(vars_to_use)) %>%
  filter(complete.cases(.))

# Linear Regression
fit_lm <- lm(avg_score ~ ., data = student_train2)
pred_lm <- predict(fit_lm, newdata = student_test2)
rmse_lm <- RMSE(pred_lm, student_test2$avg_score)


# Ranger Random Forest as it was not possible to launch Random Forest due to the RAM consumption

fit_ranger <- ranger(avg_score ~ ., data = student_train2, num.trees = 100)
pred_ranger <- predict(fit_ranger, data = student_test2)$predictions
rmse_ranger <- RMSE(pred_ranger, student_test2$avg_score)
cat("Ranger RMSE:", rmse_ranger, "\n")


# Assume "avg_score" is the target, all other columns are predictor variables
# Use only numeric columns without NA

predictors <- c("escs", "staff_shortage", "fund_gov", "school_size", "stratio" , "GDP_per_PPP_2022", "gender_parity_index","percentage_education_on_gdp")
student_train2 <- student_train %>%
  select(all_of(c("avg_score", predictors))) %>%
  filter(complete.cases(.))
student_test2 <- student_test %>%
  select(all_of(c("avg_score", predictors))) %>%
  filter(complete.cases(.))

# Convert to matrices
xgb_train <- as.matrix(student_train2 %>% select(-avg_score))
y_train <- student_train2$avg_score
xgb_test <- as.matrix(student_test2 %>% select(-avg_score))
y_test <- student_test2$avg_score

# Fit XGBoost
fit_xgb <- xgboost(
  data = xgb_train,
  label = y_train,
  nrounds = 100,
  objective = "reg:squarederror",
  verbose = 0
)

# Predict
pred_xgb <- predict(fit_xgb, newdata = xgb_test)
rmse_xgb <- RMSE(pred_xgb, y_test)
cat("XGBoost RMSE:", rmse_xgb, "\n")

# Summary table
results <- data.frame(
  Model = c("Baseline (Mean)", "Linear Regression", "Ranger (Random Forest)", "XGBoost"),
  RMSE = c(rmse_baseline, rmse_lm, rmse_ranger, rmse_xgb)
)
print(results)



## ----PISA-dataset-results, -------------------------------------------------------------


# Baseline: mean prediction
baseline_pred <- mean(student_train2$avg_score)
pred_baseline <- rep(baseline_pred, nrow(student_test2))
rmse_baseline <- RMSE(pred_baseline, student_test2$avg_score)

# Linear regression
pred_lm <- predict(fit_lm, newdata = student_test2)
rmse_lm <- RMSE(pred_lm, student_test2$avg_score)

# Ranger (Random Forest)
pred_ranger <- predict(fit_ranger, data = student_test2)$predictions
rmse_ranger <- RMSE(pred_ranger, student_test2$avg_score)

# XGBoost
pred_xgb <- predict(fit_xgb, newdata = as.matrix(student_test2 %>% select(-avg_score)))
rmse_xgb <- RMSE(pred_xgb, student_test2$avg_score)

# Results table
results <- data.frame(
  Model = c("Baseline (Mean)", "Linear Regression", "Ranger (Random Forest)", "XGBoost"),
  RMSE = c(rmse_baseline, rmse_lm, rmse_ranger, rmse_xgb)
)
print(results)


## ----PISA-dataset-best-results, ----------------------------------------------------------------


cat(sprintf("Final RMSE on holdout test set (best model): %.3f\n", min(results$RMSE)))

