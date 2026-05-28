# =========================================================
# PROJECT: Poverty and Social Exclusion Analysis (Spain)
# SCRIPT: spain_poverty_arope_analysis.R
# PURPOSE: Exploratory descriptive analysis of AROPE trends
# DATA SOURCE: INE / ECV
# =========================================================

# =========================
# Libraries
# =========================

library(tidyverse)
library(readxl)
library(haven)
library(janitor)
library(scales)
library(lubridate)
library(gt)

# ==========================================
# 1. Data path
# ==========================================

data <- read_csv2("data_poverty_index_EAPN.csv")

data <- clean_names(data)

data <- data %>%
  rename(
    age_group = grupos_de_edad,
    sex = sexo,
    year = periodo,
    arope_rate = total
  ) %>%
  mutate(
    sex = recode(
      sex,
      "Mujeres" = "Women",
      "Hombres" = "Men"
    )
  )

# =========================
# 2. Data inspection
# =========================

glimpse(data)

count(data, age_group)

count(data, sex)

names(data)

dim(data)

head(data)

summary(data)

# ==========================================
# 3. Descriptive analysis
# ==========================================

data %>%
  group_by(sex) %>%
  summarise(mean = mean(arope_rate))

data %>%
  group_by(age_group) %>%
  summarise(mean = mean(arope_rate))

data %>%
  group_by(age_group, sex) %>%
  summarise(mean = mean(arope_rate))

# ==========================================
# 4. Poverty trends: men vs women
# ==========================================

# How has the AROPE rate evolved by sex?

data %>%
  filter(age_group == "Total") %>%
  ggplot(aes(x = year,
             y = arope_rate,
             color = sex)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Evolution of poverty or social exclusion risk in Spain by sex",
    subtitle = "AROPE indicator by sex (2014–2025)",
    x = "Year",
    y = "AROPE rate (%)",
    color = "Sex",
    caption = "Source: author's own elaboration based on data from the Spanish National Statistics Institute (INE), AROPE indicator."
  ) +
  scale_y_continuous(
    labels = scales::label_number(suffix = "%")
  ) +
  theme_minimal()

# =========================
# 4.1. AROPE summary table by sex
# =========================

summary_table <- data %>%
  filter(age_group == "Total",
         year %in% c(2014, 2025)) %>%
  select(sex, year, arope_rate) %>%
  pivot_wider(
    names_from = year,
    values_from = arope_rate
  ) %>%
  mutate(
    change = `2025` - `2014`
  ) %>%
  rename(
    `2014` = `2014`,
    `2025` = `2025`,
    `Change (p.p.)` = change
  )

summary_table %>%
  gt() %>%
  fmt_number(
    columns = c(`2014`, `2025`, `Change (p.p.)`),
    decimals = 1
  ) %>%
  cols_label(
    sex = "Sex"
  ) %>%
  tab_header(
    title = "Evolution of poverty or social exclusion risk by sex",
    subtitle = "AROPE indicator, Spain (2014–2025)"
  ) %>%
  tab_source_note(
    source_note = "Source: author's own elaboration based on data from the Spanish National Statistics Institute (INE), AROPE indicator."
  )

# =========================
# 4.2. How does AROPE evolve by sex within each age cohort?
# =========================

data %>%
  filter(age_group != "Total") %>%
  ggplot(aes(x = year,
             y = arope_rate,
             color = sex)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ age_group) +
  labs(
    title = "Evolution of poverty or social exclusion risk by age group",
    subtitle = "AROPE indicator by sex (2014–2025)",
    x = "Year",
    y = "AROPE rate (%)",
    color = "Sex",
    caption = "Source: author's own elaboration based on data from the Spanish National Statistics Institute (INE), AROPE indicator."
  ) +
  scale_y_continuous(
    labels = scales::label_number(suffix = "%")
  ) +
  theme_minimal()

# =========================
# 4.3. Which groups are most affected today?
# =========================

data %>%
  filter(year == 2025,
         age_group != "Total") %>%
  ggplot(aes(x = age_group,
             y = arope_rate,
             fill = sex)) +
  geom_col(position = "dodge") +
  labs(
    title = "AROPE risk by age group in 2025",
    subtitle = "Comparison by sex",
    x = "Age group",
    y = "AROPE rate (%)",
    fill = "Sex",
    caption = "Source: author's own elaboration based on data from the Spanish National Statistics Institute (INE), AROPE indicator."
  ) +
  scale_y_continuous(
    labels = scales::label_number(suffix = "%")
  ) +
  theme_minimal()

# =========================
# 4.4. Which groups have improved the most?
# =========================

data %>%
  filter(year %in% c(2014, 2025),
         age_group != "Total") %>%
  ggplot(aes(x = age_group,
             y = arope_rate,
             fill = factor(year))) +
  geom_col(position = "dodge") +
  facet_wrap(~ sex) +
  labs(
    title = "Change in AROPE risk between 2014 and 2025",
    subtitle = "Comparison by sex and age group",
    x = "Age group",
    y = "AROPE rate (%)",
    fill = "Year"
  ) +
  scale_y_continuous(
    labels = scales::label_number(suffix = "%")
  ) +
  theme_minimal()

# =========================
# 4.5. Where is vulnerability concentrated?
# =========================

data %>%
  filter(sex == "Women",
         age_group != "Total") %>%
  ggplot(aes(x = year,
             y = age_group,
             fill = arope_rate)) +
  geom_tile() +
  labs(
    title = "Heatmap of AROPE risk",
    subtitle = "Women by age group",
    x = "Year",
    y = "Age group",
    fill = "AROPE"
  ) +
  theme_minimal()

# =========================
# 4.6. Is the gender gap widening or narrowing?
# =========================

gender_gap <- data %>%
  filter(age_group == "Total") %>%
  select(year, sex, arope_rate) %>%
  pivot_wider(
    names_from = sex,
    values_from = arope_rate
  ) %>%
  mutate(
    gender_gap = Women - Men
  )

ggplot(gender_gap,
       aes(x = year,
           y = gender_gap)) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey50"
  ) +
  geom_line() +
  geom_point() +
  labs(
    title = "Gender gap to the disadvantage of women",
    subtitle = "Difference in percentage points (women - men) in AROPE risk, 2014–2025",
    x = "Year",
    y = "Percentage points",
    caption = "Source: author's own elaboration based on data from the Spanish National Statistics Institute (INE), AROPE indicator."
  ) +
  theme_minimal()