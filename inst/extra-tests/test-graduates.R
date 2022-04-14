library(tidylfs)
library(tidyverse)

# Data Prep ----------------------------------------

# Replicate official DfE definitions
source("inst/extra-tests/dfe_graduate_definitions.R")

lfs <- lfs_load() %>%
  lfs_flag_notinscope() %>%
  lfs_flag_graduate2() %>%
  lfs_flag_postgraduate2()

# Headline Graduate Labour Market Stats --------------------

# Graduate employment rate
lfs %>%
  filter(NOTSCOPE == 0) %>%
  filter(YEAR %in% c(2019, 2020)) %>%
  filter(COUNTRY == "England") %>%
  filter(AGE %in% 16:64) %>%
  filter(GRADUATE == 1) %>%
  filter(POSTGRADUATE == 0) %>%
  lfs_summarise_unemployment(YEAR, GRADUATE) %>%
  select(YEAR, employed_percentage)


# Median salary for graduates
lfs %>%
  filter(!is.na(OCCUPATION_MAJOR)) %>%
  filter(NOTSCOPE == 0) %>%
  filter(POSTGRADUATE == 0) %>%
  filter(YEAR == 2020) %>%
  filter(WEIGHT_INCOME > 0) %>%
  filter(COUNTRY == "England") %>%
  filter(AGE %in% 16:64) %>%
  filter(GRADUATE == 1) %>%
  filter(ILODEFR == "In employment") %>%
  filter(FTPTWK == "Full-time") %>%
  lfs_summarise_salary() %>%
  mutate(median_weekly_pay = median_weekly_pay * 52) %>%
  select(median_weekly_pay)


# Prop in high skilled employment
lfs %>%
  filter(NOTSCOPE == 0) %>%
  filter(POSTGRADUATE == 0) %>%
  filter(YEAR == 2020) %>%
  filter(COUNTRY == "England") %>%
  filter(AGE %in% 16:64) %>%
  filter(GRADUATE == 1) %>%
  filter(ILODEFR == "In employment") %>%
  filter(FTPTWK == "Full-time") %>%
  filter(!is.na(OCCUPATION_MAJOR)) %>%
  # filter(POSTGRADUATE != 1) %>%
  group_by(GRADUATE, OCCUPATION_MAJOR) %>%
  summarise(n = sum(WEIGHT))  %>%
  mutate(HIGH_SKILLED = ifelse(OCCUPATION_MAJOR %in% c(1:3), 1 ,0)) %>%
  group_by(HIGH_SKILLED) %>%
  summarise(number = sum(n)) %>%
  mutate(prop = number / sum(number))
  

# Median salary premium
diff <- lfs %>%
  filter(NOTSCOPE == 0) %>%
  filter(POSTGRADUATE == 0) %>%
  filter(YEAR == 2020) %>%
  filter(COUNTRY == "England") %>%
  filter(AGE %in% 16:64) %>%
  # filter(GRADUATE == 1) %>%
  filter(ILODEFR == "In employment") %>%
  # filter(FTPTWK == "Full-time") %>%
  # filter(POSTGRADUATE != 1) %>%
  lfs_summarise_salary(GRADUATE) %>%
  mutate(median_weekly_pay = median_weekly_pay * 52) %>%
  select(median_weekly_pay)

# Salary premium
diff[[1]][[1]] - diff[[1]][[2]]

