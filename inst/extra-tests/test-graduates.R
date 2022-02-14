load_all()
library(tidyverse)

lfs <- lfs_load() %>%
  lfs_flag_graduate() %>%
  lfs_flag_postgraduate()

lfs %>%
  filter(YEAR == 2020) %>%
  filter(COUNTRY == "England") %>%
  filter(AGE %in% 16:64) %>%
  # filter(GRADUATE == 1) %>%
  # filter(POSTGRADUATE == 0) %>%
  lfs_summarise_unemployment(GRADUATE)

lfs %>%
  filter(YEAR == 2020) %>%
  filter(COUNTRY == "England") %>%
  filter(AGE %in% 16:64) %>%
  filter(GRADUATE == 1) %>%
  filter(ILODEFR == 1) %>%
  filter(FTPT == 1) %>%
  lfs_summarise_salary() %>%
  mutate(median_weekly_pay = median_weekly_pay * 52) %>%
  select(median_weekly_pay)


lfs %>%
  filter(YEAR == 2020) %>%
  filter(COUNTRY == "England") %>%
  filter(AGE %in% 16:64) %>%
  filter(GRADUATE == 1) %>%
  filter(ILODEFR == 1) %>%
  filter(FTPT == 1) %>%
  group_by(GRADUATE, OCCUPATION)

