load_all()
library(tidyverse)

lfs <- lfs_load() %>%
  lfs_flag_graduate() %>%
  lfs_flag_graduate_ons() %>%
  lfs_flag_postgraduate()

lfs %>%
  filter(QUARTER == "2020 Q4") %>%
  filter(COUNTRY == "England") %>%
  filter(AGE %in% 16:64) %>%
  filter(GRADUATE == 1) %>%
  filter(POSTGRADUATE == 0) %>%
  lfs_summarise_unemployment()



lfs %>%
  filter(AGE %in% 21:64) %>%
  group_by(QUARTER) %>%
  summarise(
      graduate = sum(as.numeric(.data$GRADUATE2 == 1) * .data$WEIGHT, na.rm = TRUE),
      nongraduate = sum(as.numeric(.data$GRADUATE2 == 0) * .data$WEIGHT, na.rm = TRUE),
      .groups = "drop"
      ) %>%
  dplyr::mutate(
      graduate_percentage = .data$graduate /
        (.data$nongraduate + .data$graduate)
      ) %>%
    dplyr::select(
      .data$QUARTER,
      .data$graduate_percentage
    ) %>%
    tail()




lfs %>%
  filter(QUARTER == "2017 Q3") %>%
  filter(AGE %in% 21:64) %>%
  lfs_summarise_unemployment(HIQUALD)
