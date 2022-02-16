library(tidylfs)
library(tidyverse)




lfs_flag_graduate2 <- function(lfs, column_name = "GRADUATE") {
  rename <- c("column_name")
  names(rename) <- {{ column_name }}

  lfs <- lfs %>%
    dplyr::mutate(column_name = ifelse(
      (.data$HIQUAL %in% c(
        "First/Foundation degree",
        "First degree",
        "First degree/foundation degree"
      )) &
        (.data$DEGREE71 %in% c("A first degree") |
          .data$DEGREE72 %in% c("A first degree") |
          .data$DEGREE73 %in% c("A first degree") |
          .data$DEGREE74 %in% c("A first degree") |
          .data$DEGREE75 %in% c("A first degree")),
      1, 0
    )) %>%
    dplyr::rename(rename)
}


lfs_flag_postgraduate2 <- function(lfs, column_name = "POSTGRADUATE") {
  rename <- c("column_name")
  names(rename) <- {{ column_name }}

  lfs <- lfs %>%
    dplyr::mutate(column_name = ifelse(
      .data$HIQUAL == "Higher degree" &
        !.data$HIGHO %in% c("Dont know"), 1, 0
    )) %>%
    dplyr::rename(rename)
}

lfs_flag_notinscope <-  function(lfs, column_name = "NOTSCOPE") {
  rename <- c("column_name")
  names(rename) <- {{ column_name }}

  lfs %>%
    dplyr::mutate(column_name = case_when(
HIQUAL == "Level 8 Diploma"| 
HIQUAL == "Level 8 Certificate"|
HIQUAL == "Level 8 Award"|
HIQUAL == "Level 7 Certificate"|
HIQUAL == "Level 7 Diploma"|
HIQUAL == 'Level 7 Award'|
HIQUAL == 'Level 6 Award'|
HIQUAL == 'Level 6 Certificate'|
HIQUAL == 'Level 6 Diploma'|
HIQUAL == "Don.t know"|
HIQUAL == "Don't know"|
HIQUAL == "Did not know"|
(HIQUAL == "Higher degree" & HIQUAL == "Dont know")|
(HIQUAL == "Higher degree" & is.na(HIGHO)) ~ 1,
TRUE ~ 0
    )) %>%
    dplyr::rename(rename)

}


lfs <- lfs_load() %>%
  lfs_flag_notinscope() %>%
  lfs_flag_graduate2() %>%
  lfs_flag_postgraduate2()

lfs %>%
  filter(NOTSCOPE == 0) %>%
  filter(YEAR %in% c(2019, 2020)) %>%
  filter(COUNTRY == "England") %>%
  filter(AGE %in% 16:64) %>%
  filter(GRADUATE == 1) %>%
  filter(POSTGRADUATE == 0) %>%
  lfs_summarise_unemployment(YEAR, GRADUATE) %>%
  select(employed_percentage)

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

diff[[1]][[1]] - diff[[1]][[2]]

