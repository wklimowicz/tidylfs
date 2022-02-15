library(tidylfs)
library(tidyverse)

extra_mappings <- function(lfs_file_column_names) {

  custom_variables <- tibble::tribble(
    # LFS Name,    Compiled Name,   Type
    ~lfs_name,       ~new_name,     ~type,
    # changing_name,   "New_Name",    "factor",
    "APPRCURR", "APPRENTICESHIP",    "factor"
    )

  return(custom_variables)
}

lfs_compile("../lfs_rds_data/", extra_mappings = extra_mappings)

lfs <- lfs_load()
setwd(here::here())

# NEET uses many different sources

lfs %>%
  filter(AGE %in% c(16:18)) %>%
  filter(YEAR == 2020) %>%
  mutate(NEET = (
         ILODEFR == "In employment" |
         APPRENTICESHIP == "Yes" |
         EDAGE == 96
)
  ) %>%
  # filter(!is.na(NEET)) %>%
  group_by(NEET) %>%
  summarise(s = sum(WEIGHT)) %>%
  mutate(prop = s/sum(s))


