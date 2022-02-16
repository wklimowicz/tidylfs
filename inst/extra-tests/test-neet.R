load_all()
library(tidyverse)

extra_mappings <- function(lfs_file_column_names) {

  custom_variables <- tibble::tribble(
    # LFS Name,    Compiled Name,   Type
    ~lfs_name,       ~new_name,     ~type,
    # changing_name,   "New_Name",    "factor",
    "NET", "NET",    "factor",
    "NEETS", "NEETS",    "factor"
    )

  return(custom_variables)
}

lfs_compile("../lfs_rds_data/", extra_mappings = extra_mappings)

lfs <- lfs_load()
setwd(here::here())

# NEET uses many different sources, so LFS figures won't match website(?)

lfs %>%
  filter(AGE %in% c(16:18)) %>%
  filter(YEAR == 2020) %>%
  filter(!is.na(NET)) %>%
  group_by(NET) %>%
  summarise(n = sum(WEIGHT)) %>%
  mutate(prop = n/sum(n))


