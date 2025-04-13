# Set up Dataset ---------------------------------------------

load_all()
# library(tidylfs);
library(tidyverse)

# Choose extra columns for NEET
extra_mappings <- function(lfs_file_column_names) {

  custom_variables <- tibble::tribble(
     ~lfs_name,   ~new_name,     ~type,
     "NET",       "NET",        "numeric",
     "NEETS",     "NEETS",      "numeric",
     "APPRCURR",  "APPRCURR",   "numeric",
     "SCHM12",    "SCHM12",     "numeric",
     "QULNOW",    "QULNOW",     "numeric",
     "ED4WK",     "ED4WK",      "numeric",
     "ENROLL",    "ENROLL",     "numeric",
     "ATTEND",    "ATTEND",     "numeric",
     "AGEDFE",    "AGEDFE",     "numeric",
     "DURUN2",    "DURUN2",     "numeric"
    )

  return(custom_variables)
}

# Convert all to RDS
# lfs_convert("../data_lfs_raw/", "../data_lfs_rds")

# # Compile into one dataset
# lfs_compile("../data_lfs_rds/", extra_mappings = extra_mappings)

lfs <- lfs_load()

# Set up NEET Definitions ---------------------------------------------

neet_raw <- lfs %>%
  filter(between(AGEDFE, 16, 24)) %>%
  filter(COUNTRY == "England") %>%
  # filter(between(GOVTOR, 1, 16)) %>%
  select(QUARTER,
         GOVTOR,
         APPRCURR,
         SCHM12,
         QULNOW,
         ED4WK,
         ENROLL,
         ATTEND,
         AGE,
         AGEDFE,
         ILODEFR,
         INECAC05,
         CURED,
         DURUN2,
         SEX,
         WEIGHT)

neet_processing <- neet_raw %>%
  mutate(NET = case_when(
  APPRCURR == 1 ~ "In education/training",
  between(SCHM12, 1, 50) ~ "In education/training",
  QULNOW == 1 ~ "In education/training",
  ED4WK == 1 ~ "In education/training",
  ENROLL == 1 & between(ATTEND, 1, 2) ~ "In education/training",
  ENROLL == 1 & ATTEND == 3 ~ "Not in education/training",
  ENROLL == 1 & ATTEND == -8 ~ "Missing",
  ENROLL == 1 ~ "In education/training",
  APPRCURR == -8 ~ "Missing",
  QULNOW == -8 ~ "Missing",
  ENROLL == -8 ~ "Missing",
  ED4WK == -8 ~ "Missing",
  AGE == 16 & SCHM12 == 97 ~ "Missing",
  AGE == 16 & SCHM12 == -9 ~ "Missing",
  TRUE ~ "Not in education/training")) %>%
  filter(NET != "Missing")


# Create publication groupings ------------------------------

neet_final <- neet_processing %>%
  mutate(AGEDFE = as.character(AGEDFE)) %>%
  mutate(AGE1617 = case_when(
    AGEDFE %in% 16:17 ~ "16-17",
    TRUE ~  NA_character_
    ),
         AGE1618 = case_when(
    AGEDFE %in% 16:18 ~ "16-18",
    TRUE ~  NA_character_
    ),
         AGE1624 = case_when(
    AGEDFE %in% 16:24 ~ "16-24",
    TRUE ~  NA_character_
    ),
         AGE1824 = case_when(
    AGEDFE %in% 18:24 ~ "18-24",
    TRUE ~  NA_character_
    ),
         AGE1924 = case_when(
    AGEDFE %in% 19:24 ~ "19-24",
    TRUE ~  NA_character_
    ),
  )

# List of groupings that show up ---------------------------
combinations <- list(
                    c("AGEDFE"),
                    c("AGE1617"),
                    c("AGE1617", "SEX"),
                    c("AGE1618"),
                    c("AGE1618", "SEX"),
                    c("AGE1624"),
                    c("AGE1624", "GOVTOR"),
                    c("AGE1624", "SEX"),
                    c("AGE1824"),
                    c("AGE1824", "GOVTOR"),
                    c("AGE1824", "SEX"),
                    c("AGE1924"),
                    c("AGE1924", "GOVTOR"),
                    c("AGE1924", "SEX")
)

# Function to map over the groupings
neet_proportions <- function(neet_lfs, ...) {
  groupings <- rlang::syms(...)

  neet_lfs %>%
    filter(
           if_any(# Should this be if_all?
            .cols = !!!groupings,
            .fns = ~ !is.na(.x)
            )
           ) %>%
    group_by(QUARTER, !!!groupings) %>%
    summarise(
              percent_net = sum((NET == "Not in education/training") * WEIGHT) / sum(WEIGHT),
              population = sum(WEIGHT),
              # number_NEET = sum(NET == "Not in Education"...
              ) %>%
    ungroup()


}

# Map over each grouping
neet_summary <- map(combinations, neet_proportions, neet_lfs = neet_final) %>%
  bind_rows()

# Group the Ages into one column, replace NA with Total
neet <- neet_summary %>%
  # filter(QUARTER == "2020 Q4") %>%
  mutate(AGE = case_when(
    !is.na(AGEDFE) ~ AGEDFE,
    !is.na(AGE1617) ~ AGE1617,
    !is.na(AGE1618) ~ AGE1618,
    !is.na(AGE1624) ~ AGE1624,
    !is.na(AGE1824) ~ AGE1824,
    !is.na(AGE1924) ~ AGE1924,
    TRUE ~ NA_character_
    )) %>%
  mutate(AGE = ifelse(is.na(AGE), "Total", AGE)) %>%
  mutate(SEX = ifelse(is.na(SEX), "Total", SEX)) %>%
  mutate(GOVTOR = ifelse(is.na(GOVTOR), "Total", GOVTOR)) %>%
  select(-starts_with(c("AGE1", "AGEDFE"))) %>%
  relocate(QUARTER, GOVTOR, SEX, AGE) %>%
  arrange(desc(QUARTER))

write_csv(neet, "neet_estimates_2000_2021.csv")

