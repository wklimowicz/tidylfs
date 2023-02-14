setwd(here::here())
load_all()
library(tidyverse)
library(data.table)
library(tictoc)

lfs_convert("../lfs_raw_data/", "../lfs_rds_data/", incremental = TRUE)


user_extra_mappings <- function(lfs_file_column_names) {

degree_class <- pick_var(
c("DEGCLS7",
"DEGCLS"),
lfs_file_column_names
)

  custom_variables <- tibble::tribble(
    ~lfs_name,       ~new_name,     ~type,
    "DTEOFBTH", "DTEOFBTH", "unlabelled_factor",
    degree_class, "DEGCLS", "factor"
    )

  return(custom_variables)
}

tic()
lfs <- lfs_compile("../lfs_rds_data/",
                   save_to_folder = TRUE,
                   extra_mappings = user_extra_mappings)
toc()


# Short version
# tic()
# lfs <- lfs_compile("../lfs_rds_data_test/", save_to_folder = TRUE)
# toc()

source("data-raw/create_test_data.R")
check()


# Degree Subject Testing
lfs[,.N, .(is.na(DEGREE_DESCRIPTION),QUARTER)] |> 
  dcast(QUARTER ~ is.na) |> sie()

lfs[QUARTER == "1997 Q3", .N, DEGREE_DESCRIPTION]

lfs[DEGREE_DESCRIPTION %ilike% "art",
    .(
      V1 = median(GRSSWK, na.rm = TRUE),
      N = .N
      ), YEAR] |>
  ggplot() +
  geom_line(aes(x = YEAR, y = V1)) +
  geom_vline(aes(xintercept = 1997)) +
  geom_vline(aes(xintercept = 2004))


lfs[!is.na(DEGREE_SUBJECT) , .N, .(SUBJECT_DESCRIPTION, DEGREE_SUBJECT)] |> sie()

lfs[is.na(SUBJECT_DESCRIPTION) & !is.na(DEGREE_SUBJECT), .N, .(QUARTER)] 


tic()
lfs <- lfs_compile("../lfs_rds_data_2020/")
toc()

lfs <- lfs_load()

lfs[is.na(CASENO), .N, YEAR]
lfs[,.N,.(YEAR, is.na(WARD))]
lfs[,.N,.(YEAR, is.na(PCON9D))]
lfs[,.N,.(YEAR, is.na(TTWA))]
lfs[,.N,.(YEAR, is.na(CTY))]

lfs[,.N,.(CTY)]

lfs[,.N,.(YEAR,HEALTH)] |>
dcast(YEAR ~ HEALTH)

lfs[,.N,DEGREE71]
lfs[,.N,INECAC05]
lfs[,.N,LAST_OCCUPATION_DESCRIPTION]
lfs[,.N,OCCUPATION_DESCRIPTION]
lfs[,.N,INDUSTRY_DESCRIPTION]

lfs[,.N,.(YEAR,DEGREE71)] |>
dcast(YEAR ~ DEGREE71)

lfs[,.N,CMBDEGREE]
lfs[,.N,QUARTER]
lfs[,.N,INDUSTRY]

lfs$OCCUPATION_DESCRIPTION

table(lfs$PARENTAL_OCCUPATION)
table(lfs$OCCUPATION)

table(lfs$OCCUPATION_DESCRIPTION)
table(lfs$PARENTAL_OCCUPATION_DESCRIPTION)

lfs %>%
  lfs_summarise_salary(OCCUPATION, OCCUPATION_DESCRIPTION) %>%
  sie()

lfs %>%
filter(OCCUPATION_DESCRIPTION %in%
c(
"Business and related research professionals",
"Business, research and administrative professionals n.e.c."
)) %>%
lfs_summarise_salary(DEGREE_SUBJECT)

lfs %>%
  filter(DEGREE_SUBJECT == "Music") %>%
  lfs_summarise_salary(OCCUPATION_DESCRIPTION) %>%
  sie()


sie()


table(lfs$OCCUPATION_DESCRIPTION)

names(lfs)

lfs %>%
  lfs_flag_teacher() %>%
  filter(!is.na(PARENTAL_OCCUPATION_MAJOR)) %>%
  filter(TEACHER == 1) %>%
  count(QUARTER, OCCUPATION_MAJOR, PARENTAL_OCCUPATION_MAJOR) %>%
  arrange(desc(n))

lfs %>%
  count(QUARTER, !is.na(PARENTAL_OCCUPATION)) %>%
  sie()

lfs %>%
  count(QUARTER, PARENTAL_OCCUPATION)

lfs %>%
  lfs_flag_teacher() %>%
  filter(teacher
  filter(YEAR %in% c(2010:2019)) %>%
  # filter(HIGHO %in% c("Post grad cert in educ", "Post grad cert in education")) %>%
  group_by(TEACHER) %>%
  count(HIGHO, sort = T)



table(lfs$QUARTER, lfs$COUNTRY)

table(lfs$QUARTER, (lfs$ILODEFR))
table(lfs$QUARTER, as.numeric(lfs$ILODEFR))
table(lfs$QUARTER, (lfs$FTPT))

table(lfs$QUARTER, lfs$SEX)
table(lfs$QUARTER, lfs$HIQUALD)
table(lfs$QUARTER, lfs$DEGREE71)

lfs[QUARTER %ilike% "Q4"] |>
  lfs_summarise_union(QUARTER, SEX) 

lfs |>
  lfs_summarise_hours(QUARTER, SEX) 

  dplyr::select(SEX, QUARTER, union_percentage)


table(lfs$ILODEFR)
table(lfs$QUARTER, lfs$FTPT)


lfs %>%
  dplyr::select(ILODEFR) %>%
  dplyr::filter(!is.na(ILODEFR)) %>%
  dplyr::mutate(as.numeric(ILODEFR))



lfs %>%
  filter(FTPTWK == "Full-time") %>%
  lfs_flag_teacher() %>%
  lfs_summarise_hours(YEAR, TEACHER, SEX) %>%
  ggplot() +
  geom_line(aes(x = YEAR, y = hours, color = TEACHER, group = interaction(TEACHER, SEX), linetype = SEX))



lfs[, .N, CASENO2
    ][, .(N2 = .N), keyby = N
    ][, prop := N2/sum(N2)
    ][N %in% 1:5, sum(prop)]

lfs[, N := .N, CASENO]

lfs[, CASENO2 := paste0(DTEOFBTH, CASENO)]

lfs[CASENO == 22311930210102]

