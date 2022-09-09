setwd(here::here())
load_all()
library(tidyverse)
library(data.table)
library(tictoc)

lfs_convert("../lfs_raw_data/", "../lfs_rds_data/", incremental = TRUE)

tic()
lfs <- lfs_compile("../lfs_rds_data/", save_to_folder = TRUE)
toc()

source("data-raw/create_test_data.R")
check()

lfs_convert("../aps_raw_data/", "../aps_rds_data/", incremental = TRUE)

tic()
lfs <- lfs_compile("../aps_rds_data/",
                   save_to_folder = TRUE,
                   aps = TRUE)
toc()

source("data-raw/create_test_data.R")
check()


tic()
lfs_compile("../lfs_rds_data_test/")
toc()

lfs <- lfs_load()

lfs[HIQUALD == "Degree or equivalent",.N, by = .(YEAR, DEGREE_DESCRIPTION)] |>
ggplot() +
    geom_col(aes(x = YEAR, y = N, fill = DEGREE_DESCRIPTION))


lfs[YEAR > 2012 & HIQUALD == "Degree or equivalent",
    mean(GRSSWK, na.rm = T),
    by = .(DEGREE_DESCRIPTION)] 

lfs[HIQUALD == "Degree or equivalent",.N,DEGREE_DESCRIPTION
][,prop := N/sum(N) * 100][DEGREE_DESCRIPTION |> is.na()]


lfs[,.N, .(CMBDEG1, CMBDEG_MAIN)] |> sie()

dplyr::count(s, CMBDEG01, CMBDEG_MAIN) |> 
tidyr::pivot_wider(CMBDEG01,
                   names_from = CMBMAIN,
values_from = n)  |> sie()

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
