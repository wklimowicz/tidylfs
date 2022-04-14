library(tidyverse)
library(tictoc)
library(data.table)
load_all()

setwd(here::here())

lfs_convert("../lfs_raw_data/", "../lfs_rds_data/")

lfs_compile("../lfs_rds_data/")

tic()
lfs <- lfs_load()
toc()

lfsdt <- data.table(lfs)

# Object size in RAM
lfsdt |>
  object.size() |>
  as.numeric() |>
  prettyunits::pretty_bytes()


tic()
fst::write_fst(lfsdt, "lfs.fst")
toc()

tic()
lfs <- fst::read_fst("lfs.fst", as.data.table = T)
toc()

# How long do people stay in sample


lfsdt[, .N, by = .(CASENO, QUARTER)][N == 1, .N, by = QUARTER] 

# CASENO only from 2001 Q3

lfsdt[CASENO != "" & YEAR > 2002][, quarters_in_sample := .N, by = CASENO
    ] [,.N, by = quarters_in_sample
     ][order(quarters_in_sample)][, prop := N / sum(N)] |>
  ggplot() +
    geom_col(aes(x = quarters_in_sample, y = prop)) +
    geom_vline(aes(xintercept = 5),
    size = 2, linetype = "dotted")

# TODO: Rewrite in data.table
# TODO: Save as fst

# Check what occupaitons senior leaders have year after

lfsdt[,`:=`(last_job = shift(OCCUPATION_DESCRIPTION, 1)), by = CASENO]


lfsdt[last_job %in% c("Senior professionals of educational establishments"), .N, by = OCCUPATION_DESCRIPTION
     ][order(-N)] |>
sie()


lfsdt[CASENO == " 96013130610101"]





lfs %>%
    lfs_summarise_unemployment(QUARTER, AGE, ETHNICITY, SEX)

lfsdt <- data.table(lfs)

setkey(lfsdt, QUARTER, CASENO)

lfsdt %>%
    dtlfs_summarise_unemployment(list(QUARTER, AGE, ETHNICITY, SEX))

dtlfs_summarise_unemployment <- function(lfs, ...) {

lfs[WEIGHT > 0 & !is.na(WEIGHT) & !is.na(ILODEFR),
      .(n = .N, 
      employed = mean((ILODEFR == "In employment") * WEIGHT, na.rm = TRUE),
      unemployed = mean((ILODEFR == "ILO unemployed") * WEIGHT, na.rm = TRUE),
      inactive = mean((ILODEFR == "Inactive") * WEIGHT, na.rm = TRUE)),
      , by = ...
      ][,
      `:=`(unemployed_percentage = unemployed / (employed + unemployed),
      employed_percentage = employed / (employed + unemployed + inactive),
      inactive_percentage = inactive / (employed + unemployed + inactive)),]

}
      
lfs %>%
    filter(LAST_OCCUPATION_DESCRIPTION ==
           "Secondary education teaching professionals") %>%
count(YEAR)

lfs %>%
    filter(LAST_OCCUPATION_DESCRIPTION ==
           "Secondary education teaching professionals") %>%
    count(OCCUPATION_DESCRIPTION, sort = T)

count(OCCUPATION_DESCRIPTION, sort = T)


lfs %>% count(LAST_OCCUPATION)

lfs %>% count(OCCUPATION_DESCRIPTION) %>% sie()



lfs <- lfs %>%
    arrange(QUARTER) %>%
    group_by(CASENO) %>%
    mutate(last_job = lag(OCCUPATION)) %>%
    ungroup()


  # Test Server
server_dir <- "//vmt1pr-spss2a/LFS/Master/Quarterly datasets/2004 onwards"
latest_quarter_filepath <- "//vmt1pr-spss2a/LFS/Master/Quarterly datasets/2004 onwards/2021 Q4.sav"

list.files(server_dir)

list.files(".")

lfs_convert(server_dir, "../lfs_raw_from_server")

lfs <- lfs_load()

lfs <- data.table(lfs)

setkey(lfs, YEAR, CASENO)

lfs

tic()
lfs[, last_job := c(NA, OCCUPATION_DESCRIPTION[-.N]), by = CASENO]
toc()

tic()
lfs[, last_job := shift(OCCUPATION_DESCRIPTION, 1), by = CASENO]
toc()

lfs[, .N, by = last_job]

lfs[OCCUPATION_DESCRIPTION == "Head teachers and principals"]

lfs[CASENO == " 96013040610101"]
lfs[CASENO == " 10711010210102"]


lfs[, .N , by =OCCUPATION_DESCRIPTION][order(-N)]

lfs["Head teachers and principals", .N, on = "OCCUPATION_DESCRIPTION", by = YEAR]
lfs["Senior professionals of educational establishments", .N, on = "OCCUPATION_DESCRIPTION", by = YEAR]
lfs["Registrars and senior administrators of educational establishments", .N, on = "OCCUPATION_DESCRIPTION", by = YEAR]

leader_descriptions <- c("Head teachers and principals",
"Senior professionals of educational establishments",
"Registrars and senior administrators of educational establishments")


lfs[, leader := OCCUPATION_DESCRIPTION %in% leader_descriptions & !is.na(CASENO)] 

lfs[, last_job_leader := shift(OCCUPATION_DESCRIPTION, 1) %in% leader_descriptions & !is.na(CASENO)] 


lfs[, .N, by = .(YEAR, leader_ever)]


lfs[, leader_ever := max(leader == 1), by = CASENO]


lfs[YEAR > 2010 & leader_ever == 1, .N, by = OCCUPATION_DESCRIPTION][order(-N)] 


lfs[YEAR > 2010 & last_job_leader == 1, .N, by = OCCUPATION_DESCRIPTION][order(-N)]  %>%
  sie()



lfs[last_job == 2317 & YEAR %in% c(2010:2020), .N, by = OCCUPATION_DESCRIPTION]

lfs[leader == 1, .N, by = OCCUPATION_DESCRIPTION]


lfsdt <- data.table(lfs)


tic()
lfs %>% 
    count(CASENO) %>%
 c   count(n)
toc()

# tic()
lfs[, .N, by = CASENO][,.N, by = N][order(-N)]
# toc()
tic()
lfsdt[, .N, by = CASENO][,.N, by = N][order(-N)]
toc()


lfsdt[QUARTER == "2021 Q3", .N, by = THISWV]
lfsdt[QUARTER == "2020 Q2", .N, by = THISWV]

