library(tidyverse)
library(tictoc)
library(data.table)
load_all()

tic()
lfs <- lfs_load()
toc()

lfs_compile("../lfs_rds_data/")

fst::write_fst(lfs, "lfs.fst")

tic()
lfs <- fst::read_fst("lfs.fst")
toc()


# TODO: Rewrite in data.table
# TODO: Save as fst

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
    filter(!is.na(LAST_OCCUPATION)) %>%
    count(YEAR)

count(OCCUPATION_DESCRIPTION, sort = T)


lfs %>% count(LAST_OCCUPATION)

lfs %>% count(OCCUPATION_DESCRIPTION) %>% sie()



lfs <- lfs %>%
    arrange(QUARTER) %>%
    group_by(CASENO) %>%
    mutate(last_job = lag(OCCUPATION)) %>%
    ungroup()

lfs %>%
    filter(last_job == 2317) %>%
    count(OCCUPATION_DESCRIPTION, sort = T)

lfsdt <- data.table(lfs)


tic()
lfs %>% 
    count(CASENO) %>%
 c   count(n)
toc()

tic()
lfsdt[, .N, by = CASENO][,.N, by = N][order(-N)]
toc()





select(OCCUPATION_DESCRIPTION, GRSSWK, INDUSTRY) 

