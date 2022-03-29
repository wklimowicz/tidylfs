library(data.table)
library(tictoc)
load_all()

# lfs_compile("../lfs_rds_data/")

server_dir <- "//vmt1pr-spss2a/LFS/Master/Quarterly datasets/2004 onwards"
latest_quarter_filepath <- "//vmt1pr-spss2a/LFS/Master/Quarterly datasets/2004 onwards/2021 Q4.sav"

list.files(server_dir)

list.files(".")

lfs_convert(, "../lfs_raw_from_server")

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



library(tictoc)

tic()
lfs %>% 
    group_by(CASENO) %>%
    summarise(n = n())
toc()

# tic()
lfs[, .N, by = CASENO][,.N, by = N][order(-N)]
# toc()





select(OCCUPATION_DESCRIPTION, GRSSWK, INDUSTRY) 

