# load_all()
library(tidyverse)

setwd(here::here())

labels_to_data_frame <- function(lab) {
x <- haven::print_labels(lab)
x2 <- attributes(x)$labels
name <- attributes(x)$labels
nx <- attributes(name) %>%
    as.data.frame()

z <- data.frame(DEGREE = x2, DEGREE_DESCRIPTION = nx$names) 
return(z)
}

s <- haven::read_sav("../data_lfs_raw/1993 Q2.sav")
z <- labels_to_data_frame(s$subjct1)

z %>%
    readr::write_csv("inst/coding_frames/sngdeg_1992_2_digit.csv")

s <- haven::read_sav("../data_lfs_raw/1998 Q4.sav")
z <- labels_to_data_frame(s$sngdeg)

z %>%
    readr::write_csv("inst/coding_frames/sngdeg_1997.csv")

s <- haven::read_sav("../data_lfs_raw/2005 Q1.sav")
z <- labels_to_data_frame(s$SNGDEG)

z %>%
    readr::write_csv("inst/coding_frames/sngdeg_2004.csv")

