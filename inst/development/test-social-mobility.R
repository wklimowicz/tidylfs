load_all()
library(tidyverse)


lfs <- lfs_load()

lfs %>%
  filter(PARENTAL_OCCUPATION_MAJOR %in% c(1:9)) %>%
  filter(!is.na(OCCUPATION_MAJOR), !is.na(PARENTAL_OCCUPATION_MAJOR)) %>%
  group_by(OCCUPATION_MAJOR, PARENTAL_OCCUPATION_MAJOR) %>%
  count() %>%
  group_by(OCCUPATION_MAJOR) %>%
  mutate(prop = n/sum(n)) %>%
  pivot_wider(id_cols = 1, names_from = 2, values_from = prop) %>%
  sie()


lfs %>%
  filter(OCCUPATION_DESCRIPTION == "Actuaries, economists and statisticians") %>%
  count(PARENTAL_OCCUPATION_DESCRIPTION, sort = T)

lfs %>%
  lfs_flag_teacher() %>%
  filter(TEACHER == 1) %>%
  count(YEAR, PARENTAL_OCCUPATION_DESCRIPTION, sort = T) %>%
  filter(!is.na(PARENTAL_OCCUPATION_DESCRIPTION)) %>%
  mutate(prop = n/sum(n))

  filter() %>%
  filter(COUNTRY == "England") %>%
  filter(AGE %in% 16:64) %>%
  filter(GRADUATE == 1) %>%
  filter(POSTGRADUATE == 0) %>%
  lfs_summarise_unemployment()

lfs %>%
    lfs_summarise_unemployment(QUARTER)

  lfs %>%
  lfs_flag_teacher() %>%
  lfs_summarise_hours(TEACHER)

lfs %>%
  lfs_flag_teacher() %>%
  lfs_summarise_hours(TEACHER)

#s
#'s



 # [1] "YEAR"
 # [2] "QUARTER"
 # [3] "SEX"
 # [4] "GOVTOR"
 # [5] "AGE"
 # [6] "FTPTWK"
 # [7] "EDAGE"
 # [8] "TTUSHR"
 # [9] "TTACHR"
# [10] "TOTUS1"
# [11] "TOTAC1"
# [12] "ACTHR"
# [13] "TOTAC2"
# [14] "TOTHRS"
# [15] "WEIGHT"
# [16] "INECAC05"
# [17] "CURED"
# [18] "QUOTA"
# [19] "WEEK"
# [20] "W1YR"
# [21] "QRTR"
# [22] "ADD"
# [23] "WAVFND"
# [24] "HHLD"
# [25] "RECNO"
# [26] "UNION"
# [27] "GRSSWK"
# [28] "BUSHR"
# [29] "WEIGHT_INCOME"
# [30] "HIGHO"
# [31] "HOURPAY"
# [32] "PUBLIC"
# [33] "BACTHR"
# [34] "TUPRES"
# [35] "ILODEFR"
# [36] "INDUSTRY"
# [37] "SUMHRS"
# [38] "HIQUALD"
# [39] "HIQUAL"
# [40] "DEGREE71"
# [41] "TUCOV"
# [42] "DEGREE_SUBJECT"
# [43] "CMBDEGREE"
# [44] "FDSICO"
# [45] "GOVTOF"
# [46] "AGES"
# [47] "COUNTRY"
# [48] "OCCUPATION"
# [49] "OCCUPATION_MAJOR"
# [50] "OCCUPATION_LAST_JOB"
# [51] "ETHNICITY"
# [52] "DEGREE72"
# [53] "DEGREE73"
# [54] "DEGREE74"
# [55] "DEGREE75"
# [56] "DISABILITY"
# [57] "PARENTAL_OCCUPATION"
# [58] "PARENTAL_OCCUPATION_MAJOR"
# [59] "OCCUPATION_DESCRIPTION"
# [60] "PARENTAL_OCCUPATION_DESCRIPTION"
# [61] "OCCUPATION_LAST_JOB_DESCRIPTION"
