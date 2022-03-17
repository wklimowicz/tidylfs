library(tidyverse)
load_all()

lfs <- lfs %>%
    arrange(QUARTER) %>%
    group_by(CASENO) %>%
    mutate(last_job = lag(OCCUPATION)) %>%
    ungroup()

lfs %>%
    filter(last_job == 2317) %>%
    count(OCCUPATION_DESCRIPTION, sort = T)

library(data.table)

lfsdt <- data.table(lfs)

library(tictoc)

tic()
lfs %>% 
    group_by(CASENO) %>%
    summarise(n = n())
toc()

tic()
x = lfsdt[, .N, by = CASENO][,.N, by = N][order(-N)]
toc()





select(OCCUPATION_DESCRIPTION, GRSSWK, INDUSTRY) 

