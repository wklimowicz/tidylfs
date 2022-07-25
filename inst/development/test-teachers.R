devtools::load_all()
library(tidyverse)

lfs <- lfs_load()

lfs <- lfs %>%
  lfs_flag_teacher()

lfs %>%
  filter(FTPTWK == "Full-time") %>%
  filter(YEAR > 2015) %>%
  # filter(TEACHER == 1) %>%
  lfs_summarise_hours(TEACHER, QUARTER)


lfs %>%
  filter(YEAR > 2015) %>%
  filter(FTPTWK == "Full-time") %>%
  # filter(TEACHER == 1) %>%
  group_by(TEACHER, YEAR) %>%
  summarise(n = n(),
            hours = mean(TTUSHR, na.rm = T))


lfs %>%
  filter(TEACHER == 1) %>%
  filter(QUARTER == "2020 Q4") %>%
  count(UNION)

lfs %>%
  filter(YEAR == 2021) %>%
  count(TEACHER)

809/(129 + 809)


lfs %>%
  filter(YEAR > 2005) %>%
  # filter(TEACHER == 1) %>%
  # filter(!GOVTOF %in% c("Scotland", "Northern Ireland")) %>%
  count(YEAR, ETHNICITY) %>%
  mutate(prop = n/sum(n)) %>%
  sie()

lfs %>%
  filter(!is.na(PARENTAL_OCCUPATION_DESCRIPTION)) %>%
  filter(YEAR > 2015) %>%
  filter(TEACHER == 1) %>%
  count(PARENTAL_OCCUPATION_DESCRIPTION, sort = T)

  unem01 <- lfs %>%
    dplyr::filter(YEAR > 1995) %>%
    lfs_summarise_unemployment(QUARTER) %>%
    dplyr::select(QUARTER, unemployed_percentage) %>%
    dplyr::mutate(unemployed_percentage = unemployed_percentage * 100) %>%
    dplyr::filter(!is.na(unemployed_percentage))

# lfs %>%
#   filter(!is.na(OCCUPATION_LAST_JOB_DESCRIPTION)) %>%
#   filter(YEAR > 2015) %>%
#   filter(HIGHO %in% c("Post grad cert in educ", "Post grad cert in education")) %>%
#   count(OCCUPATION_LAST_JOB_DESCRIPTION, sort = T)
plot(rnorm(100))

lfs %>%
  filter(!is.na(OCCUPATION_DESCRIPTION)) %>%
  filter(YEAR > 2015) %>%
  filter(TEACHER == 0) %>%
  filter(HIGHO %in% c("Post grad cert in educ", "Post grad cert in education")) %>%
  count(OCCUPATION_DESCRIPTION, sort = T)



lfs %>%
  # filter(YEAR > 2015) %>%
  filter(TEACHER == FALSE) %>%
  filter(HIGHO %in% c("Post grad cert in educ", "Post grad cert in education")) %>%
  lfs_summarise_unemployment(YEAR) %>%
  ggplot(aes(x = YEAR, y = unemployed_percentage)) +
  geom_line()

lfs %>%
  filter(YEAR > 2015) %>%
  filter(TEACHER == FALSE) %>%
  filter(HIGHO %in% c("Post grad cert in educ", "Post grad cert in education")) %>%
  lfs_summarise_salary(YEAR)



