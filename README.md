
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidylfs

<!-- badges: start -->

[![R-CMD-check](https://github.com/wklimowicz/tidylfs/workflows/R-CMD-check/badge.svg)](https://github.com/wklimowicz/tidylfs/actions)
<!-- badges: end -->

## Installation

You can install the development version of tidylfs from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("wklimowicz/tidylfs")
```

## Quickstart

Copy the LFS `.sav` files into a directory (in my example
`lfs_data_folder`) and run:

``` r
library(tidylfs)

# Converts `.sav` files to `.Rds` files, to save
# space and for quicker loading
lfs_convert("lfs_data_folder/", "lfs_rds_folder/")

# Compiles into one tidy `.fst` file.
lfs_compile("lfs_rds_folder/")
```

To load the data into your environment run:

``` r
lfs <- lfs_load(data.table = FALSE)
```

(Use data.table = TRUE if you know it, it’s much faster.)

To reproduce official ONS publications, such as

-   Unemployent [UNEM01
    NSA](https://www.ons.gov.uk/employmentandlabourmarket/peoplenotinwork/unemployment/datasets/unemploymentbyageanddurationnotseasonallyadjustedunem01nsa)

``` r
lfs %>%
    lfs_summarise_unemployment(QUARTER)
```

-   Salary by occupation
    [EARN06](https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/grossweeklyearningsbyoccupationearn06)

``` r
lfs %>%
    dplyr::filter(!is.na(OCCUPATION_MAJOR)) %>% # Filter out NA's
    dplyr::filter(FTPTWK == "Full-time") %>% # Take only full-time employees
    lfs_summarise_salary(QUARTER, OCCUPATION_MAJOR) %>%
    tidyr::pivot_wider(id_cols = QUARTER,
                       names_from = OCCUPATION_MAJOR,
                       values_from = mean_weekly_pay)
```

Extending them is easy:

-   Unemployment by quarter, sex, and age category

``` r
lfs %>%
  lfs_summarise_unemployment(QUARTER, SEX, AGES)
```

For more information on the ONS variables, see the [LFS
Guidance](https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/methodologies/labourforcesurveyuserguidance).

Some of the variables included by default are:

| Variable Name       | Definition                             |
|:--------------------|:---------------------------------------|
| YEAR                | Year                                   |
| QUARTER             | Quarter                                |
| SEX                 | Sex                                    |
| GOVTOR              | Government Office Region               |
| AGE                 | Age                                    |
| FTPTWK              | Part-Time/Full-time Status             |
| EDAGE               | Age when completed full time education |
| TTACHR              | Actual hours worked                    |
| UNION               | In union?                              |
| HIQUALD             | Highest Qualification                  |
| DEGREE_SUBJECT      | Degree Subject                         |
| OCCUPATION          | Occupation in main job                 |
| PARENTAL_OCCUPATION | Parental Occupation at 14              |
| ETHNICITY           | Ethnicity                              |
