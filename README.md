
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidylfs

<!-- badges: start -->

[![R-CMD-check](https://github.com/wklimowicz/tidylfs/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wklimowicz/tidylfs/actions/workflows/R-CMD-check.yaml)
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
`lfs_data_folder`). They should look like this:

    .
    └── lfs_data_folder/
        ├── 1992 Q1.sav
        ├── 1992 Q2.sav
        ├── ...
        └── 2020 Q2.sav

You can then run:

``` r
library(tidylfs)

# Converts `.sav` files to `.Rds` files, to save
# space and for quicker loading
lfs_convert(lfs_directory = "lfs_data_folder/",
            output_directory = "lfs_rds_folder/")

# Compiles into one file.
lfs <- lfs_compile(lfs_directory = "lfs_rds_folder/")
```

To reproduce official ONS publications, such as

- Unemployent [UNEM01
  NSA](https://www.ons.gov.uk/employmentandlabourmarket/peoplenotinwork/unemployment/datasets/unemploymentbyageanddurationnotseasonallyadjustedunem01nsa)

``` r
lfs %>%
    lfs_summarise_unemployment(QUARTER)
```

- Salary by occupation
  [EARN06](https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/grossweeklyearningsbyoccupationearn06)

``` r
lfs %>%
    dplyr::filter(
      !is.na(OCCUPATION_MAJOR), # Filter out NA's
      FTPTWK == "Full-time" # Take only full-time employees
    ) %>%
    lfs_summarise_salary(QUARTER, OCCUPATION_MAJOR)
```

Extending them is easy:

- Unemployment by quarter, sex, and age category

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

# Using the data across multiple projects

To avoid storing multiple copies of the compiled dataset, you can set
the `DATA_DIRECTORY` environment variable before compiling. This can be
done on the system or in `.Rprofile`:

``` r
Sys.setenv(DATA_DIRECTORY = "path/to/folder")
```

When compiling the dataset include the save_to_folder option as `TRUE`:

``` r
lfs_compile(lfs_directory = "lfs_rds_folder/", save_to_folder = TRUE)
```

The compiled dataset will be saved to the directory as an fst file, and
you can load it from anywhere using this command:

``` r
lfs <- lfs_load(data.table = FALSE)
```
