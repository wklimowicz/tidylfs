
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
`lfs_data_folder`). They should look like this (the naming convention is
important since that’s how the package knows which quarter and year the
data is from):

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
lfs_convert(
  directory = "lfs_data_folder/",
  output_directory = "lfs_rds_folder/"
)

# Compiles into one file.
lfs <- lfs_compile(directory = "lfs_rds_folder/")
```

To view the variable mapping use `variable_mapping()`:

``` r
variable_mapping(lfs)
```

Code to reproduce some official ONS publications is in the
`vignette("stats-replication")`. For more information on the ONS
variables, see the [LFS
Guidance](https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/methodologies/labourforcesurveyuserguidance).

A core set of variables is included by default. See `R/mappings.R` for
the complete set. To add more variables see
`vignette("adding-variables")`.

| Variable Name       | Definition                             |
|:--------------------|:---------------------------------------|
| YEAR                | Year                                   |
| QUARTER             | Quarter                                |
| SEX                 | Sex                                    |
| GOVTOR              | Government Office Region               |
| AGE                 | Age                                    |
| ETHNICITY           | Ethnicity                              |
| FTPTWK              | Part-Time/Full-time Status             |
| EDAGE               | Age when completed full time education |
| TTACHR              | Actual hours worked                    |
| HIQUALD             | Highest Qualification                  |
| DEGREE_SUBJECT      | Degree Subject                         |
| OCCUPATION          | Occupation in main job                 |
| PARENTAL_OCCUPATION | Parental Occupation at 14              |
