# Custom Mappings File

# Use this function to add extra variables to the LFS extract

# To include variables in order of priority (if they appear under different
# names in different LFS publications), use a case when:

# changing_name <- dplyr::case_when(
#   "Name_in_2004" %in% lfs_file_column_names ~ "Name_in_2004",
#   "Name_in_2010" %in% lfs_file_column_names ~ "Name_in_2010",
#   "Name_in_2020" %in% lfs_file_column_names ~ "Name_in_2020",
# )

# Use `factor` type when you want to import the spss annotations, and numeric
# when you just want the numbers


# For a comprehensive list of variables, see Volume 3 of the LFS User Guide
# https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/methodologies/labourforcesurveyuserguidance

extra_mappings <- function(lfs_file_column_names) {

  custom_variables <- tibble::tribble(
    # LFS Name,    Compiled Name,   Type
    ~lfs_name,       ~new_name,     ~type,
    # changing_name,   "New_Name",    "factor",
    "MARSEX6",           "SEX_AND_MARITIAL_STATUS",    "factor"
    )

  return(custom_variables)
}

# Run the function above and pass it as the argument to lfs_compile:

lfs_compile("your_lfs_rds_directory", extra_mappings = user_extra_mappings)
