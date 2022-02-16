user_extra_mappings <- function(lfs_file_column_names) {

  custom_variables <- tibble::tribble(
    ~lfs_name,       ~new_name,     ~type,
    "MARSEX6", "SEX_AND_MARITIAL_STATUS", "factor"
    )

  return(custom_variables)
}

# Add variables, run the function above, and pass it as the argument to lfs_compile:

lfs_compile("your_lfs_rds_directory", extra_mappings = user_extra_mappings)
