# tidylfs 0.1.1

# tidylfs 0.1.0

* `save_variables_report` has been deprecated. `lfs_compile()` attaches a `variable_mapping` attribute. Remove the argument and use `variable_mapping(lfs)` to view the mapping.

* `lfs_load()`, `aps_load()`, and `lfs_compile(save_to_folder)` have been deprecated in favour of explicitly writing the data after compilation.

* `aps` argument to `lfs_compile()` has been deprecated. Use `dataset = "aps"` instead.

* `lfs_summary_*` functions have been deprecated. The code for replicating ONS publications is in a vignettes, and should be copied over explicitly if used in analysis.

# tidylfs 0.0.3

* Data is now not saved by default after compilation, since updating the package purges the package directory.

* Setting the environment variable `DATA_DIRECTORY` to point to a folder will save the dataset there during compilation, so `lfs_load()` and `aps_load()` still work.

# tidylfs 0.0.2

* Changed final `lfs_data` to fst, with the option to load it as a data.table object.

* Added data.table methods to the summary functions

# tidylfs 0.0.1

* Initial release
