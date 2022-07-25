# tidylfs 0.0.3

* Data is now not saved by default after compilation, since updating the package purges the package directory.

* Setting the environment variable `DATA_DIRECTORY` to point to a folder will save the dataset there during compilation, so `lfs_load()` and `aps_load()` still work.

# tidylfs 0.0.2

* Changed final `lfs_data` to fst, with the option to load it as a data.table object.

* Added data.table methods to the summary functions

# tidylfs 0.0.1

* Initial release
