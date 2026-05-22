add2nc
==============

R-package to add or update global metadata attributes in NetCDF files


Provides a convenience function to add or update global metadata attributes in an existing NetCDF file. Supports any site-level or dataset-level attributes supplied as a named list or as a two-column data frame.


See 

	help(package = add2nc) 

for details on the functions provided by this package.

### Installation

Installation straight from github (if package "devtools" is already installed) via

```{r}
devtools::install_github("MarkusLoew/add2nc")
```

Installation under Windows might require the installation of Rtools.

### Example usage

```{r}
library(add2nc)

# import profile data
file.profile <- "~/FluxStorage.dat"
profile <- read.csv(file.profile)

# define locations of netcdf file to modify
nc_path <- "~/netcdf_file.nc"


# Example: add the profile storage term to the netcdf file, using TIMESTAMP vector
result <- add2nc(
  nc_path = nc_path,
  source_df = profile,
  source_time_col = "TIMESTAMP",
  vars_to_add = c("FC_Storage")
)

# Example (default: subtract 6 minutes from source timestamps) - this profile system records data 6 min after the general eddy covariance data
result <- add2nc(
  nc_path = nc_path,
  source_df = profile,
  source_time_col = "TIMESTAMP",
  vars_to_add = c("FC_Storage")
)

# Example (add 6 minutes instead)
# result <- add2nc(
#   nc_path = nc_path,
#   source_df = profile,
#   source_time_col = "TIMESTAMP",
#   vars_to_add = c("FC_Storage"),
#   time_offset_minutes = 6,
#   time_offset_direction = "add"
# )

# example with new_file = TRUE to create an updated copy of the netCDF file instead of modifying in place, otherwise an "*_updated.nc" file is created.
add2nc(
    nc_path = nc_path,
    source_df = profile,
    vars_to_add = c("FC_Storage"),
    new_var_names = c("FC_Storage"),
    longname = "CO2 storage from profile system",
    new_file = TRUE,
    output_nc_path = "~/output_updated.nc"
  )
```