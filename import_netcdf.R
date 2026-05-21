source("add2nc.R")

# import profile data
file.profile <- "~/Documents/UniMelb/Wombat/Profile_system/Data/Wombat_AP200_FluxStorage.dat"
profile <- CampbellFileImport(file.profile)


# files.nc <- c(
#     "~/Downloads/L6/2025/WombatForest1_L6.nc",
#     "~/Downloads/L6/2025/WombatForest2_L6.nc"
# )

# define locations of netcdf file to modify
nc_path <- "~/Documents/UniMelb/Wombat/Data/2026/WombatStateForest_2026_L1.nc"


# Example: add one or more profile vectors by timestamp match
result <- add2nc(
  nc_path = nc_path,
  source_df = profile,
  source_time_col = "TIMESTAMP",
  vars_to_add = c("FC_Storage")
)

# Example (default: subtract 6 minutes from source timestamps)
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

# example with new_file = TRUE to create an updated copy of the netCDF file instead of modifying in place
Add2nc(
    nc_path = nc_path,
    source_df = profile,
    vars_to_add = c("FC_Storage"),
    new_var_names = c("FC_Storage"),
    longname = "CO2 storage from profile system",
    new_file = TRUE,
    output_nc_path = "~/Documents/UniMelb/Wombat/Data/2026/WombatStateForest_2026_L1_updated.nc"
  )

ggplot(profile, aes(x = TIMESTAMP, y = FC_Storage)) +
  geom_line() +
  geom_point() +
  theme_bw()