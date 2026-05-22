# Example usage of add_global_nc_attributes function
# With a data frame

# define locations of netcdf file to modify
nc_path <- "~/Documents/UniMelb/Wombat/Data/2026/WombatStateForest_2026_L1.nc"

attrs <- data.frame(
  name = c("Longitude", "Latitude", "Site_PI", "Contact"),
  value = c("144.52", "-37.43", "John Doe", "john@example.com")
)
result <- add_global_nc_attributes(nc_path = nc_path, attributes = attrs)

# Or with a named list
attrs <- list(
  Longitude = 144.52,
  Latitude = -37.43,
  Site_PI = "Jane Smith",
  Contact = "jane@example.com"
)
result <- add_global_nc_attributes(nc_path = nc_path, attributes = attrs)