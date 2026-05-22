#' Add or update global attributes in a NetCDF file
#'
#' Purpose:
#' - Add or update global metadata attributes in an existing NetCDF file.
#' - Support common site-level attributes (Longitude, Latitude, Site PI, Contact, etc.).
#' - Optionally create a new NetCDF file with the updated attributes.
#'
#' Process overview:
#' 1. Validate required inputs (attributes data structure).
#' 2. Optionally copy the input NetCDF file to a new location.
#' 3. Open NetCDF file in write mode.
#' 4. Add or overwrite global attributes using ncatt_put.
#' 5. Return a tibble with diagnostics on attributes added.
#'
#' @param nc_path Character scalar. Path to the existing NetCDF file to modify.
#' @param attributes Data frame or named list containing global attributes to add.
#'   If data frame: must have columns `name` and `value`.
#'   If named list: names become attribute names, values become attribute values.
#' @param new_file Logical scalar. If `TRUE`, a new NetCDF file is created for output.
#'   If `FALSE`, the existing file at `nc_path` is modified in place.
#' @param output_nc_path Character scalar or `NULL`. Path for the output NetCDF file.
#'   Required if `new_file` is `TRUE`. If `NULL`, defaults to `<nc_path>_updated.nc`.
#'
#' @return A tibble with columns:
#' - `attribute_name`: name of the global attribute
#' - `attribute_value`: value of the global attribute
#' - `status`: "added" or "updated" indicating whether the attribute was new or overwritten
#' - `output_nc_path`: path to the output NetCDF file
#'
#' @examples
#' # Example with data frame
#' attrs_df <- data.frame(
#'   name = c("longitude", "latitude", "site_pi", "contact"),
#'   value = c("144.5234", "-37.4321", "John Doe", "john@example.com")
#' )
#' result <- add_global_nc_attributes(
#'   nc_path = "my_file.nc",
#'   attributes = attrs_df,
#'   new_file = TRUE,
#'   output_nc_path = "my_file_updated.nc"
#' )
#'
#' # Example with named list
#' attrs_list <- list(
#'   longitude = 144.5234,
#'   latitude = -37.4321,
#'   site_pi = "Jane Smith",
#'   contact = "jane@example.com"
#' )
#' result <- add_global_nc_attributes(
#'   nc_path = "my_file.nc",
#'   attributes = attrs_list
#' )
add_global_nc_attributes <- function(
  nc_path,
  attributes,
  new_file = TRUE,
  output_nc_path = NULL
) {

  # Load required packages
  required_packages <- c("ncdf4", "dplyr", "tibble")
  lapply(required_packages, require, character.only = TRUE)

  # Convert attributes to standardized format (data frame with name and value columns)
  if (is.list(attributes) && !is.data.frame(attributes)) {
    # Named list: convert to data frame
    if (is.null(names(attributes))) {
      stop("If attributes is a list, it must be a named list")
    }
    attr_df <- tibble(
      name = names(attributes),
      value = as.character(unlist(attributes))
    )
  } else if (is.data.frame(attributes)) {
    # Data frame: validate structure
    if (!all(c("name", "value") %in% names(attributes))) {
      stop("attributes data frame must have columns 'name' and 'value'")
    }
    attr_df <- tibble(
      name = attributes$name,
      value = as.character(attributes$value)
    )
  } else {
    stop("attributes must be a data frame with columns 'name' and 'value', or a named list")
  }

  # Validate that no attribute names are empty
  if (any(!nzchar(as.character(attr_df$name)))) {
    stop("Attribute names cannot be empty")
  }

  # Get NetCDF file to work with
  nc_work_path <- nc_path

  # Check for new file argument and create copy
  if (isTRUE(new_file)) {
    if (is.null(output_nc_path)) {
      output_nc_path <- sub("\\.nc$", "_updated.nc", nc_path)
    }

    file.copy(nc_path, output_nc_path, overwrite = TRUE)
    nc_work_path <- output_nc_path
  }

  # Open target NetCDF in write mode; close safely on function exit
  nc <- nc_open(nc_work_path, write = TRUE)
  on.exit(nc_close(nc), add = TRUE)

  # Get existing global attributes to determine if we're adding or updating
  existing_attrs <- names(ncatt_get(nc, 0))

  # Add or update each global attribute
  status_list <- vector("character", nrow(attr_df))

  for (i in seq_len(nrow(attr_df))) {
    attr_name <- attr_df$name[[i]]
    attr_value <- attr_df$value[[i]]

    # Determine if this is a new or updated attribute
    if (attr_name %in% existing_attrs) {
      status_list[[i]] <- "updated"
    } else {
      status_list[[i]] <- "added"
    }

    # Write the global attribute
    ncatt_put(nc, 0, attr_name, attr_value)
  }

  # Return a compact diagnostics table
  tibble(
    attribute_name = attr_df$name,
    attribute_value = attr_df$value,
    status = status_list,
    output_nc_path = nc_work_path
  )
}

