#' Add or update global attributes in a NetCDF file
#'
#' @description
#' Adds or updates global metadata attributes in an existing NetCDF file.
#' Supports any site-level or dataset-level attributes supplied as a named
#' list or as a two-column data frame.
#'
#' @details
#' **Process overview:**
#' 1. Validate and normalise the `attributes` argument into a name/value table.
#' 2. Optionally copy the input NetCDF file to `output_nc_path`.
#' 3. Open the NetCDF file in write mode.
#' 4. Add or overwrite each global attribute with [ncdf4::ncatt_put()].
#' 5. Return a tibble summarising what was written.
#'
#' @param nc_path Character scalar. Path to the existing NetCDF file.
#' @param attributes Data frame or named list of global attributes to add.
#'   - **Data frame**: must have columns `name` and `value`.
#'   - **Named list**: names become attribute names, values become attribute values.
#' @param new_file Logical scalar. If `TRUE`, a copy of `nc_path` is written to
#'   `output_nc_path` and that copy is modified. If `FALSE`, `nc_path` is
#'   modified in place. Default `TRUE`.
#' @param output_nc_path Character scalar or `NULL`. Destination path when
#'   `new_file = TRUE`. If `NULL` the path is derived by appending `_updated`
#'   before the `.nc` extension of `nc_path`.
#'
#' @return A [tibble::tibble()] with one row per attribute containing:
#' \describe{
#'   \item{attribute_name}{Name of the global attribute.}
#'   \item{attribute_value}{Value written to the global attribute.}
#'   \item{status}{`"added"` if the attribute was new; `"updated"` if it already
#'     existed.}
#'   \item{output_nc_path}{Path to the (possibly new) output NetCDF file.}
#' }
#'
#' @examples
#' \dontrun{
#' # Data-frame interface
#' attrs_df <- data.frame(
#'   name  = c("longitude", "latitude", "site_pi", "contact"),
#'   value = c("144.5234", "-37.4321", "John Doe", "john@example.com")
#' )
#' result <- add_global_nc_attributes(
#'   nc_path        = "my_file.nc",
#'   attributes     = attrs_df,
#'   new_file       = TRUE,
#'   output_nc_path = "my_file_updated.nc"
#' )
#'
#' # Named-list interface
#' attrs_list <- list(
#'   longitude = 144.5234,
#'   latitude  = -37.4321,
#'   site_pi   = "Jane Smith",
#'   contact   = "jane@example.com"
#' )
#' result <- add_global_nc_attributes(
#'   nc_path    = "my_file.nc",
#'   attributes = attrs_list
#' )
#' }
#'
#' @importFrom ncdf4 nc_open nc_close ncatt_get ncatt_put
#' @importFrom tibble tibble
#' @export
add_global_nc_attributes <- function(
  nc_path,
  attributes,
  new_file = TRUE,
  output_nc_path = NULL
) {

  # Convert attributes to standardized format (data frame with name and value columns)
  if (is.list(attributes) && !is.data.frame(attributes)) {
    if (is.null(names(attributes))) {
      stop("If attributes is a list, it must be a named list")
    }
    attr_df <- tibble::tibble(
      name  = names(attributes),
      value = as.character(unlist(attributes))
    )
  } else if (is.data.frame(attributes)) {
    if (!all(c("name", "value") %in% names(attributes))) {
      stop("attributes data frame must have columns 'name' and 'value'")
    }
    attr_df <- tibble::tibble(
      name  = attributes$name,
      value = as.character(attributes$value)
    )
  } else {
    stop("attributes must be a data frame with columns 'name' and 'value', or a named list")
  }

  if (any(!nzchar(as.character(attr_df$name)))) {
    stop("Attribute names cannot be empty")
  }

  # Get NetCDF file to work with
  nc_work_path <- nc_path

  if (isTRUE(new_file)) {
    if (is.null(output_nc_path)) {
      output_nc_path <- sub("\\.nc$", "_updated.nc", nc_path)
    }

    file.copy(nc_path, output_nc_path, overwrite = TRUE)
    nc_work_path <- output_nc_path
  }

  # Open target NetCDF in write mode; close safely on function exit
  nc <- ncdf4::nc_open(nc_work_path, write = TRUE)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  # Get existing global attributes to determine add vs. update
  existing_attrs <- names(ncdf4::ncatt_get(nc, 0))

  status_list <- character(nrow(attr_df))

  for (i in seq_len(nrow(attr_df))) {
    attr_name  <- attr_df$name[[i]]
    attr_value <- attr_df$value[[i]]

    status_list[[i]] <- if (attr_name %in% existing_attrs) "updated" else "added"

    ncdf4::ncatt_put(nc, 0, attr_name, attr_value)
  }

  tibble::tibble(
    attribute_name  = attr_df$name,
    attribute_value = attr_df$value,
    status          = status_list,
    output_nc_path  = nc_work_path
  )
}
