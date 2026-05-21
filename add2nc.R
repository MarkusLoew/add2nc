#' Add timestamp-aligned vectors from a data frame into an existing NetCDF file
#'
#' Purpose:
#' - Import one or more numeric vectors from `source_df` into `nc_path`.
#' - Align source rows to the NetCDF time axis using exact timestamp matching.
#' - Optionally shift source timestamps by a fixed minute offset before matching.
#' - Create target NetCDF variables if they do not already exist.
#'
#' Process overview:
#' 1. Validate required inputs (`source_time_col`, `vars_to_add`, naming lengths).
#' 2. Open NetCDF in write mode and resolve the time axis (`nc_time_var` or auto-detect).
#' 3. Convert NetCDF time to POSIXct using CF-compliant units/calendar.
#' 4. Parse source timestamps and apply optional fixed offset.
#' 5. Filter source rows to NetCDF time window, deduplicate timestamps (keep last),
#'    then exact-match to NetCDF indices.
#' 6. Add missing variables to the NetCDF file and write full-length aligned vectors.
#' 7. Return a tibble with diagnostics on matching and settings used.
#'
#' @param nc_path Character scalar. Path to the existing NetCDF file to modify.
#' @param source_df Data frame containing a timestamp column and variables to write.
#' @param source_time_col Character scalar. Column name in `source_df` containing timestamps. Default `"TIMESTAMP"`.
#' @param vars_to_add Character vector. Source column names to import from `source_df`.
#' @param new_var_names Character vector or `NULL`. Output variable names in NetCDF.
#'   If `NULL`, defaults to `paste0(vars_to_add, "_profile")`.
#' @param units Character scalar or character vector. Units string(s) assigned to newly created
#'   NetCDF variables. If length 1, the same units are used for all variables.
#'   Ignored for variables that already exist in the file.
#' @param longname Character scalar, character vector, or `NULL`. Long name(s) assigned to
#'   newly created NetCDF variables. If length 1, the same long name is used for all variables.
#'   If `NULL`, defaults to `paste(vars_to_add, "imported from source dataframe")`.
#'   Ignored for variables that already exist in the file.
#' @param missval Numeric scalar. Missing-value code used when building full-length outputs.
#' @param nc_time_var Character scalar. Preferred NetCDF time axis name (e.g. `"time"`).
#'   If not found, a time-like dimension (units containing `"since"`) is auto-detected.
#' @param tz Character scalar. Time zone used for NetCDF timestamp conversion and matching.
#' @param source_tz Character scalar. Time zone used when parsing source timestamps.
#' @param time_offset_minutes Numeric scalar. Fixed minute offset applied to source timestamps
#'   before matching (default `6`).
#' @param time_offset_direction Character scalar. One of `"subtract"`, `"add"`, `"none"`.
#'   Controls how `time_offset_minutes` is applied to source timestamps.
#' @param new_file Logical scalar. If `TRUE`, a new NetCDF file is created for output.
#'   If `FALSE`, the existing file at `nc_path` is modified in place.
#' @param output_nc_path Character scalar or `NULL`. Path for the output NetCDF file.
#'   Required if `new_file` is `TRUE`. If `NULL`, defaults to `<nc_path>_updated.nc`.
#'
#' @return A tibble with one row per imported variable:
#' - `source_var`: source column name in `source_df`
#' - `target_var`: target variable name in NetCDF
#' - `matched_rows`: number of source rows matched to NetCDF timestamps
#' - `nc_time_points`: length of NetCDF time axis
#' - `resolved_time_name`: resolved NetCDF time axis name used
#' - `time_offset_minutes`: offset value applied
#' - `time_offset_direction`: offset direction applied
#' - `output_nc_path`: path to the output NetCDF file
#'
#' @examples
#' result <- add2nc(
#'   nc_path = nc_path,
#'   source_df = profile,
#'   source_time_col = "TIMESTAMP",
#'   vars_to_add = c("FC_Storage"),
#'   time_offset_minutes = 6,
#'   time_offset_direction = "subtract"
#' )
add2nc <- function(
  nc_path,
  source_df,
  source_time_col = "TIMESTAMP",
  vars_to_add,
  new_var_names = NULL,
  units = "unknown",
  longname = NULL,
  missval = -9999,
  nc_time_var = "time",
  tz = "UTC",
  source_tz = tz,
  time_offset_minutes = 6,
  time_offset_direction = c("subtract", "add", "none"),
  new_file = TRUE,
  output_nc_path = NULL
) {

  # list required packages
  required_packages <- c("ncdf4", "CFtime", "dplyr", "tibble")
  lapply(required_packages, require, character.only = TRUE)

  # preliminary checks and setup
  time_offset_direction <- match.arg(time_offset_direction)

  if (is.null(new_var_names)) {
    new_var_names <- paste0(vars_to_add, "_profile")
  }

  if (length(new_var_names) != length(vars_to_add)) {
    stop("new_var_names must have the same length as vars_to_add")
  }

  if (length(units) == 1) {
    units <- rep(units, length(vars_to_add))
  }

  if (length(units) != length(vars_to_add)) {
    stop("units must have length 1 or the same length as vars_to_add")
  }

  if (is.null(longname)) {
    longname <- paste(vars_to_add, "imported from source dataframe")
  }

  if (length(longname) == 1) {
    longname <- rep(longname, length(vars_to_add))
  }

  if (length(longname) != length(vars_to_add)) {
    stop("longname must be NULL, length 1, or the same length as vars_to_add")
  }
  
  # get netcdf file
  nc_work_path <- nc_path
  
  # check for new file argument and create copy (by default)
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

  # Resolve the time axis name; it may be a variable and/or a dimension
  resolved_time_name <- nc_time_var
  time_var_exists <- resolved_time_name %in% names(nc$var)
  time_dim <- nc$dim[[resolved_time_name]]

  # If the supplied time name is not found, auto-detect a time-like dimension via "units since ..."
  if (is.null(time_dim) && !time_var_exists) {
    dim_names <- names(nc$dim)
    time_like_dims <- dim_names[
      vapply(
        nc$dim,
        \(d) is.character(d$units) && grepl("since", d$units, ignore.case = TRUE),
        logical(1)
      )
    ]

    if (length(time_like_dims) == 1) {
      resolved_time_name <- time_like_dims[[1]]
      time_dim <- nc$dim[[resolved_time_name]]
      time_var_exists <- resolved_time_name %in% names(nc$var)
    } else {
      stop(
        paste0(
          "Could not resolve nc_time_var. Tried '", nc_time_var, "'. ",
          "Available vars: ", paste(names(nc$var), collapse = ", "),
          " | Available dims: ", paste(names(nc$dim), collapse = ", ")
        )
      )
    }
  }

  # Read raw NetCDF time values and units from variable or dimension
  if (time_var_exists) {
    nc_time_raw <- ncvar_get(nc, resolved_time_name)
    nc_time_units <- ncatt_get(nc, resolved_time_name, "units")$value
  } else {
    if (is.null(time_dim)) {
      stop("Resolved time dimension is NULL")
    }
    nc_time_raw <- time_dim$vals
    nc_time_units <- time_dim$units
  }

  # Time units are required for CFtime conversion
  if (is.null(nc_time_units) || is.na(nc_time_units) || !nzchar(nc_time_units)) {
    stop("Time units not found for resolved time axis")
  }

  # Read calendar; fallback to proleptic_gregorian if absent
  nc_time_calendar <- tryCatch(
    ncatt_get(nc, resolved_time_name, "calendar")$value,
    error = \(e) NA_character_
  )
  if (is.null(nc_time_calendar) || is.na(nc_time_calendar) || !nzchar(nc_time_calendar)) {
    nc_time_calendar <- "proleptic_gregorian"
  }

  # Convert NetCDF time to POSIXct for matching
  nc_cf <- CFtime(nc_time_units, calendar = nc_time_calendar, nc_time_raw)
  nc_timestamps <- as.POSIXct(as_timestamp(nc_cf, asPOSIX = TRUE), tz = tz)

  # Parse source timestamps with several common datetime formats
  parse_source_time <- function(x, source_tz) {
    if (inherits(x, "POSIXt")) {
      return(as.POSIXct(x, tz = source_tz))
    }

    x_chr <- as.character(x)
    as.POSIXct(
      x_chr,
      tz = source_tz,
      tryFormats = c(
        "%Y-%m-%d %H:%M:%S",
        "%Y/%m/%d %H:%M:%S",
        "%d/%m/%Y %H:%M",
        "%d/%m/%Y %H:%M:%S",
        "%Y%m%d %H%M",
        "%Y%m%d%H%M",
        "%Y%m%d%H%M%S"
      )
    )
  }

  source_timestamps <- parse_source_time(source_df[[source_time_col]], source_tz = source_tz)

  # Apply fixed clock offset between source system and NetCDF system
  offset_seconds <- switch(
    time_offset_direction,
    add = as.numeric(time_offset_minutes) * 60,
    subtract = -as.numeric(time_offset_minutes) * 60,
    none = 0
  )
  source_timestamps <- source_timestamps + offset_seconds

  # Drop rows where source timestamps could not be parsed
  valid_rows <- !is.na(source_timestamps)
  source_df <- source_df[valid_rows, , drop = FALSE]
  source_timestamps <- source_timestamps[valid_rows]

  if (!nrow(source_df)) {
    stop("No parseable source timestamps; check source_time_col format and source_tz")
  }

  # Keep only rows whose timestamps fall inside the NetCDF time window
  nc_num <- as.numeric(nc_timestamps)
  src_num <- as.numeric(source_timestamps)
  time_window <- range(nc_num, na.rm = TRUE)
  in_window <- src_num >= time_window[1] & src_num <= time_window[2]

  source_df <- source_df[in_window, , drop = FALSE]
  source_timestamps <- source_timestamps[in_window]

  if (!nrow(source_df)) {
    nc_min <- as.POSIXct(time_window[1], origin = "1970-01-01", tz = tz)
    nc_max <- as.POSIXct(time_window[2], origin = "1970-01-01", tz = tz)
    stop(
      paste0(
        "No source rows fall inside netCDF time window. ",
        "netCDF window [", format(nc_min, "%Y-%m-%d %H:%M:%S %Z"), " .. ",
        format(nc_max, "%Y-%m-%d %H:%M:%S %Z"), "]. ",
        "Try setting source_tz and/or time_offset_minutes."
      )
    )
  }

  # If duplicate source timestamps exist, keep the last observation
  source_key <- as.numeric(source_timestamps)
  keep_last <- !duplicated(source_key, fromLast = TRUE)
  source_df <- source_df[keep_last, , drop = FALSE]
  source_timestamps <- source_timestamps[keep_last]

  # Exact timestamp match from source rows to NetCDF time index
  nc_index <- match(as.numeric(source_timestamps), nc_num)
  matched <- !is.na(nc_index)

  if (!any(matched)) {
    stop("No exact timestamp matches between source_df and netCDF time axis")
  }

  # Keep matched rows only
  source_df <- source_df[matched, , drop = FALSE]
  nc_index <- nc_index[matched]

  # Get the resolved time dimension object for creating new variables
  time_dim <- nc$dim[[resolved_time_name]]
  if (is.null(time_dim)) {
    stop("No resolved time dimension found in netCDF")
  }

  # Create missing target variables in the NetCDF file
  vars_in_file <- names(nc$var)
  vars_missing_in_file <- setdiff(new_var_names, vars_in_file)

  if (length(vars_missing_in_file) > 0) {
    for (i in seq_along(new_var_names)) {
      target_var <- new_var_names[[i]]
      source_var <- vars_to_add[[i]]

      if (!(target_var %in% vars_missing_in_file)) {
        next
      }

      new_var <- ncvar_def(
        name = target_var,
        units = units[[i]],
        dim = list(time_dim),
        missval = missval,
        longname = longname[[i]]
      )

      nc <- ncvar_add(nc, new_var)
    }
  }

  # Build full-length output vectors (NetCDF time length), fill with missval,
  # then insert matched source values at their aligned NetCDF indices
  n_time <- length(nc_timestamps)

  for (i in seq_along(vars_to_add)) {
    source_var <- vars_to_add[[i]]
    target_var <- new_var_names[[i]]

    out <- rep(missval, n_time)
    values <- as.numeric(source_df[[source_var]])
    valid_values <- !is.na(values)
    out[nc_index[valid_values]] <- values[valid_values]

    ncvar_put(nc, target_var, out)
  }

  # Return a compact diagnostics table
  tibble(
    source_var = vars_to_add,
    target_var = new_var_names,
    matched_rows = sum(matched),
    nc_time_points = length(nc_timestamps),
    resolved_time_name = resolved_time_name,
    time_offset_minutes = as.numeric(time_offset_minutes),
    time_offset_direction = time_offset_direction,
    output_nc_path = nc_work_path
  )
}