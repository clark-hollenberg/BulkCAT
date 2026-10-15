# input data validation functions
#############################################################

# checks to make sure grid_size is either 2km or 1km sides
validate_grid_size <- function(grid_size){
  if (!(grid_size %in% c(2000, 1000))){
    stop("Only grid cell sizes of 4 sq-km (2000m sides) or 1 sq-km (1000m sides) are supported.")}
}

# checks input_df and poly_layer for issues, returns whether aoo must be skipped
validate_inputs <- function(input_df, poly_layer, required_cols, community){
  poly_sf = NA

  # if you have a dataframe input
  if (!is.null(input_df)){
    if (community == TRUE){
      warning("poly_layer is required for communities to calculate direct AOO measurements. Consider buffering plot points based on a typical patch size. \n
              AOO will be calculated using grid cell approach.")
    }
    if (!is.null(poly_layer)){
      stop("You should provide EITHER an input_df with lat/lon coordinates OR a poly_layer, not both!")
    }
    if (!is.data.frame(input_df))
    {
      stop("input_df must be a data frame.")
    }

  } else {
    # if you have a poly_layer input
    if (!is.null(poly_layer)){
        if (inherits(poly_layer, "character")) {
          poly_sf <- sf::st_read(poly_layer, quiet = TRUE)
        } else if (inherits(poly_layer, "sf")) {
          poly_sf <- poly_layer
        } else {
          stop("poly_layer must be either a path to a shapefile or an sf object.")
        }

        # ensure CRS exists
        if (is.na(sf::st_crs(poly_sf))) {
          stop("poly_layer has no CRS defined.")
        }

      # Split multipart geometries into individual polygon parts
      poly_parts <- sf::st_cast(poly_sf, "POLYGON")

      # Calculate one point within each polygon part
      points <- sf::st_point_on_surface(poly_parts)

      # Transform points to longitude/latitude
      points <- sf::st_transform(points, 4326)

      # Extract X, Y coordinates
      coords <- sf::st_coordinates(points)

      # Assign coordinates to the corresponding polygon-part attributes
      input_df <- sf::st_drop_geometry(poly_parts)
      input_df[[required_cols[2]]] <- coords[, "Y"]  # Latitude
      input_df[[required_cols[3]]] <- coords[, "X"]  # Longitude

    } else{
      stop("You must provide either a valid input points dataframe or polygon shapefile/sf object.")
    }

    missing_cols <- setdiff(required_cols, names(input_df)) # only the sname column is required if poly layer
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }
    if (any(input_df[[required_cols[2]]] > 86 | input_df[[required_cols[2]]] < -86, na.rm = TRUE)) {
      stop("Latitude values must be between -86 and 86.")
    }
  }

  return(list(
    poly_sf = poly_sf,
    input_df = input_df
  ))
}


# validate factors_df

validate_factors_df <- function(factors_df, df, sname){
  if (!is.null(factors_df)){
    if (!is.data.frame(factors_df))
    {
      stop("factors_df must be a data frame. Refer to documentation for formatting.")
    }
    if (!(sname %in% names(factors_df))) {
      stop("factors_df does not contain scientific name column: ", sname)
    }else{
      if (!(any(factors_df[[sname]] %in% df[[sname]]))) {
        stop("factors_df does not contain any scientific names matching input taxa in column ", sname)
      } else{
        # Keep only one row per species
        factors_df <- factors_df[!is.na(factors_df[[sname]]), ]

        # Match factor names to the species names used in the analysis
        factors_df <- factors_df[
          factors_df[[sname]] %in% unique(df[[sname]]),
        ]
      }
    }
  } else{
    warning("Note: factors_df was not provided. Consider setting threats=TRUE or run calc_threats() after completion to see the effects of threats on ranks")
  }

  return(factors_df)
}


