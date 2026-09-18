# input data validation functions
#############################################################

# checks to make sure grid_size is either 2km or 1km sides
validate_grid_size <- function(grid_size){
  if (!(grid_size %in% c(2000, 1000))){
    stop("Only grid cell sizes of 4 sq-km (2000m sides) or 1 sq-km (1000m sides) are supported.")}
}

# checks input_df and poly_layer for issues, returns whether aoo must be skipped
validate_inputs <- function(input_df, poly_layer, required_cols, community){
  no_aoo = FALSE

  # if you have a dataframe input
  if (!is.null(input_df)){
    if (community == TRUE){
      warning("poly_layer required for communities to calculate AOO measurements. Consider buffering plot points based on a typical patch size. \nAOO will not be included in
              base rank calculations. AOO can still be included in factors_df if desired.")
      no_aoo = TRUE
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
        # create input_df from polygon centroids (used for EOO and num_EOs)

        # ensure CRS exists
        if (is.na(sf::st_crs(poly_sf))) {
          stop("poly_layer has no CRS defined.")
        }
    } else{
      stop("You must provide either a valid input points dataframe or polygon shapefile/sf object.")
      }
  }

  missing_cols <- setdiff(required_cols, names(input_df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  return (no_aoo)
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


