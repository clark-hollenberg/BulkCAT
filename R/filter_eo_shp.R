#' Pre-filters element occurrences (EOs) based on specified attribute values
#'
#' Removes EOs where specified attribute columns contain specified values.
#' Missing (NA) values are retained. By default, removes EOs with an
#' EO_Rank of X or X? and EOs flagged with a questionable ID.
#'
#' @param eo_shp Shapefile or path to shapefile containing EOs (usually
#'   downloaded from Biotics)
#' @param remove Named list specifying columns and corresponding values to remove.
#'   For example, \code{list(EO_Rank = c("X", "X?"), id_question = "Y")}.
#'   Names must correspond to columns in \code{eo_shp}. Default is
#'   \code{list(EO_Rank = c("X", "X?"), id_question = "Y")}.
#' @param sname Species or community name column. Default = "scientificName"
#' @return An sf object with specified EOs removed
#' @export
#'
filter_eo_shp <- function(
    eo_shp,
    remove = list(
      EO_Rank = c("X", "X?"),
      id_question = "Y"
    ),
    sname = "scientificName"
) {

  # Read shapefile attribute table
  # Check that input is an sf object; if not, try reading as a file path
  if (!inherits(eo_shp, "sf")) {
    if (is.character(eo_shp) &&
        length(eo_shp) == 1 &&
        file.exists(eo_shp)) {
      eo_shp <- sf::st_read(eo_shp, quiet = TRUE)
    } else {
      stop("eo_shp must be an sf object or a valid file path")
    }
  }

  # Check required species/community name column
  if (!(sname %in% names(eo_shp))) {
    stop("Column ", sname, " is missing from input")
  }

  # Check that remove is a named list
  if (!is.list(remove) ||
      is.null(names(remove)) ||
      any(names(remove) == "")) {
    stop("'remove' must be a named list of columns and values to remove")
  }

  # Apply each filter
  for (col in names(remove)) {

    # Skip columns that are not present in the input
    if (!(col %in% names(eo_shp))) {
      next
    }

    # Skip NULL filters
    if (is.null(remove[[col]])) {
      next
    }

    # Remove rows matching specified values while retaining NA values
    eo_shp <- eo_shp[
      is.na(eo_shp[[col]]) |
        !(eo_shp[[col]] %in% remove[[col]]),
      ,
      drop = FALSE
    ]
  }

  return(eo_shp)
}
