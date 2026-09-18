#' Converts a shapefile with multipart element occurrence (EO) polygons into centroid coordinates for each polygon.
#'
#' @param eo_shp Shapefile or path to shapefile containing EOs (usually downloaded from Biotics)
#' @param sname Species or community name column. Default = "scientificName"
#' @return A data frame with sname and coordinates for each centroid
#' @export
#'

eo_to_centroids <- function(eo_shp, sname = "scientificName") {

  # Load shapefile if needed
  if (!inherits(eo_shp, "sf")) {
    if (is.character(eo_shp) && length(eo_shp) == 1 && file.exists(eo_shp)) {
      eo_shp <- sf::st_read(eo_shp, quiet = TRUE)
    } else {
      stop("eo_shp must be an sf object or a valid file path")
    }
  }

  # Check species column
  if (!sname %in% names(eo_shp)) {
    stop("Column '", sname, "' not found in eo_shp")
  }

  # Explode multipart geometries into individual polygons
  eo_shp <- sf::st_cast(eo_shp, "POLYGON")

  # Calculate centroids guaranteed to fall within each polygon
  centroids <- sf::st_point_on_surface(eo_shp)

  # Transform to WGS84
  centroids <- sf::st_transform(centroids, 4326)

  # Extract coordinates
  coords <- sf::st_coordinates(centroids)

  # Save as data frame
  df <- data.frame(
    sname = eo_shp[[sname]],
    decimalLatitude = coords[, "Y"],
    decimalLongitude = coords[, "X"]
  )

  return(df)
}
