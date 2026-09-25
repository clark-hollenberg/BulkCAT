########################################################
# function to calculate eoo
# made modifications to handle polar and antimeridian species
calculate_eoo <- function(species_subset, df, lat, lon, species, sname, check_global){

    if (check_global){
      species_wgs_df <- df[df[[sname]] == species, ]
      wgs_crs = 4326
      lon_values <- species_wgs_df[[lon]]
      lat_values <- species_wgs_df[[lat]]

      if (check_polar(lat_values)){

        warning("Using polar projection for species ", species)
        # take the original WGS centroids for that species
        gdf <- sf::st_as_sf(species_wgs_df, coords = c(lon, lat), crs = wgs_crs)

        #Use spherical center for potentially polar distributions
        center <- spherical_center(lon_values, lat_values)
        species_subset <- sf::st_transform(
          gdf,
          paste0(
            "+proj=laea",
            " +lat_0=", center["lat"],
            " +lon_0=", center["lon"],
            " +datum=WGS84",
            " +units=m",
            " +no_defs"
          )
        )
      }
      else{
        if (check_antimeridian(lon_values)){

          warning("Using antimeridian projection for species ", species)
          # Calculate mean longitude of positive and negative observations
          lon_pos <- lon_values[lon_values >= 0]
          lon_neg <- lon_values[lon_values < 0]

          mean_pos <- mean(lon_pos)
          mean_neg <- mean(lon_neg)

          # Distance between groups across the antimeridian
          wrapped_distance <- (180 - mean_pos) + (180 + mean_neg)

          # Central longitude halfway between the two groups,
          # traveling across the antimeridian
          lon_0 <- mean_pos + wrapped_distance / 2

          # Convert back to [-180, 180]
          if (lon_0 > 180) {
            lon_0 <- lon_0 - 360
          }

          # Centered cylindrical equal-area projection
          eoo_crs <- paste0(
            "+proj=cea +lon_0=", lon_0,
            " +lat_ts=0 +datum=WGS84 +units=m +no_defs"
          )
          gdf <- sf::st_as_sf(species_wgs_df, coords = c(lon, lat), crs = wgs_crs)
          species_subset <- sf::st_transform(
            gdf,
            eoo_crs
          )
        }
      }
    }

    # apply calculation to correctly transformed coordinates
    hull <- sf::st_convex_hull(sf::st_union(species_subset))
    eoo_area_km2 <- as.numeric(sf::st_area(hull)) / 1e6

  return(eoo_area_km2)
}

#######################################################
# Calculate spherical center of geographic coordinates
spherical_center <- function(lon, lat) {

  lon_rad <- lon * pi / 180
  lat_rad <- lat * pi / 180

  # Convert longitude/latitude to Cartesian coordinates
  x <- cos(lat_rad) * cos(lon_rad)
  y <- cos(lat_rad) * sin(lon_rad)
  z <- sin(lat_rad)

  # Mean position on the sphere
  x_mean <- mean(x, na.rm = TRUE)
  y_mean <- mean(y, na.rm = TRUE)
  z_mean <- mean(z, na.rm = TRUE)

  # Normalize
  magnitude <- sqrt(
    x_mean^2 +
      y_mean^2 +
      z_mean^2
  )

  x_mean <- x_mean / magnitude
  y_mean <- y_mean / magnitude
  z_mean <- z_mean / magnitude

  # Convert back to longitude/latitude
  center_lon <- atan2(y_mean, x_mean) * 180 / pi
  center_lat <- asin(z_mean) * 180 / pi

  c(
    lon = center_lon,
    lat = center_lat
  )
}

#######################################################
# check if observations are clustered around one of the poles.
check_polar <- function(lat) {
  mean_lat <- mean(lat, na.rm = TRUE)

  if (mean_lat > 70 || mean_lat < -70) {
    return(TRUE)
  }
  return(FALSE)
}

###################################################
# function to check if EOO across the antimeridian is necessary
check_antimeridian <- function(lon) {

  lon_pos <- lon[lon >= 0]
  lon_neg <- lon[lon < 0]

  # Need observations on both sides of Greenwich for antimeridian to be relevant
  if (length(lon_pos) == 0 || length(lon_neg) == 0) {
    return(FALSE)
  }

  # taking means is more robust to truly global distributions
  mean_pos <- mean(lon_pos)
  mean_neg <- mean(lon_neg)

  # Distance between the two groups without wrapping
  normal_distance <- mean_pos - mean_neg

  # Distance between the groups across the antimeridian
  wrapped_distance <- (180 - mean_pos) + (180 + mean_neg)

  return(wrapped_distance < normal_distance)
}
