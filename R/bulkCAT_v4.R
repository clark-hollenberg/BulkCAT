#' Run BulkCAT analysis
#'
#' This function completes conservation status assessments (rarity ranks) using NatureServe
#' methodology. BulkCAT requires a multi-species dataset with name column sname formatted as EITHER a point observations dataframe with lat/lon columns OR a polygon layer.
#'
#' @param input_df Input multispecies observation points dataframe. Default = NULL
#' @param poly_layer Shapefile path or sf object for polygon layer to be used for calculating AOO. Recommended for plant communities. Default = NULL.
#' @param factors_df Optional supplemental dataframe containing rank factors that are pre-assigned by experts (e.g. download from Biotics). Supplemental factors include threats, trends, and population sizes. See README for additional details on factors_df.
#' @param community Boolean if the input contains plant association names. Note that the category boundaries for AOO differ between species and communities. Default = FALSE. Large patch AOO calculations will be default unless patch size specified in factors_df. Without a poly_layer, communities will be ranked based on EOO and number of EOs. Buffer points into polygons if needed.
#' @param sname Species name column. Default = "scientificName"
#' @param lat Latitude column name. Default = "decimalLatitude"
#' @param lon Longitude column name. Default = "decimalLongitude"
#' @param eo_separation Minimum separation distance (m) for unique EO clusters. Default = 1000m.
#' @param grid_size Side length (m) for AOO grid cells. Default = 2000m. Must use either 1000 or 2000m edge lengths for appropriate AOO scoring.
#' @param trinomial_synon Boolean if single trinomial names should be matched to parent binomial
#' @return Dataframe with calculated rarity metrics for each species/element. If factors_df is supplied, these fields will be joined to output.
#' @export
#'

run_bulkCAT <- function(input_df = NULL,
                        poly_layer = NULL,
                        factors_df = NULL,
                        community = FALSE,
                        sname = "scientificName",
                        lat = "decimalLatitude",
                        lon = "decimalLongitude",
                        eo_separation = 1000,
                        grid_size = 2000,
                        trinomial_synon = FALSE) {
  # ----------------------------------------------------------------
  # ----- Input validation -----
  # ----------------------------------------------------------------
  validate_grid_size(grid_size)
  required_cols <- c(sname, lat, lon)
  no_aoo <- validate_inputs(input_df, poly_layer, required_cols, community)


  print("Loading points...")
  if (!is.null(input_df)){
    df <- input_df[(!is.na(input_df[[lat]]) &
                      !is.na(input_df[[lon]]) &
                      !is.na(input_df[[sname]])),]

  } else{
    # reproject to WGS84 for lat/lon
    poly_sf <- sf::st_transform(poly_sf, 4326)

    # centroids in geographic coordinates
    centroids <- sf::st_centroid(poly_sf)
    centroid_coords <- sf::st_coordinates(centroids)
    centroids[[lon]] <- centroid_coords[, 1]
    centroids[[lat]] <- centroid_coords[, 2]
    df <- as.data.frame(sf::st_drop_geometry(centroids))
  }

  # remove genus-only records (keep names with ≥ 2 parts)
  df <- df[sapply(strsplit(df[[sname]], "\\s+"), length) >= 2, ]

  if (all(is.na(df[[sname]]))) {
    stop("Species column has only missing values.")
  }

  factors_df <- validate_factors_df(factors_df, df, sname)

  # ----------------------------------------------------------------
  # ----- Prepare coordinate reference systems -----
  # ----------------------------------------------------------------
  equal_area_crs <- 6933  # World Cylindrical Equal Area projection
  wgs_crs <- 4326         # WGS84

  if (!is.null(poly_layer)) {
    # ensure it's in World Equal Area
    poly_sf <- sf::st_transform(poly_sf, crs = 6933)
  }

  gdf <- sf::st_as_sf(df, coords = c(lon, lat), crs = wgs_crs)
  gdf_proj <- sf::st_transform(gdf, crs = equal_area_crs)

  # ----------------------------------------------------------------
  # ----- Identify species list -----
  # ----------------------------------------------------------------
  species_list <- sort(unique(gdf_proj[[sname]]))

  # create list object to store results
  results <- list()

  # ----------------------------------------------------------------
  # ----- Species loop -----
  # ----------------------------------------------------------------
  for (species in species_list) {
    cat("Processing:", species, "\n")

    ########## trinomial handling
    # Match all descendant varieties/subspecies if just genus + species
    if (community == FALSE){
      if (length(strsplit(species, "\\s+")[[1]]) == 2) {
        species_subset <- gdf_proj[startsWith(gdf_proj[[sname]], species), ]
      } else {
          if ((trinomial_synon == TRUE) && (community == FALSE)){
            # trinomial: check if this is the only infraspecific taxon for the binomial
            species_parts <- strsplit(species, "\\s+")[[1]]
            binomial <- paste(species_parts[1:2], collapse = " ")

            # find all taxa in list starting with the binomial
            related_taxa <- grep(paste0("^", binomial, "\\b"), species_list, value = TRUE)

            binomial_present <- binomial %in% related_taxa
            num_infraspp <- sum(lengths(strsplit(related_taxa, "\\s+")) > 2)

            if (binomial_present && num_infraspp == 1) {
              # only trinomial and binomial exists -> copy binomial results
              print("Only one infraspecies found, matching binomial results...")
              binomial_result <- results[nrow(results), ]
              results <- rbind(results, data.frame(
                species = species,
                num_obs = binomial_result$num_obs,
                eoo_area_km2 = binomial_result$eoo_area_km2,
                aoo_num_cells = binomial_result$aoo_num_cells,
                num_eo = binomial_result$num_eo,
                stringsAsFactors = FALSE
              ))
              next  # skip recalculation for this trinomial
              }
            }
          # multiple infraspecific taxa -> only use exact match
          species_subset <- gdf_proj[gdf_proj[[sname]] == species, ]
          }
    } else
    {
      # if processing a plant community dataset (trinomial not relevant)
      species_subset <- gdf_proj[gdf_proj[[sname]] == species, ]
    }

    num_obs <- nrow(species_subset)     # count number of observations
    if (num_obs == 0) {
      warning("No observations for: ", species, "; skipping.")
      next
    }

    ###### AOO: 2x2 km Grid ######
    ##########################################################
    coords <- sf::st_coordinates(species_subset)

    # Options for polygon AOO calculation
    if (!is.null(poly_layer)) {
      # calculate AOO with 2x2km cells for plants
      if (!community){
        poly_subset <- poly_sf[poly_sf[[sname]]==species, ]

        # Fixed global origin (IUCN-style)
        origin <- c(0, 0)

        # Get bounding box
        bb <- sf::st_bbox(poly_subset)

        # Snap bbox to grid
        xmin <- floor((as.numeric(bb["xmin"]) - origin[1]) / grid_size) * grid_size + origin[1]
        ymin <- floor((as.numeric(bb["ymin"]) - origin[2]) / grid_size) * grid_size + origin[2]
        xmax <- ceiling((as.numeric(bb["xmax"]) - origin[1]) / grid_size) * grid_size + origin[1]
        ymax <- ceiling((as.numeric(bb["ymax"]) - origin[2]) / grid_size) * grid_size + origin[2]

        # Create snapped bbox polygon
        snapped_bbox <- sf::st_as_sfc(sf::st_bbox(
          c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
          crs = sf::st_crs(poly_subset)
        ))
        # Build grid
        grid <- sf::st_make_grid(
          snapped_bbox,
          cellsize = grid_size,
          square = TRUE
        )
        grid_sf <- sf::st_sf(geometry = grid)

        # Intersect grid with species geometry
        hits <- sf::st_intersects(grid_sf, poly_subset, sparse = FALSE)
        # Count occupied cells
        aoo_cells <- sum(apply(hits, 1, any))
      }
      # calculate AOO with based on total area in sq-km for communities
      else {
        poly_subset <- poly_sf[poly_sf[[sname]]==species, ]
        aoo_km2 <- sum(as.numeric(sf::st_area(poly_subset)), na.rm = TRUE) / 1e6
      }
    }
    # if no polygon layer provided
    else {
      # if AOO should be calculated based on the input_df (unlikely for communities unless using plot data, common for plants)
      bottomleftpoints <- floor(coords / grid_size)
      uniquecells <- unique(bottomleftpoints)
      aoo_cells <- nrow(uniquecells)
        if (community)
        {
          # convert cells to km2
          aoo_km2 <- aoo_cells * (grid_size^2) / 1e6
        }
    }

    ###### EOO: Convex Hull Area ######

    # if < 3 points were provided for taxon, set eoo = aoo (note, must convert from cells to km2)
    if (num_obs < 3){
      if (community) {
        eoo_area_km2 <- aoo_km2
      } else {
        eoo_area_km2 <- aoo_cells * (grid_size^2) / 1e6
      }
    } else{
      eoo_area_km2 <- calculate_eoo(species_subset, df, lat, lon, species, sname)
    }

    ###### EO Cluster Count ######
    # note that this could be modified to allow for number of EO calculation based on polygons (most relevant for plants)

      clustering <- dbscan::dbscan(
        coords,
        eps = eo_separation,
        minPts = 1
      )
      num_clusters <- max(clustering$cluster)

    ###### Append to results ######
    if (!community){
      results <- rbind(results, data.frame(
        species = species,
        num_obs = num_obs,
        eoo_area_km2 = round(eoo_area_km2, 2),
        aoo_num_cells = aoo_cells,
        num_eo_calc = num_clusters,
        stringsAsFactors = FALSE
      ))
    } else{
    results <- rbind(results, data.frame(
      species = species,
      num_obs = num_obs,
      eoo_area_km2 = round(eoo_area_km2, 2),
      aoo_km2 = aoo_km2,
      num_eo_calc = num_clusters,
      stringsAsFactors = FALSE
    ))
    }
  }

  ranked_results <- apply_factors(results, sname, factors_df, community, grid_size, no_aoo)

  return(ranked_results)
}
