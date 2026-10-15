#' Determine number of pre-defined element occurrences (EOs) (unique EO_ID) and number of good EOs based on user-provided shapefile
#'
#' @param eo_shp Shapefile or path to shapefile containing EOs (usually downloaded from Biotics)
#' @param EOID_col Required name containing unique identifier (e.g. EO_ID, ObjectID), for each pre-defined occurrence. Default = "EORANK"
#' @param EO_rank_col Optional name containing overall EO_rank (e.g. A, AB, D, etc). Default = "EORANK"
#' @param sname Species or community name column. Default = "scientificName"
#' @param v2 Boolean if the new CNHP-proposed method for handling number of good EOs should be used.
#' @return A data frame with number of EOs and number of good EOs organized by species or community name
#' @export
#'
tabulate_eos <- function(eo_shp, EOID_col = "EOID", EO_rank_col = "EORANK", sname = "scientificName", v2 = FALSE){

  # Read shapefile attribute table
  # Check that input is an sf object; if not, try reading as a file path
  if (!inherits(eo_shp, "sf")) {
    if (is.character(eo_shp) && length(eo_shp) == 1 && file.exists(eo_shp)) {
      eo_shp <- sf::st_read(eo_shp, quiet = TRUE)
      df <- sf::st_drop_geometry(eo_shp)
    } else {
      if (!is.data.frame(eo_shp)){
        stop("eo_shp must be an sf object or a valid file path")
      } else{df <- eo_shp}
    }
  } else{
    df <- sf::st_drop_geometry(eo_shp)
  }

  # Check required columns
  required_cols <- c(EOID_col, sname)

  if (!all(required_cols %in% names(df))) {
    stop(
      "Missing required column(s): ",
      paste(setdiff(required_cols, names(df)), collapse = ", ")
    )
  }

  # Count unique EOs for each species
  num_eo <- aggregate(
    df[[EOID_col]],
    list(sname = df[[sname]]),
    function(x) {
      x <- x[!is.na(x) & trimws(as.character(x)) != ""]

      if (length(x) == 0) {
        return(NA_integer_)
      }

      length(unique(x))
    }
  )

  names(num_eo)[2] <- "num_eo_gis"

  # if no EO Ranks, just return EO count
  if (!(EO_rank_col %in% names(df))) {
    return(num_eo)
  }

  # Determine whether each species has any actual EO ranks
  has_eo_rank <- aggregate(
    df[[EO_rank_col]],
    list(sname = df[[sname]]),
    function(x) {
      x <- x[!is.na(x) & trimws(as.character(x)) != ""]
      length(x) > 0
    }
  )

  names(has_eo_rank)[2] <- "has_eo_rank"

  # Count unique good EOs
  good_ranks <- c("A", "B", "AB", "AC", "A?", "B?")

  good_df <- df[
    !is.na(df[[EO_rank_col]]) &
      trimws(as.character(df[[EO_rank_col]])) != "" &
      df[[EO_rank_col]] %in% good_ranks,
    ,
    drop = FALSE
  ]

  num_good_eo <- aggregate(
    good_df[[EOID_col]],
    list(sname = good_df[[sname]]),
    function(x) {
      x <- x[!is.na(x) & trimws(as.character(x)) != ""]

      if (length(x) == 0) {
        return(NA_integer_)
      }

      length(unique(x))
    }
  )

  names(num_good_eo)[2] <- "num_good_eo_gis"

  # Combine
  df_v1 <- merge(
    num_eo,
    has_eo_rank,
    by = "sname",
    all.x = TRUE
  )

  df_v1 <- merge(
    df_v1,
    num_good_eo,
    by = "sname",
    all.x = TRUE
  )

  # Species with actual EO ranks but no good EOs get zero
  df_v1$num_good_eo_gis[
    df_v1$has_eo_rank & is.na(df_v1$num_good_eo_gis)
  ] <- 0

  # helper function
  assign_points <- function(value, rules, metric) {

    val_col <- paste0(metric, "Val")
    score_col <- paste0(metric, "Score")

    out <- rep(NA, length(value))

    ok <- !is.na(value)

    if (any(ok)) {

      idx <- vapply(
        value[ok],
        function(x) which(x <= rules[[metric]][[val_col]])[1],
        integer(1)
      )

      out[ok] <- rules[[metric]][[score_col]][idx]
    }

    out
  }

  rules_df <- list(
    Num = data.frame(
      NumVal = c(5, 20, 80, 300, Inf),
      NumScore = c("A", "B", "C", "D", "E")
    ),

    NumGood = data.frame(
      NumGoodVal = c(0, 3, 12, 40, 125, Inf),
      NumGoodScore = c("A", "B", "C", "D", "E", "F"),

      stringsAsFactors = FALSE
    ))

  # Convert counts to factor codes
  df_v1$num_eo_gis_score <- assign_points(
    df_v1$num_eo_gis,
    rules_df,
    "Num"
  )

  df_v1$num_good_eo_gis_score <- assign_points(
    df_v1$num_good_eo_gis,
    rules_df,
    "NumGood"
  )

  #####################################################
  # Alternative good EO assessment using biologist ranks
  #####################################################
  if (v2){
    # Good EO ranks used for the low estimate
    good_ranks_v2 <- c("A", "B", "AB", "AC", "A?", "B?")

    # E rank is included only in the high estimate
    high_ranks_v2 <- c(good_ranks_v2, "E")

    # Initialize output columns
    df_v1$num_good_eo_v2_low <- NA_real_
    df_v1$num_good_eo_v2_high <- NA_real_
    df_v1$num_good_eo_gis_score_v2 <- NA_character_

    # Work species by species
    species_list <- unique(df[[sname]])

    for (sp in species_list) {

      sp_df <- df[df[[sname]] == sp, ]
      # Remove EOs without a rank
      ranked_df <- sp_df[
        (!is.na(sp_df[[EO_rank_col]])) & (trimws(sp_df[[EO_rank_col]]) != ""),
        ,
        drop = FALSE
      ]

      # Skip if there are no ranked EOs
      if (nrow(ranked_df) == 0) {
        next
      }

      ranks <- ranked_df[[EO_rank_col]]

      ###################################################
      # Case 1: Species has at least one biologist-
      # assessed EO (A/B/etc. or E)
      ###################################################

      no_biologist_ranks <- c("H", "H?", "E")

      has_biologist_eo <- any(!(ranks %in% no_biologist_ranks))
      if (has_biologist_eo) {

        # Low estimate: only good EOs
        low_df <- ranked_df[
          ranked_df[[EO_rank_col]] %in% good_ranks_v2,
          ,
          drop = FALSE
        ]

        # High estimate: good EOs + E-ranked EOs
        high_df <- ranked_df[
          ranked_df[[EO_rank_col]] %in% high_ranks_v2,
          ,
          drop = FALSE
        ]

        # Count unique EOs
        low_count <- length(unique(
          low_df[[EOID_col]][!is.na(low_df[[EOID_col]])]
        ))

        high_count <- length(unique(
          high_df[[EOID_col]][!is.na(high_df[[EOID_col]])]
        ))

        # Save counts for this species
        idx <- df_v1$sname == sp

        df_v1$num_good_eo_v2_low[idx] <- low_count
        df_v1$num_good_eo_v2_high[idx] <- high_count

        # Convert counts to scores
        low_score <- assign_points(
          low_count,
          rules_df,
          "NumGood"
        )

        high_score <- assign_points(
          high_count,
          rules_df,
          "NumGood"
        )

        # If scores are the same, don't produce AA, BB, etc.
        if (identical(low_score, high_score)) {
          df_v1$num_good_eo_gis_score_v2[idx] <- low_score
        } else {
          df_v1$num_good_eo_gis_score_v2[idx] <- paste0(
            low_score,
            high_score
          )
        }

        ###################################################
        # Case 2: All EOs are historical (H or H?)
        ###################################################

      } else if (all(ranks %in% c("H", "H?"))) {

        idx <- df_v1$sname == sp

        df_v1$num_good_eo_v2_low[idx] <- 0
        df_v1$num_good_eo_v2_high[idx] <- 0
        df_v1$num_good_eo_gis_score_v2[idx] <- "A"

        ###################################################
        # Case 3: Neither condition applies
        ###################################################

      } else {

        idx <- df_v1$sname == sp

        df_v1$num_good_eo_v2_low[idx] <- NA_real_
        df_v1$num_good_eo_v2_high[idx] <- NA_real_
        df_v1$num_good_eo_gis_score_v2[idx] <- NA_character_
      }
    }
  }

  # Restore requested species-column name
  names(df_v1)[names(df_v1) == "sname"] <- sname
  # remove helper
  df_v1$has_eo_rank <- NULL

  return(df_v1)
}
