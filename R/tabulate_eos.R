#' Determine number of pre-defined element occurrences (EOs) (unique EO_ID) and number of good EOs based on user-provided shapefile
#'
#' @param eo_shp Shapefile or path to shapefile containing EOs (usually downloaded from Biotics)
#' @param EO_rank_col Optional name containing overall EO_rank (e.g. A, AB, D, etc). Default = "EORANK"
#' @param sname Species or community name column. Default = "scientificName"
#' @return A data frame with number of EOs and number of good EOs organized by species or community name
#' @export
#'
#'
tabulate_eos <- function(eo_shp, EOID_col = "EOID", EO_rank_col = "EORANK", sname = "scientificName"){

  # Read shapefile attribute table
  # Check that input is an sf object; if not, try reading as a file path
  if (!inherits(eo_shp, "sf")) {
    if (is.character(eo_shp) && length(eo_shp) == 1 && file.exists(eo_shp)) {
      eo_shp <- sf::st_read(eo_shp, quiet = TRUE)
    } else {
      stop("eo_shp must be an sf object or a valid file path")
    }
  }

  df <- sf::st_drop_geometry(eo_shp)

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
    function(x) length(unique(x[!is.na(x)]))
  )

  names(num_eo)[2] <- "num_eo_gis"

  # if no EO Ranks, just return EO count
  if (!(EO_rank_col %in% names(df))){
    return(num_eo)
  }

  # Count unique good EOs for each species
  # Good EO ranks
  good_ranks <- c("A", "B", "AB", "AC", "A?", "B?")

  good_df <- df[
    !is.na(df[[EO_rank_col]]) &
      df[[EO_rank_col]] %in% good_ranks,
  ]

  num_good_eo <- aggregate(
    good_df[[EOID_col]],
    list(sname = good_df[[sname]]),
    function(x) length(unique(x[!is.na(x)]))
  )

  names(num_good_eo)[2] <- "num_good_eo_gis"

  # Combine
  df <- merge(
    num_eo,
    num_good_eo,
    by = "sname",
    all.x = TRUE
  )

  # Species with no good EOs get zero
  df$num_good_eo_gis[is.na(df$num_good_eo_gis)] <- 0

  # Convert counts to factor codes
  df$num_eo_gis <- assign_points(
    df$num_eo_gis,
    rules_df,
    "Num"
  )

  df$num_good_eo_gis <- assign_points(
    df$num_good_eo_gis,
    rules_df,
    "Num"
  )

  # Restore requested species-column name
  names(df)[names(df) == "sname"] <- sname

  return(df)
}



#' Pre-filters element occurrences (EOs) that are assessed as extirpated (X/X? EO_Rank) or have questionable IDs
#'
#' @param eo_shp Shapefile or path to shapefile containing EOs (usually downloaded from Biotics)
#' @param EO_rank_col Optional name containing overall EO_rank (e.g. A, AB, D, etc). Default = "EORANK"
#' @param remove_ranks Optional vector of EO_ranks to remove (e.g. X, X?, F, etc). Default = c("X", "X?")
#' @param id_question_col Optional name containing data on questionable ids. Default = "id_question"
#' @param sname Species or community name column. Default = "scientificName"
#' @return A data frame with number of EOs and number of good EOs organized by species or community name
#' @export
#'

eo_remove_X_and_questions <- function(eo_shp, EOID_col = "EOID", EO_rank_col = "EORANK", remove_ranks = c("X", "X?"),
                                            id_question_col = "id_question", id_remove_flags = c("Y", "?"), sname = "scientificName"){
  # Read shapefile attribute table
  # Check that input is an sf object; if not, try reading as a file path
  if (!inherits(eo_shp, "sf")) {
    if (is.character(eo_shp) && length(eo_shp) == 1 && file.exists(eo_shp)) {
      eo_shp <- sf::st_read(eo_shp, quiet = TRUE)
    } else {
      stop("eo_shp must be an sf object or a valid file path")
    }
  }

  # Check required columns
  optional_cols <- c(EO_rank_col, id_question_col)

  if (!(sname %in% names(eo_shp)) || (!any(optional_cols %in% names(eo_shp)))) {
    stop("Columns", sname, "or both of ", optional_cols, " missing from input")
  } else{
    if (EO_rank_col %in% names(eo_shp) && !is.null(remove_ranks)) {
      # Remove geometry with EO_rank_col in remove_ranks
      eo_shp <- eo_shp[
        is.na(eo_shp[[EO_rank_col]]) |
          !(eo_shp[[EO_rank_col]] %in% remove_ranks),
        ,
        drop = FALSE
      ]
    }

    if (id_question_col %in% names(eo_shp) && !is.null(id_remove_flags)) {
      # Remove geometry with id_question_col in id_remove_flags
      eo_shp <- eo_shp[
        is.na(eo_shp[[id_question_col]]) |
          !(eo_shp[[id_question_col]] %in% id_remove_flags),
        ,
        drop = FALSE
      ]
    }
  }

  return(eo_shp)

}
