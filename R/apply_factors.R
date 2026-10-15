# ----------------------------------------------------------------
# Apply NatureServe rank factors to BulkCAT results
# ----------------------------------------------------------------

apply_factors <- function(results,
                          sname,
                          factors_df,
                          community,
                          grid_size) {

  # ==============================================================
  # RANKING RULES
  # ==============================================================

  rules_df <- list(
    EOO = data.frame(
      EOOVal = c(100, 250, 1000, 5000, 20000, 200000, 2500000, Inf),
      EOOScore = c(0, 0.79, 1.57, 2.36, 3.14, 3.93, 4.71, 5.5)
    ),

    AOO = data.frame(
      AOOVal = c(1, 2, 5, 25, 125, 500, 2500, 12500, Inf),
      AOOScore = c(0, 1.38, 2.76, 4.12, 5.50, 6.88, 8.26, 9.62, 11.00)
    ),

    Num = data.frame(
      NumVal = c(5, 20, 80, 300, Inf),
      NumScore = c(0, 1.38, 2.75, 4.13, 5.5)
    ),

    Rank = data.frame(
      RankVal = c(1.5, 2.5, 3.5, 4.5, Inf),
      RankScore = c("S1", "S2", "S3", "S4", "S5"),
      stringsAsFactors = FALSE
    )
  )

  if (grid_size == 1000) {
    rules_df$AOO$AOOVal <-
      c(4, 10, 20, 100, 500, 2000, 10000, 50000, Inf)
  }

  rules_AOO_df_community <- list(
    AOOsmall = data.frame(
      AOOsmallVal = c(0.1, 0.5, 1, 2, 5, 20, 100, 500, Inf),
      AOOsmallScore = c(0, 1.38, 2.76, 4.12, 5.50, 6.88, 8.26, 9.62, 11.00)),
    AOOlarge = data.frame(
      AOOlargeScore = c(0, 1.38, 2.76, 4.12, 5.50, 6.88, 8.26, 9.62, 11.00),
      AOOlargeVal = c(1, 5, 10, 20, 100, 500, 2000, 20000, Inf)),
    AOOmatrix = data.frame(
      AOOmatrixScore = c(0, 1.38, 2.76, 4.12, 5.50, 6.88, 8.26, 9.62, 11.00),
      AOOmatrixVal = c(10, 30, 100, 300, 1000, 5000, 25000, 200000, Inf))
  )

  # ==============================================================
  # GENERIC NUMERIC SCORING
  # ==============================================================

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

  score_eoo <- function(x)
    assign_points(x, rules_df, "EOO")

  score_num <- function(x)
    assign_points(x, rules_df, "Num")

  score_rank <- function(x)
    assign_points(x, rules_df, "Rank")

  # =================================================================
  # BULKCAT CALCULATION (based only on spatial factors in input data)
  # =================================================================

  eoo_points <- score_eoo(results$eoo_area_km2)
  num_points <- score_num(results$num_eo_calc)

  # ---- AOO ----
  if (community) {
      # first calculate aoo_values based on community patch size

      aoo_types <- c("AOOsmall", "AOOlarge", "AOOmatrix")

      # calculate points for each patch size
      aoo_points_grid <- setNames(
        lapply(aoo_types, \(type) {
          assign_points(
            results$aoo_km2_grid,
            rules_AOO_df_community,
            type
          )
        }),
        paste0(tolower(aoo_types), "_grid")
      )

      score_lookup <- c(
        "0" = "A",
        "1.38" = "B",
        "2.76" = "C",
        "4.12" = "D",
        "5.5" = "E",
        "6.88" = "F",
        "8.26" = "G",
        "9.62" = "H",
        "11" = "I"
      )

      # Convert numeric values to letter scores
      aoo_scores_grid <- lapply(aoo_points_grid, \(x) {
        unname(score_lookup[as.character(x)])
      })

      results <- results %>%
        mutate(
          AOOsmall_gridScore = aoo_scores_grid$aoosmall_grid,
          AOOlarge_gridScore = aoo_scores_grid$aoolarge_grid,
          AOOmatrix_gridScore = aoo_scores_grid$aoomatrix_grid,
          AOOsmall_gridPoints = aoo_points_grid$aoosmall_grid,
          AOOlarge_gridPoints = aoo_points_grid$aoolarge_grid,
          AOOmatrix_gridPoints = aoo_points_grid$aoomatrix_grid
        )

      # using grid values
      results$RankSpatial_smallPatch_grid <-
        score_rank(((eoo_points +
                       aoo_points_grid$aoosmall_grid +
                       num_points) / 4))

      results$RankSpatial_largePatch_grid <-
        score_rank(((eoo_points +
                       aoo_points_grid$aoolarge_grid +
                       num_points) / 4))

      results$RankSpatial_matrix_grid <-
        score_rank(((eoo_points +
                       aoo_points_grid$aoomatrix_grid +
                       num_points) / 4))

      if ("aoo_km2" %in% names(results)) {
        aoo_points <- setNames(
          lapply(aoo_types, \(type) {
            assign_points(
              results$aoo_km2,
              rules_AOO_df_community,
              type
            )
          }),
          tolower(aoo_types))

        # Convert numeric values to letter scores
        aoo_scores <- lapply(aoo_points, \(x) {
          unname(score_lookup[as.character(x)])
        })

        results <- results %>%
          mutate(
            AOOsmall_Score = aoo_scores$aoosmall,
            AOOlarge_Score = aoo_scores$aoolarge,
            AOOmatrix_Score = aoo_scores$aoomatrix,
            AOOsmall_Points = aoo_points$aoosmall,
            AOOlarge_Points = aoo_points$aoolarge,
            AOOmatrix_Points = aoo_points$aoomatrix
          )

        results$RankSpatial_smallPatch <-
          score_rank(((eoo_points +
                         aoo_points$aoosmall +
                         num_points) / 4))

        results$RankSpatial_largePatch <-
          score_rank(((eoo_points +
                         aoo_points$aoolarge +
                         num_points) / 4))

        results$RankSpatial_matrix <-
          score_rank(((eoo_points +
                         aoo_points$aoomatrix +
                         num_points) / 4))
      }



      # calculate option without AOO
      results$RankSpatial_no_AOO <-
        score_rank(((eoo_points +
                       num_points) / 2))

  } else {
    aoo_points <- assign_points(
      results$aoo_num_cells,
      rules_df,
      "AOO"
    )
    results$PointsSpatial <-
      ((eoo_points +
         aoo_points +
         num_points) / 4)

    results$RankSpatial <- score_rank(results$PointsSpatial)
  }

  # if there are no additional user-supplied factors, then the process is complete.
  if (is.null(factors_df)) {
    return(results)
  }

  ###############################################
  # Point conversions for additional rank factors
  ###############################################

  # Population size
  pop_size_scores <- c(
    Z = 0,
    A = 0,
    B = 1.58,
    C = 3.14,
    D = 4.72,
    E = 6.28,
    F = 7.86,
    G = 9.42,
    H = 11.0
  )

eoo_scores <- c(
    A = 0,
    B = 0.79,
    C = 1.57,
    D = 2.36,
    E = 3.14,
    F = 3.93,
    G = 4.71,
    H = 5.5
  )

 num_eo_scores <- c(
    A = 0,
    B = 1.38,
    C = 2.75,
    D = 4.13,
    E = 5.5
  )

  aoo_scores <- c(
    A = 0,
    B = 1.38,
    C = 2.76,
    D = 4.12,
    E = 5.50,
    F = 6.88,
    G = 8.26,
    H = 9.62,
    I = 11.00
  )

  # number of Good EOs / percent good area
  good_eo_scores <- c(
    A = 0,
    B = 2.2,
    C = 4.4,
    D = 6.6,
    E = 8.8,
    F = 11.0
  )

  # Overall Threat
  threat_scores <- c(
    A = 0,
    B = 1.83,
    C = 3.67,
    D = 5.5
  )

  # Intrinsic Vulnerability
  vulnerability_scores <- c(
    A = 0,
    B = 2.75,
    C = 5.5
  )

  # long-term trend
  trend_long_scores <- c(
    A = -0.50,
    B = -0.40,
    C = -0.31,
    D = -0.22,
    E = -0.14,
    F = -0.07,
    G =  0.00,
    H =  0.07,
    I =  0.14
  )

  # short-term trend
  trend_short_scores <- c(
    A = -1.00,
    B = -0.80,
    C = -0.62,
    D = -0.44,
    E = -0.28,
    F = -0.14,
    G =  0.00,
    H =  0.14,
    I =  0.28
  )

  # ==============================================================
  # ADD FACTOR COLUMNS TO RESULTS
  #
  # Only add columns that contain at least one non-NA value after filtering to species
  # ==============================================================

  factor_idx <- match(
    results$species,
    factors_df[[sname]]
  )

  factor_cols <- setdiff(
    names(factors_df),
    sname
  )

  factor_cols <- factor_cols[
    vapply(
      factors_df[factor_cols],
      function(x) {
        matched <- !is.na(factor_idx)
        any(!is.na(x[factor_idx[matched]]))
      },
      logical(1)
    )
  ]

  for (col in factor_cols) {
    results[[col]] <- NA
    matched <- !is.na(factor_idx)
    results[[col]][matched] <-
      factors_df[[col]][factor_idx[matched]]
  }

  # ==============================================================
  # Convert factor codes to low/high score ranges
  # ==============================================================

  factor_range <- function(x, scores) {

    x <- toupper(trimws(as.character(x)))

    # Remove spaces / separators that might occur in imported data
    x <- gsub("\\s+", "", x)

    low <- rep(NA_real_, length(x))
    high <- rep(NA_real_, length(x))

    valid <- !is.na(x) &
      x != "" &
      !x %in% c("U", "NA", "NULL") &
      nchar(x) %in% c(1, 2)

    if (!any(valid)) {
      return(list(
        low = low,
        high = high
      ))
    }

    # Extract first and last letter.
    # A, AC, B, BD, etc. become their endpoint codes.
    start_code <- substr(x, 1, 1)
    end_code <- substr(x, nchar(x), nchar(x))

    start_idx <- match(start_code, names(scores))
    end_idx <- match(end_code, names(scores))

    good <- valid &
      !is.na(start_idx) &
      !is.na(end_idx)

    low[good] <- scores[start_idx[good]]
    high[good] <- scores[end_idx[good]]

    # Ensure low <= high
    swap <- good & low > high

    tmp <- low[swap]
    low[swap] <- high[swap]
    high[swap] <- tmp

    list(
      low = low,
      high = high
    )
  }

  # ==============================================================
  # Factor definitions
  #
  # Each factor name corresponds to a column in `results`, and
  # its value is the named vector containing the factor's scores.
  # ==============================================================

  factor_scores <- list(
    pop_size = pop_size_scores,
    num_eo_user = num_eo_scores,
    eoo_user = eoo_scores,
    aoo_user = aoo_scores,
    num_good_eo = good_eo_scores,   # These have the same scoring
    perc_good_area = good_eo_scores, # These have the same scoring
    threat = threat_scores,
    trend_long = trend_long_scores,
    trend_short = trend_short_scores,
    vulnerability = vulnerability_scores
  )

  # ==============================================================
  # Calculate low/high ranges for every factor
  # ==============================================================

  factor_ranges <- lapply(
    names(factor_scores),
    function(factor) {

      if (factor %in% names(results)) {

        factor_range(
          results[[factor]],
          factor_scores[[factor]]
        )

      } else {

        list(
          low = rep(NA_real_, nrow(results)),
          high = rep(NA_real_, nrow(results))
        )
      }
    }
  )

  names(factor_ranges) <- names(factor_scores)
  ##############################################
  # add points to factor_ranges based for the EOO, AOO, and number of EOs determined by BulkCAT
  ###############################################
  # num EOs
  factor_ranges$num_eo_calc <- list(
    low = assign_points(results$num_eo_calc, rules_df, "Num"),
    high = assign_points(results$num_eo_calc, rules_df, "Num")
  )

  # AOO
  if (!community){
  factor_ranges$aoo_calc <- list(
    low = assign_points(results$aoo_num_cells, rules_df, "AOO"),
    high = assign_points(results$aoo_num_cells, rules_df, "AOO")
  )} else{
      # direct or not, and patch type or not
      if ("aoo_patch_type" %in% names(results)) {
          if ("aoo_km2" %in% names(results)){
            aoo_val <- aoo_points$aoolarge # default for large/unknown/NA

            small_patch <- !is.na(results$aoo_patch_type) &
              results$aoo_patch_type == "Small patch"

            matrix_patch <- !is.na(results$aoo_patch_type) &
              results$aoo_patch_type == "Matrix"

            aoo_val[small_patch] <- aoo_points$aoosmall[small_patch]
            aoo_val[matrix_patch] <- aoo_points$aoomatrix[matrix_patch]

            factor_ranges$aoo_calc <- list(
              low = aoo_val,
              high = aoo_val
            )
          }
          # grid cell method
          aoo_val <- aoo_points_grid$aoolarge_grid # default for large/unknown/NA

          small_patch <- !is.na(results$aoo_patch_type) &
            results$aoo_patch_type == "Small patch"

          matrix_patch <- !is.na(results$aoo_patch_type) &
            results$aoo_patch_type == "Matrix"

          aoo_val[small_patch] <- aoo_points_grid$aoosmall_grid[small_patch]
          aoo_val[matrix_patch] <- aoo_points_grid$aoomatrix_grid[matrix_patch]

          factor_ranges$aoo_calc_grid <- list(
            low = aoo_val,
            high = aoo_val
          )

          } else {
              print("No patch type provided, assuming large patch type for all communities")
              factor_ranges$aoo_calc_grid <- list(
                low = aoo_points_grid$aoolarge,
                high = aoo_points_grid$aoolarge
                    )
              if ("aoo_km2" %in% names(results)){
                factor_ranges$aoo_calc <- list(
                  low = aoo_points$aoolarge,
                  high = aoo_points$aoolarge
                )
              }
          }
      }

  # EOO
  factor_ranges$eoo_calc <- list(
    low = assign_points(results$eoo_area_km2, rules_df, "EOO"),
    high = assign_points(results$eoo_area_km2, rules_df, "EOO")
  )

  # ==============================================================
  # Row-wise factor overrides
  # ==============================================================

  # --------------------------------------------------------------
  # 1. If threat is present, ignore vulnerability
  # --------------------------------------------------------------

  if ("threat" %in% names(results) && "vulnerability" %in% names(results)) {

    threat_present <- !is.na(results$threat) &
      trimws(as.character(results$threat)) != "" &
      !toupper(trimws(as.character(results$threat))) %in% c("U", "NA", "NULL")

    factor_ranges$vulnerability$low[threat_present] <- NA_real_
    factor_ranges$vulnerability$high[threat_present] <- NA_real_
  }

  # --------------------------------------------------------------
  # 2. If both good EO and good area are present, use the lower
  #    of the two scores for both low and high
  # --------------------------------------------------------------

  if ("num_good_eo" %in% names(results) &&
      "perc_good_area" %in% names(results)) {

    good_eo_present <- !is.na(factor_ranges$num_good_eo$low) &
      !is.na(factor_ranges$num_good_eo$high)

    good_area_present <- !is.na(factor_ranges$perc_good_area$low) &
      !is.na(factor_ranges$perc_good_area$high)

    both_present <- good_eo_present & good_area_present

    # Take the lower score at each endpoint
    factor_ranges$num_good_eo$low[both_present] <-
      pmin(
        factor_ranges$num_good_eo$low[both_present],
        factor_ranges$perc_good_area$low[both_present]
      )

    factor_ranges$num_good_eo$high[both_present] <-
      pmin(
        factor_ranges$num_good_eo$high[both_present],
        factor_ranges$perc_good_area$high[both_present]
      )
  }

  # ==============================================================
  # CALCULATE RARITY
  # ==============================================================
  # Return a factor's low/high scores, or all NA if the factor
  # was not provided for any species.
  get_factor <- function(factor, range) {

    if (!factor %in% names(factor_ranges)) {
      return(rep(NA_real_, nrow(results)))
    }
    out <- factor_ranges[[factor]][[range]]
    return(out)
  }

  # Calculate the mean of all non-NA factor scores.
  # Returns NA if all factors are NA for a row.
  mean_non_na <- function(..., weights = NULL) {

    x <- cbind(...)

    if (is.null(weights)) {
      weights <- rep(1, ncol(x))
    }

    weight_sum <- rowSums(
      matrix(
        rep(weights, each = nrow(x)),
        nrow = nrow(x)
      ) * !is.na(x)
    )

    out <- rowSums(x, na.rm = TRUE) / weight_sum

    out[weight_sum == 0] <- NA_real_

    out
  }

  # for species
  if (!community) {

    # ---- EO-calculated rarity ----
    # pop_size, num_eo_calc, eoo_calc, aoo_calc, num_good_eo

    results$Points_rarity_eo_calc_low <- mean_non_na(
      get_factor("pop_size", "low"),
      get_factor("num_eo_calc", "low"),
      get_factor("eoo_calc", "low"),
      get_factor("aoo_calc", "low"),
      get_factor("num_good_eo", "low"),
      weights = c(2, 1, 1, 2, 2)
    )

    results$Points_rarity_eo_calc_high <- mean_non_na(
      get_factor("pop_size", "high"),
      get_factor("num_eo_calc", "high"),
      get_factor("eoo_calc", "high"),
      get_factor("aoo_calc", "high"),
      get_factor("num_good_eo", "high"),
      weights = c(2, 1, 1, 2, 2)
    )

    # ---- User-supplied EO rarity ----
    # pop_size, num_eo_user, eoo_calc, aoo_calc, num_good_eo
    if ("num_eo_user" %in% names(factor_ranges)){
      results$Points_rarity_eo_user_low <- mean_non_na(
        get_factor("pop_size", "low"),
        get_factor("num_eo_user", "low"),
        get_factor("eoo_calc", "low"),
        get_factor("aoo_calc", "low"),
        get_factor("num_good_eo", "low"),
        weights = c(2, 1, 1, 2, 2)
      )

      results$Points_rarity_eo_user_high <- mean_non_na(
        get_factor("pop_size", "high"),
        get_factor("num_eo_user", "high"),
        get_factor("eoo_calc", "high"),
        get_factor("aoo_calc", "high"),
        get_factor("num_good_eo", "high"),
        weights = c(2, 1, 1, 2, 2)
      )

      # for the taxa with num_eo_user = NA, these should be the same as Points_rarity_eo_calc_low/high
      na_eo_user <- is.na(results$num_eo_user)

      results$Points_rarity_eo_user_low[na_eo_user] <-
        results$Points_rarity_eo_calc_low[na_eo_user]

      results$Points_rarity_eo_user_high[na_eo_user] <-
        results$Points_rarity_eo_calc_high[na_eo_user]
    }
  } else{
      # for communities, it is important to distinguish between aoo_user and aoo_calc rather than num_eos
      # ---- AOO-calculated rarity ----
      # pop_size, num_eo_calc, eoo_calc, aoo_calc, num_good_eo

      # direct method
      if ("aoo_km2" %in% names(results)) {
        Points_rarity_aoo_calc_low <- mean_non_na(
          get_factor("pop_size", "low"),
          get_factor("num_eo_calc", "low"),
          get_factor("eoo_calc", "low"),
          get_factor("aoo_calc", "low"),
          get_factor("num_good_eo", "low"),
          weights = c(2, 1, 1, 2, 2)
        )

        Points_rarity_aoo_calc_high <- mean_non_na(
          get_factor("pop_size", "high"),
          get_factor("num_eo_calc", "high"),
          get_factor("eoo_calc", "high"),
          get_factor("aoo_calc", "high"),
          get_factor("num_good_eo", "high"),
          weights = c(2, 1, 1, 2, 2)
        )
      }
    Points_rarity_aoo_calc_grid_low <- mean_non_na(
      get_factor("pop_size", "low"),
      get_factor("num_eo_calc", "low"),
      get_factor("eoo_calc", "low"),
      get_factor("aoo_calc_grid", "low"),
      get_factor("num_good_eo", "low"),
      weights = c(2, 1, 1, 2, 2)
    )

    Points_rarity_aoo_calc_grid_high <- mean_non_na(
      get_factor("pop_size", "high"),
      get_factor("num_eo_calc", "high"),
      get_factor("eoo_calc", "high"),
      get_factor("aoo_calc_grid", "high"),
      get_factor("num_good_eo", "high"),
      weights = c(2, 1, 1, 2, 2)
    )

    # without AOO
    Points_rarity_no_aoo_low <- mean_non_na(
      get_factor("pop_size", "low"),
      get_factor("num_eo_calc", "low"),
      get_factor("eoo_calc", "low"),
      get_factor("num_good_eo", "low"),
      weights = c(2, 1, 1, 2)
    )

    Points_rarity_no_aoo_high <- mean_non_na(
      get_factor("pop_size", "high"),
      get_factor("num_eo_calc", "high"),
      get_factor("eoo_calc", "high"),
      get_factor("num_good_eo", "high"),
      weights = c(2, 1, 1, 2)
    )

      # ---- User-supplied AOO rarity ----
      # pop_size, num_eo_user, eoo_calc, aoo_user, num_good_eo
      if ("aoo_user" %in% names(factor_ranges)){
        Points_rarity_aoo_user_low <- mean_non_na(
          get_factor("pop_size", "low"),
          get_factor("num_eo_calc", "low"),
          get_factor("eoo_calc", "low"),
          get_factor("aoo_user", "low"),
          get_factor("num_good_eo", "low"),
          weights = c(2, 1, 1, 2, 2)
        )

        Points_rarity_aoo_user_high <- mean_non_na(
          get_factor("pop_size", "high"),
          get_factor("num_eo_calc", "high"),
          get_factor("eoo_calc", "high"),
          get_factor("aoo_user", "high"),
          get_factor("num_good_eo", "high"),
          weights = c(2, 1, 1, 2, 2)
        )
      }
    }

  # ==============================================================
  # THREATS
  # Threat points are multiplied by the overall Threat weight = .3
  # ==============================================================
  results$Points_threats_user_low <- mean_non_na(
    get_factor("threat", "low"),
    get_factor("vulnerability", "low"))
  results$Points_threats_user_high <- mean_non_na(
    get_factor("threat", "high"),
    get_factor("vulnerability", "high"))

  # ==============================================================
  # TRENDS
  #
  # These values are already weighted according to the calculator.
  # Therefore they are added directly to the final points.
  #
  # A range such as AC produces:
  #   low  = A contribution
  #   high = C contribution
  # ==============================================================

  trend_low <- cbind(
    get_factor("trend_short", "low"),
    get_factor("trend_long", "low")
  )

  trend_high <- cbind(
    get_factor("trend_short", "high"),
    get_factor("trend_long", "high")
  )

  results$Points_trend_low <-
    rowSums(trend_low, na.rm = TRUE)

  results$Points_trend_high <-
    rowSums(trend_high, na.rm = TRUE)

  # Both trends missing -> NA
  results$Points_trend_low[
    rowSums(!is.na(trend_low)) == 0
  ] <- NA_real_

  results$Points_trend_high[
    rowSums(!is.na(trend_high)) == 0
  ] <- NA_real_

  # ==============================================================
  # FINAL POINT RANGE
  # ==============================================================


  # if threat points are NA for a species, then rarity should be multiplied by 1.0. trends may be NA
  # If trend points are NA, treat them as 0.

  rarity_multiplier <- ifelse(
    is.na(results$Points_threats_user_low),
    1.0,
    0.7
  )

  trend_low <- ifelse(
    is.na(results$Points_trend_low),
    0,
    results$Points_trend_low
  )

  trend_high <- ifelse(
    is.na(results$Points_trend_high),
    0,
    results$Points_trend_high
  )

  threat_low <- ifelse(
    is.na(results$Points_threats_user_low),
    0,
    results$Points_threats_user_low
  )

  threat_high <- ifelse(
    is.na(results$Points_threats_user_high),
    0,
    results$Points_threats_user_high
  )

  ##########################
  # handle eo_calc/eo_user and aoo_calc/aoo_user for communities and species separately

  if (!community) {

    results$Points_factors_eo_calc_low <-
      rarity_multiplier * results$Points_rarity_eo_calc_low +
      0.3 * threat_low +
      trend_low

    results$Points_factors_eo_calc_high <-
      rarity_multiplier * results$Points_rarity_eo_calc_high +
      0.3* threat_high +
      trend_high

    # if there were any user supplied EO counts in factors_df
    if ("Points_rarity_eo_user_low" %in% names(results)){
      results$Points_factors_eo_user_low <-
        rarity_multiplier * results$Points_rarity_eo_user_low +
        0.3* threat_low +
        trend_low

      results$Points_factors_eo_user_high <-
        rarity_multiplier * results$Points_rarity_eo_user_high +
        0.3* threat_high +
        trend_high
    }
  } else {

    if ("aoo_km2" %in% names(results)){
      results$Points_factors_aoo_calc_low <-
        rarity_multiplier * Points_rarity_aoo_calc_low +
        0.3 * threat_low +
        trend_low

      results$Points_factors_aoo_calc_high <-
        rarity_multiplier * Points_rarity_aoo_calc_high +
        0.3* threat_high +
        trend_high
    }

    # if there were any user supplied EO counts in factors_df
    if ("Points_rarity_aoo_user_low" %in% names(results)){
      results$Points_factors_aoo_user_low <-
        rarity_multiplier * Points_rarity_aoo_user_low +
        0.3* threat_low +
        trend_low

      results$Points_factors_aoo_user_high <-
        rarity_multiplier * Points_rarity_aoo_user_high +
        0.3* threat_high +
        trend_high
    }

    # calculate points based on grid cell AOO
    results$Points_factors_aoo_calc_grid_low <-
      rarity_multiplier * Points_rarity_aoo_calc_grid_low +
      0.3 * threat_low +
      trend_low

    results$Points_factors_aoo_calc_grid_high <-
      rarity_multiplier * Points_rarity_aoo_calc_grid_high +
      0.3* threat_high +
      trend_high
  }

  #########################################
  # Final ranking rollup (flexible to Points_factor column names)

  return(
    list(
    results = assign_rank(results),
    factor_ranges = factor_ranges)
  )
}
