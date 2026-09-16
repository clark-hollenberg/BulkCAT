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

  rules_AOO_df_community <- data.frame(
    AOOlargeVal = c(1, 2, 5, 20, 125, 500, 5000, 50000, Inf),
    AOOmatrixVal = c(10, 30, 100, 300, 1000, 5000, 25000,
                     200000, Inf),
    AOOsmallVal = c(0.1, 0.5, 1, 2, 5, 20, 100, 500, Inf),

    AOOlargeScore = c(0, 1.38, 2.76, 4.12, 5.50, 6.88, 8.26, 9.62, 11.00),
    AOOmatrixScore = c(0, 1.38, 2.76, 4.12, 5.50, 6.88, 8.26, 9.62, 11.00),
    AOOsmallScore = c(0, 1.38, 2.76, 4.12, 5.50, 6.88, 8.26, 9.62, 11.00),

    stringsAsFactors = FALSE
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

      idx <- findInterval(
        value[ok],
        rules[[metric]][[val_col]]
      )

      idx[idx < 1] <- 1
      idx[idx > length(rules[[metric]][[score_col]])] <-
        length(rules[[metric]][[score_col]])

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
    aoo_small <- assign_points(
      results$aoo_km2,
      rules_AOO_df_community,
      "AOOsmall"
    )

    aoo_large <- assign_points(
      results$aoo_km2,
      rules_AOO_df_community,
      "AOOlarge"
    )

    aoo_matrix <- assign_points(
      results$aoo_km2,
      rules_AOO_df_community,
      "AOOmatrix"
    )

    # second, calculate rarity points based for each hypothetical patch size
    results$PointsSpatial_smallPatch <-
      (eoo_points +
         aoo_small +
         num_points / 3)

    results$PointsSpatial_largePatch <-
      (eoo_points +
         aoo_large +
         num_points / 3)

    results$PointsSpatial_matrix <-
      (eoo_points +
         aoo_matrix +
         num_points / 3)

    results$RankSpatial_smallPatch <-
      score_rank(results$PointsSpatial_smallPatch)

    results$RankSpatial_largePatch <-
      score_rank(results$PointsSpatial_largePatch)

    results$RankSpatial_matrix <-
      score_rank(results$PointsSpatial_matrix)

  } else {
    aoo_points <- assign_points(
      results$aoo_num_cells,
      rules_df,
      "AOO"
    )

    results$PointsSpatial <-
      (eoo_points +
         aoo_points +
         score_num(results$num_eo_calc) / 3)

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

 num_eo_scores <- c(
    A = 0,
    B = 1.38,
    C = 2.75,
    D = 4.13,
    E = 5.5
  )

  aoo_scores <- c(
    A = 0,
    B = 0.69,
    C = 1.38,
    D = 2.06,
    E = 2.75,
    F = 3.44,
    G = 4.13,
    H = 4.81,
    I = 5.5
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
  # Only add columns that:
  #   1. exist in factors_df
  #   2. contain at least one non-NA value
  # ==============================================================

  factor_idx <- match(
    results$species,
    factors_df[[sname]]
  )

  factor_cols <- intersect(
    c(
      "pop_size",
      "aoo",
      "eoo",
      "num_eo_user",
      "num_good_eo",
      "perc_good_area",
      "trend_long",
      "trend_short",
      "threat",
      "vulnerability"
    ),
    names(factors_df)
  )

  factor_cols <- factor_cols[
    vapply(
      factors_df[factor_cols],
      function(x) any(!is.na(x)),
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
    eoo = eoo_scores,
    aoo = aoo_scores,
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
###############################################
  # add points to factor_ranges based for the EOO, AOO, and number of EOs determined by BulkCAT
  ###############################################
  factor_ranges$num_eo_calc <- list(
    low = assign_points(results$num_eo_calc, rules_df, "Num"),
    high = assign_points(results$num_eo_calc, rules_df, "Num")
  )

  if (!community){
  factor_ranges$aoo_calc <- list(
    low = assign_points(results$aoo_num_cells, rules_df, "AOO"),
    high = assign_points(results$aoo_num_cells, rules_df, "AOO")
  )} else{
    print("FILL THIS IN!")
  }

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


  return(factor_ranges)


  # ==============================================================
  # CALCULATE RARITY
  # ==============================================================
  # Helper function: calculate mean of non-NA factor scores
  # Returns NA if all factors are NA
  mean_non_na <- function(...) {
    x <- cbind(...)
    n_values <- rowSums(!is.na(x))
    out <- rowSums(x, na.rm = TRUE) / n_values
    out[n_values == 0] <- NA_real_
    return(out)
  }

  if (!community){
    # ---- EO-calculated rarity ----
    # pop_size, num_eo_calc, eoo_calc, aoo_calc, num_good_eo

    results$Points_rarity_eo_calc_low <- mean_non_na(
      factors_df$pop_size$low,
      factors_df$num_eo_calc$low,
      factors_df$eoo_calc$low,
      factors_df$aoo_calc$low,
      factors_df$num_good_eo$low
    )

    results$Points_rarity_eo_calc_high <- mean_non_na(
      factors_df$pop_size$high,
      factors_df$num_eo_calc$high,
      factors_df$eoo_calc$high,
      factors_df$aoo_calc$high,
      factors_df$num_good_eo$high
    )

    # ---- User-supplied EO rarity ----
    # pop_size, num_eo_user, eoo_calc, aoo_calc, num_good_eo

    results$Points_rarity_eo_user_low <- mean_non_na(
      factors_df$pop_size$low,
      factors_df$num_eo_user$low,
      factors_df$eoo_calc$low,
      factors_df$aoo_calc$low,
      factors_df$num_good_eo$low
    )

    results$Points_rarity_eo_user_high <- mean_non_na(
      factors_df$pop_size$high,
      factors_df$num_eo_user$high,
      factors_df$eoo_calc$high,
      factors_df$aoo_calc$high,
      factors_df$num_good_eo$high
    )
  } else{
    "FILL IN!"
  }

  # ==============================================================
  # TRENDS
  #
  # These values are already weighted according to the calculator.
  # Therefore they are added directly to the final points.
  #
  # A range such as AC produces:
  #
  #   low  = A contribution
  #   high = C contribution
  # ==============================================================

  # ==============================================================
  # THREATS
  #
  # Threat points are multiplied by the overall Threat weight = .3
  # ==============================================================



  # ==============================================================
  # FINAL POINT RANGE
  # ==============================================================

  if (!community) {

    # Rarity = 70%
    # Threats = 30%
    # Trends are already weighted and are added directly.

    results$Points_all_factors_low <-
      0.7 * results$Points_rarity +
      0.3 * threat$low +
      trend_low

    results$Points_all_factors_high <-
      0.7 * results$Points_rarity +
      0.3 * threat$high +
      trend_high

    # Useful single value for diagnostics
    results$Points_all_factors <-
      (results$Points_all_factors_low +
         results$Points_all_factors_high) / 2

    # Final SRank based on the calculated interval
    results$SRank_calculated <-
      rank_from_range(
        results$Points_all_factors_low,
        results$Points_all_factors_high
      )
  }

  return(results)
}
