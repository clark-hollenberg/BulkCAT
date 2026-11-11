#' Run BulkCAT without spatial calculations
#'
#' This function completes conservation status assessments (rarity ranks) using NatureServe
#' methodology. run_factors() requires a multi-species dataset with name column sname and rank factor columns. Note that minimum factor requirements are not enforced.
#'
#' @param factors_df Dataframe containing rank factors that are pre-assigned by experts (e.g. download from Biotics). Supplemental factors include threats, trends, and population sizes. See README for additional details on factors_df.
#' @param sname Species name column. Default = "scientificName"
#' @return Dataframe with calculated rarity metrics for each species/element.
#' @export
#'

run_factors <- function(factors_df = NULL,
                        sname = "scientificName") {

  # ----------------------------------------------------------------
  # ----- Input validation -----
  # ----------------------------------------------------------------
  if (is.null(factors_df)){
      stop("factors_df is required. Refer to documentation for formatting.")
    }
    if (!is.data.frame(factors_df))
    {
      stop("factors_df must be a data frame. Refer to documentation for formatting.")
    }
    if (!(sname %in% names(factors_df))) {
      stop("factors_df does not contain scientific name column: ", sname)
    }

  # ----------------------------------------------------------------
  # ----- Rank calculations -----
  # ----------------------------------------------------------------

  ###############################################
  # Point conversions for rank factors
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
  # Each factor name corresponds to a column in `factors_df`, and
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

      if (factor %in% names(factors_df)) {

        factor_range(
          factors_df[[factor]],
          factor_scores[[factor]]
        )

      } else {

        list(
          low = rep(NA_real_, nrow(factors_df)),
          high = rep(NA_real_, nrow(factors_df))
        )
      }
    }
  )

  names(factor_ranges) <- names(factor_scores)

  # ==============================================================
  # Row-wise factor overrides
  # ==============================================================

  # --------------------------------------------------------------
  # 1. If threat is present, ignore vulnerability
  # --------------------------------------------------------------

  if ("threat" %in% names(factors_df) && "vulnerability" %in% names(factors_df)) {

    threat_present <- !is.na(factors_df$threat) &
      trimws(as.character(factors_df$threat)) != "" &
      !toupper(trimws(as.character(factors_df$threat))) %in% c("U", "NA", "NULL")

    factor_ranges$vulnerability$low[threat_present] <- NA_real_
    factor_ranges$vulnerability$high[threat_present] <- NA_real_
  }

  # --------------------------------------------------------------
  # 2. If both good EO and good area are present, use the lower
  #    of the two scores for both low and high
  # --------------------------------------------------------------

  if ("num_good_eo" %in% names(factors_df) &&
      "perc_good_area" %in% names(factors_df)) {

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
      return(rep(NA_real_, nrow(factors_df)))
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

  # ------- Calculate rarity points -----------------------------
  # pop_size, num_eo_user, eoo_user, aoo_user, num_good_eo
  factors_df$Points_rarity_low <- mean_non_na(
        get_factor("pop_size", "low"),
        get_factor("num_eo_user", "low"),
        get_factor("eoo_user", "low"),
        get_factor("aoo_user", "low"),
        get_factor("num_good_eo", "low"),
        weights = c(2, 1, 1, 2, 2)
      )

  factors_df$Points_rarity_high <- mean_non_na(
        get_factor("pop_size", "high"),
        get_factor("num_eo_user", "high"),
        get_factor("eoo_user", "high"),
        get_factor("aoo_user", "high"),
        get_factor("num_good_eo", "high"),
        weights = c(2, 1, 1, 2, 2)
      )



  # ==============================================================
  # THREATS
  # Threat points are multiplied by the overall Threat weight = .3
  # ==============================================================
  factors_df$Points_threats_user_low <- mean_non_na(
    get_factor("threat", "low"),
    get_factor("vulnerability", "low"))
  factors_df$Points_threats_user_high <- mean_non_na(
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

  factors_df$Points_trend_low <-
    rowSums(trend_low, na.rm = TRUE)

  factors_df$Points_trend_high <-
    rowSums(trend_high, na.rm = TRUE)

  # Both trends missing -> NA
  factors_df$Points_trend_low[
    rowSums(!is.na(trend_low)) == 0
  ] <- NA_real_

  factors_df$Points_trend_high[
    rowSums(!is.na(trend_high)) == 0
  ] <- NA_real_

  # ==============================================================
  # FINAL POINT RANGE
  # ==============================================================


  # if threat points are NA for a species, then rarity should be multiplied by 1.0. trends may be NA
  # If trend points are NA, treat them as 0.

  rarity_multiplier <- ifelse(
    is.na(factors_df$Points_threats_user_low),
    1.0,
    0.7
  )

  trend_low <- ifelse(
    is.na(factors_df$Points_trend_low),
    0,
    factors_df$Points_trend_low
  )

  trend_high <- ifelse(
    is.na(factors_df$Points_trend_high),
    0,
    factors_df$Points_trend_high
  )

  threat_low <- ifelse(
    is.na(factors_df$Points_threats_user_low),
    0,
    factors_df$Points_threats_user_low
  )

  threat_high <- ifelse(
    is.na(factors_df$Points_threats_user_high),
    0,
    factors_df$Points_threats_user_high
  )

  ##########################

  factors_df$Points_factors_all_low <-
      rarity_multiplier * factors_df$Points_rarity_low +
      0.3 * threat_low +
      trend_low

  factors_df$Points_factors_all_high <-
      rarity_multiplier * factors_df$Points_rarity_high +
      0.3* threat_high +
      trend_high

  #########################################
  # Final ranking rollup (flexible to Points_factor column names)

  return(assign_rank(factors_df))

}
