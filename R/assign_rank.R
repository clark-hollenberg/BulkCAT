  # ==============================================================
  # ASSIGN NATURESERVE-STYLE RANGE RANKS FROM POINT RANGES
  # ==============================================================

  assign_rank  <- function(
    results,
    prefix = "Points_factors",
    single_threshold = 0.95,
    question_threshold = 0.80
  ) {

    # ------------------------------------------------------------
    # Find low/high point columns
    # ------------------------------------------------------------

    low_cols <- grep(
      paste0("^", prefix, "_.*_low$"),
      names(results),
      value = TRUE
    )

    high_cols <- grep(
      paste0("^", prefix, "_.*_high$"),
      names(results),
      value = TRUE
    )

    low_vars <- sub(
      paste0("^", prefix, "_(.*)_low$"),
      "\\1",
      low_cols
    )

    high_vars <- sub(
      paste0("^", prefix, "_(.*)_high$"),
      "\\1",
      high_cols
    )

    variables <- intersect(low_vars, high_vars)


    # ------------------------------------------------------------
    # Rank boundaries
    # ------------------------------------------------------------

    rank_bounds <- c(
      S1 = -2,
      S2 = 1.5,
      S3 = 2.5,
      S4 = 3.5,
      S5 = 4.5
    )

    rank_names <- names(rank_bounds)

    # Upper boundary of S5
    max_score <- 5.5


    # ------------------------------------------------------------
    # Convert a score to a rank
    # ------------------------------------------------------------

    score_to_rank <- function(x) {
      out <- rep(NA_character_, length(x))
      ok <- !is.na(x)

      if (any(ok)) {
        idx <- findInterval(x[ok], rank_bounds)

        idx[idx < 1] <- 1
        idx[idx > length(rank_names)] <- length(rank_names)

        out[ok] <- rank_names[idx]
      }

      out
    }


    # ------------------------------------------------------------
    # Determine range rank for one low/high pair
    # ------------------------------------------------------------

    calculate_range_rank <- function(low, high) {

      if (is.na(low) && is.na(high)) {
        return(NA_character_)
      }

      if (is.na(low)) {
        low <- high
      }

      if (is.na(high)) {
        high <- low
      }

      # Ensure correct ordering
      lower <- min(low, high)
      upper <- max(low, high)

      # Exact same score = exact rank
      if (lower == upper) {
        return(score_to_rank(lower))
      }

      # Calculate amount of the score range in each rank
      rank_lower <- c(-2, 1.5, 2.5, 3.5, 4.5)
      rank_upper <- c(1.5, 2.5, 3.5, 4.5, max_score)

      overlap <- pmax(
        0,
        pmin(upper, rank_upper) -
          pmax(lower, rank_lower)
      )

      proportions <- overlap / (upper - lower)

      # ----------------------------------------------------------
      # Rule 2: >= 95% in one rank
      # ----------------------------------------------------------

      max_prop <- max(proportions)

      if (max_prop >= single_threshold) {

        rank <- rank_names[which.max(proportions)]

        return(rank)
      }


      # ----------------------------------------------------------
      # Rule 3: >= 80% but < 95% in one rank
      # ----------------------------------------------------------

      if (max_prop >= question_threshold) {

        rank <- rank_names[which.max(proportions)]

        return(paste0(rank, "?"))
      }


      # ----------------------------------------------------------
      # Rule 4: >= 95% in two ranks
      # ----------------------------------------------------------

      two_prop <- sort(proportions, decreasing = TRUE)[1:2]

      if (sum(two_prop) >= single_threshold) {

        ranks <- rank_names[
          order(proportions, decreasing = TRUE)[1:2]
        ]

        ranks <- sort(
          as.numeric(sub("S", "", ranks))
        )

        return(paste0("S", ranks[1], "S", ranks[2]))
      }


      # ----------------------------------------------------------
      # Rule 5: >= 95% in three ranks
      # ----------------------------------------------------------

      three_prop <- sort(proportions, decreasing = TRUE)[1:3]

      if (sum(three_prop) >= single_threshold) {

        ranks <- rank_names[
          order(proportions, decreasing = TRUE)[1:3]
        ]

        ranks <- sort(
          as.numeric(sub("S", "", ranks))
        )

        return(
          paste0(
            "S", ranks[1],
            "S", ranks[3]
          )
        )
      }

      # ----------------------------------------------------------
      # Fallback
      # ----------------------------------------------------------

      # If none of the thresholds are met, return the ranks
      # represented by the endpoints.
      low_rank <- score_to_rank(lower)
      high_rank <- score_to_rank(upper)

      if (low_rank == high_rank) {
        return(low_rank)
      }

      paste0(low_rank, high_rank)
    }


    # ------------------------------------------------------------
    # Apply to every factor
    # ------------------------------------------------------------

    for (variable in variables) {

      low_col <- paste0(prefix, "_", variable, "_low")
      high_col <- paste0(prefix, "_", variable, "_high")

      rank_col <- paste0("Rank_factors_", variable)

      results[[rank_col]] <- mapply(
        calculate_range_rank,
        results[[low_col]],
        results[[high_col]]
      )
    }

    return(results)
  }
