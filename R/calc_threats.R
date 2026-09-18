#' Show the effects of different potential threat options on calculated ranks.
#'
#' Includes low, medium, and high threat options, calculating SRank_lowT, SRank_medT, SRank_highT
#' to assist with review of rarity-based ranks.
#'
#' @param input_df A data frame containing the records to deduplicate {usually an output of runBulkCAT()}
#' @param rarity_points_col A string specifying the column name which contains rarity-based points.
#'   Defaults to "PointsSpatial".
#' @param trends_col A string specifying the column name which contains rarity-based points.
#'   Defaults to "Points".
#' @return A data frame calculated SRanks and Points for different threat options.
#' @export
#'
calc_threats <- function(input_df, rarity_points_col = "PointsSpatial") {
  # create rules_df based on NS methodology (same as in run_BulkCAT())
  rules_df <- data.frame(
    RankVal = c(1.5, 2.5, 3.5, 4.5, 6, NA, NA, NA, NA),
    RankScore = c("S1", "S2", "S3", "S4", "S5", NA, NA, NA, NA),
    stringsAsFactors = FALSE
  )

  assign_points <- function(value, rules, metric) {
    val_col <- paste0(metric, "Val")
    score_col <- paste0(metric, "Score")
    idx <- which(value <= rules[[val_col]])[1]
    if (length(idx)==0) return(0)
    rules[[score_col]][idx]
  }

  score_rank <- function(x) vapply(x, assign_points, character(1), rules = rules_df, metric = "Rank")


  # threat scenarios
  threats <- data.frame(
    suffix = c("_VeryHighThreat", "_HighThreat", "_MedThreat", "_LowThreat"),
    value  = c(0, 1.83, 3.67, 5.5),
    stringsAsFactors = FALSE
  )

  # apply threats
  for (i in seq_len(nrow(threats))) {

    threat <- threats$suffix[i]
    value  <- threats$value[i]
    suffix <- sub("^Points", "", rarity_points_col)

    srank_col_new <- paste0("SRank", suffix, threat)
    rarity_points_col_new <- paste0(rarity_points_col, threat)

    input_df[[rarity_points_col_new]] <- input_df[[rarity_points_col]] * 0.7 + value * 0.3
    input_df[[srank_col_new]]  <- score_rank(input_df[[rarity_points_col_new]])
  }

  return(input_df)
}
