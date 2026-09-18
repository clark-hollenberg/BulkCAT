#' Deduplicate Records by Specified Columns
#'
#' Removes duplicate records from a data frame based on specified columns,
#' giving priority to institutions with more records.
#'
#' @param input_df A data frame containing the records to deduplicate.
#' @param cols A character vector of column names to check for duplicates.
#'   Defaults to c("recordedBy", "recordNumber", "scientificName", "eventDate").
#' @param institution_col A string specifying the column name used to prioritize
#'   records by institution count. Defaults to "institutionCode".
#' @return A data frame with duplicates removed. Prints the number of records removed.
#' @export
#'
deduplicate <- function(input_df, cols = c("recordedBy", "recordNumber", "scientificName", "eventDate"), institution_col = "institutionCode") {
  # Step 1: count records per institution
  inst_counts <- table(input_df[[institution_col]])

  # Step 2: add counts back as a helper column
  input_df$inst_count <- inst_counts[input_df[[institution_col]]]

  # Step 3: order rows by inst_count (descending), then by original order
  ord <- order(-input_df$inst_count, seq_len(nrow(input_df)))
  input_df <- input_df[ord, ]

  # Step 4: drop duplicates based on user-defined columns
  dedup_df <- input_df[!duplicated(input_df[cols]), ]

  # Remove helper column
  dedup_df$inst_count <- NULL
  duplicates <- nrow(input_df) - nrow(dedup_df)
  cat("Observations removed via deduplication:", duplicates, "\n")

  return(dedup_df)
}
