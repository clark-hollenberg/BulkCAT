# sample code for running BulkCAT
######################################

# load example input files
input_df <- read.csv("sample_input_df.csv")
factors_df <- read.csv("factors_df_example.csv")

# rank outputs
results <- run_bulkCAT(input_df = input_df,
                        factors_df = factors_df)

write.csv(results, "example_BulkCAT_results.csv", row.names=FALSE)


# example for communities
results <- run_bulkCAT(input_df = input_df,
                       factors_df = factors_df,
                       community = TRUE)

write.csv(results, "example_BulkCAT_results_community.csv", row.names=FALSE)

#
# example running BulkCAT with internal data
#################################################################################
eo_gdb <- "example_path/CNHPData/CNHPData20260916.gdb"

# load polygon layer
eo_shp <- sf::st_read(
  eo_gdb,
  layer = "AllEOs",
  quiet = TRUE
)

eo_shp_filtered <- filter_eo_shp(eo_shp, remove = list(EORank = c("X", "X?"), Ident=c("?")), sname = "SName")

eo_shp_filtered <- eo_shp_filtered[
  eo_shp_filtered$MajorGroup == "Vascular Plants",
]

eo_count <- tabulate_eos(eo_shp_filtered, EO_rank_col="EORank", sname = "SName")
eo_count <- eo_count %>% rename(scientificName = SName)

eo_points <- eo_to_centroids(eo_shp_filtered, sname="SName")
eo_points <- eo_points %>% rename(scientificName = sname)

factors_df <- read.csv("path_to_actual/biotics_factors_df.csv")

names(factors_df) <- tolower(names(factors_df))

factors_df <- factors_df %>% rename(scientificName = sname)

factors_df <- merge(factors_df, eo_count, by = "scientificName")

# rename columns so that factors_df uses the calculated eo numbers as the user-provided.
factors_df <- factors_df %>% rename(num_eo_biotics = num_eo_user, num_good_eo_biotics = num_good_eo)
factors_df <- factors_df %>% rename(num_eo_user = num_eo_gis_score, num_good_eo = num_good_eo_gis_score)

output <- run_bulkCAT(input_df = eo_points,
                      poly_layer = NULL,
                      factors_df = factors_df,
                      community = FALSE,
                      sname = "scientificName",
                      lat = "decimalLatitude",
                      lon = "decimalLongitude",
                      eo_separation = 2000,
                      grid_size = 2000,
                      trinomial_synon = FALSE)

write.csv(output, "_v2_bulkcat_eo_ranking.csv", row.names=FALSE)

####################################
# example code for calculating ranks using only factors_df (no spatial data)

factors_df <- read.csv("_testing/factors_df_communities.csv")
output <- run_factors(factors_df, sname = "SName")

