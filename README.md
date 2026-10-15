# BulkCAT

Multispecies NatureServe element rank calculator

Created by Clark Hollenberg at the Colorado Natural Heritage Program (CNHP), October 2024

---

## **Introduction**

BulkCAT calculates conservation status metrics used in the NatureServe framework to support semi-automated bulk assessments. The package implements a bulk-processing version of the "Conservation status Assessment Tool" (CAT) methodology from NatureServe. Spatial rank factors for multiple species can be calculated using user-supplied point or polygon occurrence data with WGS84 latitude and longitude coordinates. Additional non-spatial rank factors (e.g. trends and threats) can be supplied by the user to complete the assessment process and calculate a rank. BulkCAT can also be used for plant associations/communities. 

For each species, it computes range extent/extent of occurrence (EOO in km²), area of occupancy (AOO, usually number of 2x2 km grid cells), number of hypothetical Element Occurrences (EOs), and an overall rarity rank (subnational rank - SRank).  This allows efficient multi-species assessments at scales not feasible with interactive tools like GeoCAT or RARECAT. Calculated values may differ from RARECAT by ~1% for large datasets.

---

## **Installation**

BulkCAT requires R/RStudio to be installed on your computer. If you still need to do this, follow these installation [instructions](https://rstudio-education.github.io/hopr/starting.html).

You can install BulkCAT directly from GitHub using the `remotes` package:

```r
# Install remotes if you don't already have it
install.packages("remotes")

# Install BulkCAT from GitHub
remotes::install_github("clark-hollenberg/BulkCAT")
```

After installation, load the package and review function help pages.

```r
library(BulkCAT)

# access function help pages
?run_bulkCAT
?tabulate_eos
?deduplicate
?filter_eo_shp
?eo_to_centroids
```
---

## **Example usage**

```r
# Load package
library(BulkCAT)

# Load example input CSV (if included in the package)
csv_path <- "path/to/input_file.csv"
input_df <- read.csv(csv_path)

# Remove duplicates (optional)
input_df <- deduplicate(
  input_df = input_df,
  cols = c("recordedBy", "recordNumber", "scientificName", "eventDate"),
  institution_col = "institutionCode"
)

# Run BulkCAT
results <- run_bulkCAT(input_df = input_df,
                        poly_layer = NULL,          # optional polygon layer
                        factors_df = NULL,          # optional supplemental factors
                        community = FALSE,          # if assessing plant community dataset
                        sname = "scientificName",   # column name
                        lat = "decimalLatitude",    # column name
                        lon = "decimalLongitude",   # column name
                        eo_separation = 2000,       # meters
                        grid_size = 2000,           # meters
                        trinomial_synon = FALSE)    # match single trinomial names to parent binomial

# View results
head(results)

# calculate impact of high/med/low threat options (if not done in run_BulkCAT())
results <- calc_threats(
  input_df = results,
  rarity_points_col = "PointsSpatial",
  trend_points_col = NULL
)

# export to csv
output_path <- "path/to/output.csv
results.to_csv(output_path)

```
---

## **Input data for ranking**

BulkCAT requires users to download and pre-filter their own data. Natural heritage programs in the NatureServe network often have occurrence datasets with private coordinates that are not available on GBIF.  See the "download_sf_for_ranking.sql" query to download source features for ranking from the NatureServe Biotics database. For exploratory analysis, GBIF querying is available in NatureServe's RARECAT Shiny app at https://natureserve.shinyapps.io/RARECAT/. See the "rgbif" package for integrating GBIF downloads directly into your BulkCAT pipeline. Data filtering and cleaning may vary depending on the taxonomic group, region, and data sources. For example, an analysis for understudied organisms may want to include data with poor spatial accuracy. See the "CoordinateCleaner" package for standard data filtering tools. The [synon]((https://github.com/clark-hollenberg/synon) R package was created to assist with taxonomic harmonization for rare plants and may be helpful for preparing BulkCAT datasets. BulkCAT can be applied for any species or plant community and can support subnational, national, or global assessments.

To run BulkCAT for Colorado's vascular plants at CNHP, we downloaded SEINet and iNaturalist research grade observations.Note that GBIF downloads may have obscured locations for some species. A SEINet login with special permissions allows use of the most accurate locations available. Scientific names were translated to SNAMEs used in Biotics using [synon](https://github.com/clark-hollenberg/synon). synon uses Biotics synonyms, rWCVP, Symbiota, GNAME/SNAME mapping, and optional infraspecific epithet dropping to reach \~99% coverage. Deduplication was performed conservatively to avoid deleting unique records, prioritizing duplicates based on herbarium record counts for the sample region (Colorado). If you need guidance on how to use these tools for your state, contact me.

BulkCAT is designed to be used with either a points or polygon dataset. A polygon layer may especially be useful for ranking plant communities.

If using a point dataset, the input dataframe should include at least three columns:

* `scientificName` – species name
* `decimalLatitude` – decimal degrees in WGS84
* `decimalLongitude` – decimal degrees in WGS84

You can change the names of these columns, but if you do, they must also be modified in the function call. A polygon sf object or shapefile path only needs to have the species name field. 

---

## **Ranking Rules**

This bulk calculator calculates spatial rank factors in the rarity category. There are many other rank factors that require expert evaluation (e.g. short-term trend, overall threat, number of good occurrences). These rank factors can be supplied by the user in a factors dataframe. Expert-evaluated factors can be downloaded from NatureServe Network databases (Biotics) and combined with spatial calculations in BulkCAT for a complete assessment. See the "BulkCAT_factors_df.sql" query for download from the Biotics ESR database. For previously unassessed species, provisional rank factors may be tested in bulk with the calc_threats() function. For some organisms, it might be possible to estimate rank factors systematically based on species characteristics (e.g. all alpine plants might be considered threatened by climate change). Any non-spatial rank factors should be carefully reviewed before assigning a final rank.

From spatial data, BulkCAT calculates:

* **Range Extent (EOO)**
* **Area of Occupancy (AOO)** – double weighted
* **Number of hypothetical EOs**

Ranking rules are based on the **RULES sheet** from the Element Rank Estimator Excel macro workbook (NatureServe): [link](https://www.natureserve.org/products/conservation-rank-calculator/download).

---

## **Infraspecific ranking**

* Binomial species without infraspecific epithet are clustered with any infraspecific taxa for ranking.
  * Example: *Abies lasiocarpa* includes points/polygons from *Abies lasiocarpa var. bifolia*.
* Trinomial names are ranked using only exact matches.
* If there is exactly one infraspecies for a given binomial name in the dataset, the trinomial will be considered synonymous with the binomial. (this can be disabled with trinomial_match = FALSE)

---

## **Range Extent (EOO)**

* Range extent is calculated with the minimum convex hull that includes all point occurrences or polygon centroids
* If necessary, occurrences will reprojected to a polar or antimeridian coordinate system
  
---

## **Area of Occupancy (AOO)**

* For species, AOO is calculated based on the number of grid cells (either 2x2km or 1x1km) that intersect the point/poly occurrence layer.
* Plant communities use direct estimate of area of occupancy in sq-km rather than the number of occupied cells. BulkCAT calculates AOO with direct measurements of polygon area (if polygons are supplied) and grid cell intersections for comparison. NatureServe generally uses direct area AOO, but grid cells may be more applicable for some communities.
  * AOO based on grid cell intersection is always calculated
  * AOO based on polygon area is calculated if a polygon layer is supplied
  * Rank calculations are completed with up to four alternative AOO options (grid cell, direct area, user-defined AOO estimates, and without AOO)
  * AOO category metrics are variable based on the typical ecosystem patch size (small, large, or matrix). Large patch is used by default.
  * Typical patch size and expert-estimated AOO can be supplied in factors_df for communities
---

## **Estimating Number of EOs**

* By default, occurrences are clustered using a **2 km separation distance**.
* This is adjustable via the function arguments or at the start of the script.
* If number of occurrences has been pre-determined, this can be supplied in factors_df
  * The tabulate_eos() function is designed to calculate for each element
    * number of EOs
    * number of good EOs 
  
---

## **Supplemental rank factors (factors_df)**

* Supplemental rank factors may be included in the final rank by supplying the calculator with a "factors_df", see "factors_df_example.xlsx".
  * Column names must match exactly
  * The use of supplemental rank factors is designed to assist with database QC as spatial data rapidly shift
  
---
