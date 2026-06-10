# This script does two things.
# Sub-step 1A finds and stacks all the new individual NDVI TIF files you downloaded
# from QGIS into a single raster object ready for analysis.
# Sub-step 1B then reads the trial plan shapefile and sowing date from your metadata
# Excel file, calculates the average NDVI for each treatment strip on each image date,
# works out days after planting, and saves three CSV files ready for the plotting script.

rm(list = ls())

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(lubridate)
  library(readxl)
})

# ====================== CHANGE THIS NUMBER ONLY ======================
site_number_input <- 1  # 1 through 6
# =====================================================================

site_lookup <- data.frame(
  id = 1:6,
  site_number = c(
    "1.Walpeup_MRS125",
    "2.Crystal_Brook_Brians_House",
    "3.Wynarka_Mervs_West",
    "4.Wharminda_Woodys",
    "5.Walpeup_Gums",
    "6.Crystal_Brook_Randals"
  ),
  site_name = c(
    "Walpeup_MRS125",
    "Crystal_Brook_Brians_House",
    "Wynarka_Mervs_West",
    "Wharminda_Woodys",
    "Walpeup_Gums",
    "Crystal_Brook_Randals"
  ),
  stringsAsFactors = FALSE
)

site_row    <- site_lookup[site_lookup$id == site_number_input, ]
site_number <- site_row$site_number
site_name   <- site_row$site_name

cat("Site selected:", site_number, "\n")

# =====================================================================
# ====================== PATHS and DIRECTORIES ========================
# =====================================================================

year_of_analysis <- 2025
yr_short         <- substr(as.character(year_of_analysis), 3, 4)  # "25"

dir           <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
headDir       <- paste0(dir, "/work/Output-1/", site_number)
metadata_path <- paste0(dir, "/work/Output-1/0.Site-info/")
metadata_file <- "names of treatments per site 2025 metadata and other info.xlsx"

# Folder where the new per-image NDVI TIFs live
ndvi_dir <- file.path(headDir,
                      "7.In_Season_data", yr_short,
                      "8.Sentinel_QGIS_Jackie")

# Folder where output CSVs will be saved (new subfolder inside 8.Sentinel_QGIS_Jackie)
saveDir <- file.path(headDir,
                     "7.In_Season_data", yr_short,
                     "8.Sentinel_QGIS_Jackie",
                     "Growth_curves_output")

if (!dir.exists(saveDir)) {
  dir.create(saveDir, recursive = TRUE)
  message("Created output directory: ", saveDir)
}

################################################################################
##        SUB-STEP 1A: Find, sort and stack NDVI TIF files                    ##
################################################################################

# Files match pattern: *NDVI*10m.tif  (ignores the plain 10m files)
ndvi_files <- list.files(
  path       = ndvi_dir,
  pattern    = "NDVI.*10m\\.tif$",
  full.names = TRUE
)

if (length(ndvi_files) == 0) stop("No NDVI TIF files found in: ", ndvi_dir)
cat("Found", length(ndvi_files), "NDVI TIF files\n")

# Parse dates from filenames (handles yyyy-mm-dd or yyyymmdd formats)
fnames    <- basename(ndvi_files)
dates_8 <- str_extract(fnames, "(?<!\\d)\\d{8}(?!\\d)")


# Pull the trial plan relative path from metadata and build the full path
trial_shp_rel  <- meta %>%
  filter(variable == "trial.plan") %>%
  pull(`file path`) %>%
  .[1]

trial_shp_full <- paste0(headDir, trial_shp_rel)
cat("Reading trial plan:", trial_shp_full, "\n")
trial.plan <- sf::st_read(trial_shp_full, quiet = TRUE)

# Read sowing date from seasons sheet
# read_excel has already parsed the date so just convert directly
seasons <- readxl::read_excel(
  paste0(metadata_path, metadata_file),
  sheet = "seasons"
) %>%
  filter(Site == site_number, Year == year_of_analysis)

if (nrow(seasons) == 0) stop("No sowing date found for site/year in metadata.")

plant_date <- as.Date(seasons$`Sowing date`)
cat("Sowing date:", format(plant_date), "\n")

################################################################################
##        SUB-STEP 1B: Align CRS and extract mean NDVI per treatment strip    ##
################################################################################

# Convert trial plan to terra format and align CRS to match the raster
trial.plan.v <- terra::vect(trial.plan)
if (!identical(terra::crs(trial.plan.v, proj = TRUE), terra::crs(sen.dat, proj = TRUE))) {
  trial.plan.v <- terra::project(trial.plan.v, terra::crs(sen.dat, proj = TRUE))
}

# Dissolve strips by treatment (combines any replicate polygons per treatment)
# Note: tryCatch tries terra::dissolve() first — if that fails for any reason,
# it falls back to terra::aggregate() instead
polys_treat <- tryCatch(
  terra::dissolve(trial.plan.v, "treat_desc"),
  error = function(e) terra::aggregate(trial.plan.v, by = "treat_desc")
)

cat("Number of treatments found:", nrow(polys_treat), "\n")
print(polys_treat$treat_desc)

# Calculate days after planting for each image date
dap_sen <- as.numeric(img_dates - plant_date)
cat("DAP range:", min(dap_sen), "to", max(dap_sen), "\n")
