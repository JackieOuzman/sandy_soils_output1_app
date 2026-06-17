# =============================================================================
# SCRIPT 1: NDVI Time Series Data Processing — af-sandysoils-ii
# =============================================================================
# Purpose:
#   Reads per-image Sentinel NDVI TIF files for a selected trial site,
#   stacks them into a multi-layer raster, removes duplicate dates, sorts
#   chronologically, and calculates days after planting (DAP) using sowing
#   dates from the project metadata file. Extracts mean NDVI per treatment
#   strip and per soil zone (from separate shapefiles), then computes the
#   area under the NDVI curve (AUC) for each treatment x zone combination
#   using the trapezoidal rule. Also extracts treatment-level means directly
#   from the raster (ignoring zones) to avoid averaging-of-averages bias.
#   Saves two CSVs per site ready for Script 2 (plotting).
#
# Inputs:
#   - Per-image NDVI TIFs:  headDir/7.In_Season_data/YY/8.Sentinel_QGIS_Jackie/
#   - Trial plan shapefile: path from metadata sheet "file location etc",
#                           variable == "trial.plan"
#   - Zone shapefile:       path from metadata sheet "file location etc",
#                           variable == "location of zone shp"
#   - Sowing date:          metadata sheet "seasons", column "Sowing date"
#
# Outputs (both saved to Growth_curves_output/):
#   1. <site_name>_NDVI_treatment_zone_DAP.csv
#      Columns: site, date, DAP, treat, treat_desc, zone, mean_ndvi, AUC
#      Use for: plots split by treatment AND zone
#
#   2. <site_name>_NDVI_treatment_only_DAP.csv
#      Columns: site, date, DAP, treat, treat_desc, zone, mean_ndvi, AUC
#      zone column = "all" throughout
#      Use for: plots of treatments only (no zone split)
#      Note: mean_ndvi here is extracted directly from pixels within each
#      treatment strip — NOT an average of zone means — so it is unbiased.
#
# Nodata handling:
#   Sentinel-2 tiles downloaded via the QGIS PAT Sentinel plugin code
#   tile-edge pixels (outside the satellite swath) as 0 rather than NA.
#   These are masked to NA before the stack is clipped and saved, so they
#   render as transparent in the Shiny app and are excluded from all
#   zonal statistics. The fix is applied at line ~425 before writeRaster().
#
# Author:  Jackie Ouzman, CSIRO Agriculture & Food
# Project: af-sandysoils-ii
# Created: June 2025
# Modified: June 2026 — added zero-to-NA masking for Sentinel tile edges
# =============================================================================
# =============================================================================

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

# =============================================================================
# USER INPUT — change site number and year  only
# =============================================================================
year_of_analysis <- 2026

site_number_input <- 3  # 1 through 8

# =============================================================================
# SITE LOOKUP TABLE
# =============================================================================

site_lookup <- data.frame(
  id = 1:8,
  site_number = c(
    "1.Walpeup_MRS125",
    "2.Crystal_Brook_Brians_House",
    "3.Wynarka_Mervs_West",
    "4.Wharminda_Woodys",
    "5.Walpeup_Gums",
    "6.Crystal_Brook_Randals",
    "7.Wharminda_Bonanza",
    "8.Wynarka_Tanks"
  ),
  site_name = c(
    "Walpeup_MRS125",
    "Crystal_Brook_Brians_House",
    "Wynarka_Mervs_West",
    "Wharminda_Woodys",
    "Walpeup_Gums",
    "Crystal_Brook_Randals",
    "Wharminda_Bonanza",
    "Wynarka_Tanks"
  ),
  stringsAsFactors = FALSE
)

site_row    <- site_lookup[site_lookup$id == site_number_input, ]
site_number <- site_row$site_number
site_name   <- site_row$site_name
cat("Site selected:", site_number, "\n")

# =============================================================================
# PATHS AND DIRECTORIES
# =============================================================================


yr_short         <- substr(as.character(year_of_analysis), 3, 4)   # "25"

dir           <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
headDir       <- file.path(dir, "work", "Output-1", site_number)
metadata_path <- file.path(dir, "work", "Output-1", "0.Site-info")
metadata_file <- "names of treatments per site 2025 metadata and other info.xlsx"

ndvi_dir <- file.path(headDir,
                      "7.In_Season_data", yr_short,
                      "8.Sentinel_QGIS_Jackie")

saveDir <- file.path(ndvi_dir, "Growth_curves_output")

if (!dir.exists(saveDir)) {
  dir.create(saveDir, recursive = TRUE)
  message("Created output directory: ", saveDir)
}

# =============================================================================
# READ METADATA
# =============================================================================

meta <- readxl::read_excel(
  file.path(metadata_path, metadata_file),
  sheet = "file location etc"
) %>%
  filter(Site == site_number)

# Helper: pull a single value from metadata by variable name
meta_val <- function(var_name, col = "file path") {
  meta %>%
    filter(variable == var_name) %>%
    pull(col) %>%
    .[1]
}

# =============================================================================
# SUB-STEP 1A: FIND, DEDUPLICATE AND STACK NDVI TIFS
# =============================================================================

cat("\n--- SUB-STEP 1A: Loading NDVI TIF files ---\n")

ndvi_files <- list.files(
  path    = ndvi_dir,
  pattern = "NDVI.*10m\\.tif$",
  full.names = TRUE
)

if (length(ndvi_files) == 0) {
  stop("No NDVI TIF files found in: ", ndvi_dir)
}
cat("Found", length(ndvi_files), "NDVI TIF files\n")

fnames <- basename(ndvi_files)

# Try dash-separated date first (yyyy-mm-dd), then compact (yyyymmdd)
dates_parsed <- suppressWarnings(
  as.Date(str_extract(fnames, "\\d{4}-\\d{2}-\\d{2}"), format = "%Y-%m-%d")
)

missing_idx <- which(is.na(dates_parsed))
if (length(missing_idx) > 0) {
  compact <- str_extract(fnames[missing_idx], "(?<!\\d)\\d{8}(?!\\d)")
  dates_parsed[missing_idx] <- as.Date(compact, format = "%Y%m%d")
}

if (any(is.na(dates_parsed))) {
  warning("Could not parse dates from these files — they will be excluded:\n  ",
          paste(fnames[is.na(dates_parsed)], collapse = "\n  "))
  ndvi_files   <- ndvi_files[!is.na(dates_parsed)]
  dates_parsed <- dates_parsed[!is.na(dates_parsed)]
}

# Remove duplicate dates — keep first occurrence
dup_flag <- duplicated(dates_parsed)
if (any(dup_flag)) {
  cat("Removing", sum(dup_flag), "duplicate date(s):\n  ",
      paste(fnames[dup_flag], collapse = "\n  "), "\n")
  ndvi_files   <- ndvi_files[!dup_flag]
  dates_parsed <- dates_parsed[!dup_flag]
}

# Sort chronologically
ord        <- order(dates_parsed)
ndvi_files <- ndvi_files[ord]
img_dates  <- dates_parsed[ord]

cat("Dates after deduplication and sorting:\n  ",
    paste(format(img_dates), collapse = ", "), "\n")

# Stack into a single SpatRaster (one layer per date)
sen.dat        <- terra::rast(ndvi_files)
names(sen.dat) <- format(img_dates, "%Y-%m-%d")

cat("Raster stack:", nlyr(sen.dat), "layers,",
    nrow(sen.dat), "rows,", ncol(sen.dat), "cols\n")

# =============================================================================
# SUB-STEP 1B: SOWING DATE AND DAYS AFTER PLANTING
# =============================================================================

cat("\n--- SUB-STEP 1B: Sowing date and DAP ---\n")

seasons <- readxl::read_excel(
  file.path(metadata_path, metadata_file),
  sheet = "seasons"
) %>%
  filter(Site == site_number, Year == year_of_analysis)

if (nrow(seasons) == 0) {
  stop("No sowing date found for site '", site_number,
       "' and year ", year_of_analysis, " in the seasons sheet.")
}

# Sowing dates may arrive as numeric serial, Date object, or character string
sow_raw <- seasons$`Sowing date`[1]

plant_date <- if (inherits(sow_raw, "Date") || inherits(sow_raw, "POSIXct")) {
  as.Date(sow_raw)
} else if (is.numeric(sow_raw)) {
  as.Date(sow_raw, origin = "1899-12-30")
} else {
  sow_char <- trimws(as.character(sow_raw))
  parsed <- suppressWarnings(
    tryCatch({
      if (grepl("^\\d{5}$", sow_char)) {
        as.Date(as.numeric(sow_char), origin = "1899-12-30")
      } else {
        lubridate::parse_date_time(
          sow_char,
          orders = c("dmy", "ymd", "mdy", "d-m-Y", "d/m/Y", "Y-m-d"),
          quiet  = TRUE
        ) %>% as.Date()
      }
    }, error = function(e) NA_Date_)
  )
  if (is.na(parsed)) {
    stop("Could not parse sowing date: '", sow_char,
         "'\nPlease check the seasons sheet for site: ", site_number)
  }
  parsed
}

cat("Sowing date:", format(plant_date), "\n")
dap_vec <- as.numeric(img_dates - plant_date)
cat("DAP values:", paste(dap_vec, collapse = ", "), "\n")

# =============================================================================
# SUB-STEP 1C: READ TRIAL PLAN AND ZONE SHAPEFILES
# =============================================================================

cat("\n--- SUB-STEP 1C: Reading shapefiles ---\n")

# --- Trial plan ---
trial_shp_path <- paste0(headDir, meta_val("trial.plan"))
cat("Trial plan:", trial_shp_path, "\n")
if (!file.exists(trial_shp_path)) stop("Trial plan shapefile not found: ", trial_shp_path)
trial_plan <- sf::st_read(trial_shp_path, quiet = TRUE)

# --- Zone shapefile ---
zone_shp_path <- paste0(headDir, meta_val("location of zone shp"))
cat("Zone shapefile:", zone_shp_path, "\n")
if (!file.exists(zone_shp_path)) stop("Zone shapefile not found: ", zone_shp_path)
zone_shp <- sf::st_read(zone_shp_path, quiet = TRUE)

# --- Zone field name: site-specific lookup (metadata value is unreliable) ---
zone_field <- case_when(
  site_number == "1.Walpeup_MRS125"             ~ "gridcode",
  site_number == "2.Crystal_Brook_Brians_House"  ~ "cluster",
  site_number == "3.Wynarka_Mervs_West"          ~ "fcl_mdl",
  site_number == "4.Wharminda_Woodys"            ~ "fcl_mdl",
  site_number == "5.Walpeup_Gums"                ~ "cluster3",
  site_number == "6.Crystal_Brook_Randals"        ~ "cluster",
  site_number == "7.Wharminda_Bonanza"            ~ "DN",
  site_number == "8.Wynarka_Tanks"                ~ "zone",
  TRUE ~ NA_character_
)

cat("Zone field being used:", zone_field, "\n")
if (is.na(zone_field)) stop("No zone field defined for site: ", site_number)
if (!zone_field %in% names(zone_shp)) {
  stop("Zone field '", zone_field, "' not found in zone shapefile.\n",
       "Available fields: ", paste(names(zone_shp), collapse = ", "))
}

# =============================================================================
# SUB-STEP 1D: ALIGN CRS, INTERSECT TREATMENTS x ZONES
# =============================================================================

cat("\n--- SUB-STEP 1D: Intersecting treatment strips x zones ---\n")

trial_plan_reproj <- trial_plan %>%
  st_transform(crs = st_crs(terra::crs(sen.dat)))

zone_shp_reproj <- zone_shp %>%
  st_transform(crs = st_crs(terra::crs(sen.dat))) %>%
  rename(zone = all_of(zone_field))

treat_zone <- sf::st_intersection(
  trial_plan_reproj %>% select(treat, treat_desc),
  zone_shp_reproj   %>% select(zone)
) %>%
  filter(treat != "B")   # drop buffer strips

cat("Treatment x zone combinations after removing Buffer:", nrow(treat_zone), "\n")

treat_zone_v <- terra::vect(treat_zone)

# =============================================================================
# SUB-STEP 1E: EXTRACT MEAN NDVI PER TREATMENT x ZONE x DATE
# =============================================================================

cat("\n--- SUB-STEP 1E: Extracting mean NDVI (treatment x zone) ---\n")

extracted <- terra::extract(sen.dat, treat_zone_v, fun = mean, na.rm = TRUE)

poly_attrs <- as.data.frame(treat_zone_v)[, c("treat", "treat_desc", "zone")]

ndvi_long <- cbind(poly_attrs, extracted[, -1, drop = FALSE]) %>%
  pivot_longer(
    cols      = -c(treat, treat_desc, zone),
    names_to  = "date",
    values_to = "mean_ndvi"
  ) %>%
  mutate(
    date = as.Date(date),
    DAP  = as.numeric(date - plant_date),
    site = site_name,
    zone = as.character(zone)
  ) %>%
  arrange(treat, zone, date)

cat("Rows in treatment x zone data:", nrow(ndvi_long), "\n")

# =============================================================================
# SUB-STEP 1F: AUC PER TREATMENT x ZONE
# =============================================================================

cat("\n--- SUB-STEP 1F: Calculating AUC (treatment x zone) ---\n")

trap_auc <- function(dap, ndvi) {
  ok   <- !is.na(ndvi) & !is.na(dap)
  dap  <- dap[ok]; ndvi <- ndvi[ok]
  if (length(dap) < 2) return(NA_real_)
  ord  <- order(dap)
  dap  <- dap[ord]; ndvi <- ndvi[ord]
  sum(diff(dap) * (ndvi[-length(ndvi)] + ndvi[-1]) / 2)
}

auc_zone <- ndvi_long %>%
  group_by(site, treat, treat_desc, zone) %>%
  summarise(AUC = trap_auc(DAP, mean_ndvi), .groups = "drop")

ndvi_out <- ndvi_long %>%
  left_join(auc_zone, by = c("site", "treat", "treat_desc", "zone")) %>%
  select(site, date, DAP, treat, treat_desc, zone, mean_ndvi, AUC)

out_file <- file.path(saveDir, paste0(site_name, "_NDVI_treatment_zone_DAP.csv"))
write.csv(ndvi_out, file = out_file, row.names = FALSE)
cat("Saved:", out_file, "\n")

# =============================================================================
# SUB-STEP 1G: EXTRACT MEAN NDVI PER TREATMENT ONLY (NO ZONE SPLIT)
# =============================================================================
# Pixels are extracted fresh from the raster for each dissolved treatment
# polygon — this avoids any averaging-of-averages bias from the zone data.
# =============================================================================

cat("\n--- SUB-STEP 1G: Extracting mean NDVI (treatment only) ---\n")

# Dissolve trial plan to one polygon per treatment (merges replicate strips)
treat_only <- trial_plan_reproj %>%
  filter(treat != "B") %>%
  group_by(treat, treat_desc) %>%
  summarise(.groups = "drop")

treat_only_v <- terra::vect(treat_only)

extracted_treat <- terra::extract(sen.dat, treat_only_v, fun = mean, na.rm = TRUE)

ndvi_treat_long <- cbind(
  as.data.frame(treat_only_v)[, c("treat", "treat_desc")],
  extracted_treat[, -1, drop = FALSE]
) %>%
  pivot_longer(
    cols      = -c(treat, treat_desc),
    names_to  = "date",
    values_to = "mean_ndvi"
  ) %>%
  mutate(
    date = as.Date(date),
    DAP  = as.numeric(date - plant_date),
    site = site_name,
    zone = "all"          # sentinel value: no zone split
  ) %>%
  arrange(treat, date)

cat("Rows in treatment-only data:", nrow(ndvi_treat_long), "\n")

# AUC per treatment (no zone split)
auc_treat <- ndvi_treat_long %>%
  group_by(site, treat, treat_desc, zone) %>%
  summarise(AUC = trap_auc(DAP, mean_ndvi), .groups = "drop")

ndvi_treat_out <- ndvi_treat_long %>%
  left_join(auc_treat, by = c("site", "treat", "treat_desc", "zone")) %>%
  select(site, date, DAP, treat, treat_desc, zone, mean_ndvi, AUC)

out_file_treat <- file.path(saveDir,
                            paste0(site_name, "_NDVI_treatment_only_DAP.csv"))
write.csv(ndvi_treat_out, file = out_file_treat, row.names = FALSE)
cat("Saved:", out_file_treat, "\n")

cat("\n=== Script 1 complete ===\n")
cat("Outputs written to:", saveDir, "\n")



# =============================================================================
# SUB-STEP 1H (OPTIONAL): SAVE CLIPPED RASTER STACK FOR GIF/PNG RENDERING
# =============================================================================

boundary_path <- paste0(headDir, meta_val("boundary_shapefile"))

if (file.exists(boundary_path)) {
  boundary     <- sf::st_read(boundary_path, quiet = TRUE) %>%
    st_transform(crs = st_crs(terra::crs(sen.dat)))
  boundary_v   <- terra::vect(boundary)
  sen_clipped  <- terra::crop(sen.dat, boundary_v) %>%
    terra::mask(boundary_v)
  stack_file   <- file.path(saveDir, paste0(site_name, "_NDVI_stack.tif"))
  terra::writeRaster(sen_clipped, stack_file, overwrite = TRUE)
  cat("Clipped raster stack saved:", stack_file, "\n")
} else {
  cat("Boundary shapefile not found — skipping clipped stack save\n")
}

