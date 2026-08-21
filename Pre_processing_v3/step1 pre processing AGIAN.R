# =============================================================================
# SCRIPT 1: NDVI Time Series Data Processing — af-sandysoils-ii
# =============================================================================
# Purpose:
#   Reads per-image Sentinel NDVI TIF files (from the NDVI subfolder) for a
#   selected trial site, checks their georeferencing against the trial plan
#   shapefile, stacks them into a multi-layer raster, removes duplicate
#   dates, sorts chronologically, and calculates days after planting (DAP)
#   using sowing dates from the project metadata file. Extracts mean NDVI
#   per treatment strip and per soil zone (from separate shapefiles), then
#   computes the area under the NDVI curve (AUC) for each treatment x zone
#   combination using the trapezoidal rule. Also extracts treatment-level
#   means directly from the raster (ignoring zones) to avoid
#   averaging-of-averages bias. Saves two CSVs per site ready for Script 2
#   (plotting).
#
# Inputs:
#   - Per-image NDVI TIFs:  headDir/7.In_Season_data/YY/8.Sentinel_QGIS_Jackie/NDVI/
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
#   Some Sentinel-2 tile exports use -999 for nodata (tile-edge pixels
#   outside the satellite swath). These are masked to NA in Sub-step 1A-ii,
#   right after the stack is built and before any extraction, so they don't
#   drag down the zonal means calculated in Sub-steps 1E and 1G. NOTE: for
#   sites where nodata is already stored as true NA in the source TIFs
#   (confirmed for Crystal_Brook_Brians_House and Walpeup_Gums, Aug 2026),
#   this step finds 0 pixels to mask and is a no-op — that's expected, not
#   an error.
#
# Georeferencing check (added Aug 2026):
#   The pre-built "*_NDVI-Stack_10m.tif" file is NO LONGER USED as an input.
#   It was found to carry incorrect coordinates for at least one site
#   (Walpeup_Gums) despite a correct-looking CRS tag. Script now reads
#   individual per-date TIFs from the "NDVI" subfolder instead, and checks
#   each file's extent against the trial plan shapefile before stacking —
#   any file whose extent doesn't overlap the trial plan is excluded and
#   reported, since a subfolder may contain a mix of older mis-georeferenced
#   exports and newer correct ones from the same QGIS tool.
#
# Known outstanding issue (flagged, not yet fixed):
#   The Buffer-strip filter checks for treat != "Buffer", but the actual
#   value in the trial plan shapefile is "BUFF" — so buffer strips are NOT
#   currently being excluded from either output file (Sub-steps 1D and 1G).
#   Confirmed present in Crystal_Brook_Brians_House outputs, Aug 2026.
#   Fix (when ready): change "Buffer" to "BUFF" in both filter() calls below.
#
# Author:  Jackie Ouzman, CSIRO Agriculture & Food
# Project: af-sandysoils-ii
# Created: June 2025
# Modified: June 2026 — added zero-to-NA masking for Sentinel tile edges
# Modified: Aug 2026 — switched from pre-built stack to NDVI subfolder +
#           trial-plan overlap check, after mis-georeferenced stack found
#           for Walpeup_Gums
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

site_number_input <- 5 # 1 through 8

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

# --- Read trial plan early, so we can sanity-check NDVI file extents against it ---
trial_shp_path <- paste0(headDir, meta_val("trial.plan"))
cat("Trial plan:", trial_shp_path, "\n")
if (!file.exists(trial_shp_path)) stop("Trial plan shapefile not found: ", trial_shp_path)
trial_plan <- sf::st_read(trial_shp_path, quiet = TRUE)
trial_bbox <- st_bbox(trial_plan)

# =============================================================================
# SUB-STEP 1A: FIND AND LOAD NDVI DATA FROM THE NDVI SUBFOLDER
# =============================================================================
# The pre-built stack ("*_NDVI-Stack_10m.tif") is intentionally NOT used —
# see header note above. Individual per-date TIFs are read from the "NDVI"
# subfolder, and each file's extent is checked against the trial plan
# before stacking, since the folder can contain a mix of old
# mis-georeferenced exports and newer correct ones.
# =============================================================================

cat("\n--- SUB-STEP 1A: Locating NDVI data ---\n")

ndvi_subfolder <- file.path(ndvi_dir, "NDVI")

if (!dir.exists(ndvi_subfolder)) {
  stop("NDVI subfolder not found: ", ndvi_subfolder)
}

ndvi_files <- list.files(
  path       = ndvi_subfolder,
  pattern    = "NDVI.*10m\\.tif$",
  full.names = TRUE
)

if (length(ndvi_files) == 0) {
  stop("No NDVI TIF files found in: ", ndvi_subfolder)
}

cat("Found", length(ndvi_files), "files in NDVI subfolder\n")

fnames <- basename(ndvi_files)
img_dates <- as.Date(str_extract(fnames, "\\d{4}-\\d{2}-\\d{2}"), format = "%Y-%m-%d")

if (any(is.na(img_dates))) {
  stop("Could not parse dates from these filenames: ",
       paste(fnames[is.na(img_dates)], collapse = ", "))
}

# --- Sanity check: keep only files whose extent overlaps the trial plan ---
# (Guards against mis-georeferenced files sitting alongside correct ones —
#  checked against the trial plan's true location, not by majority count,
#  since a folder can have MORE old broken files than new correct ones.)
file_overlaps_trial <- sapply(ndvi_files, function(f) {
  e <- terra::ext(terra::rast(f))
  !(e[2] < trial_bbox["xmin"] || e[1] > trial_bbox["xmax"] ||
      e[4] < trial_bbox["ymin"] || e[3] > trial_bbox["ymax"])
})

cat("\nFiles overlapping trial plan location:", sum(file_overlaps_trial),
    "of", length(ndvi_files), "\n")

if (any(!file_overlaps_trial)) {
  cat("WARNING — files excluded (extent does not overlap trial plan, likely mis-georeferenced):\n  ",
      paste(fnames[!file_overlaps_trial], collapse = "\n  "), "\n")
}

ndvi_files <- ndvi_files[file_overlaps_trial]
img_dates  <- img_dates[file_overlaps_trial]
fnames     <- fnames[file_overlaps_trial]

if (length(ndvi_files) == 0) {
  stop("No NDVI files overlap the trial plan location — check georeferencing.")
}

# Remove duplicate dates — keep first occurrence
dup_flag <- duplicated(img_dates)
if (any(dup_flag)) {
  cat("Removing", sum(dup_flag), "duplicate date(s):\n  ",
      paste(fnames[dup_flag], collapse = "\n  "), "\n")
  ndvi_files <- ndvi_files[!dup_flag]
  img_dates  <- img_dates[!dup_flag]
  fnames     <- fnames[!dup_flag]
}

# Sort chronologically
ord        <- order(img_dates)
ndvi_files <- ndvi_files[ord]
img_dates  <- img_dates[ord]

cat("\nDates after deduplication and sorting:\n  ",
    paste(format(img_dates), collapse = ", "), "\n")

# Stack into a single SpatRaster (one layer per date)
sen.dat        <- terra::rast(ndvi_files)
names(sen.dat) <- format(img_dates, "%Y-%m-%d")

cat("Raster stack:", nlyr(sen.dat), "layers,",
    nrow(sen.dat), "rows,", ncol(sen.dat), "cols\n")

cat("Raster extent (xmin, xmax, ymin, ymax):",
    paste(round(as.vector(terra::ext(sen.dat)), 1), collapse = ", "), "\n")

# =============================================================================
# SUB-STEP 1A-ii: MASK NODATA VALUES (-999) TO NA
# =============================================================================
# Some Sentinel-2 exports use -999 for nodata (tile-edge pixels outside the
# satellite swath). These are converted to NA here, before any extraction,
# so they don't drag down the zonal means calculated in Sub-steps 1E and 1G.
# NOTE: for sites where nodata is already true NA in the source files, this
# step will report 0 pixels masked — that's expected, not an error (the
# sum(is.na(...)) check afterward confirms real NA is still present).
# =============================================================================

cat("\n--- SUB-STEP 1A-ii: Masking nodata (-999) to NA ---\n")

nodata_counts <- sapply(1:nlyr(sen.dat), function(i) {
  sum(values(sen.dat[[i]]) == -999, na.rm = TRUE)
})
names(nodata_counts) <- names(sen.dat)

cat("Nodata (-999) pixel count per layer:\n")
print(nodata_counts)

sen.dat <- terra::app(sen.dat, fun = function(x) {
  x[x == -999] <- NA
  x
})
names(sen.dat) <- format(img_dates, "%Y-%m-%d")

cat("Nodata masking complete.\n")
cat("Total NA cells in stack:", sum(is.na(values(sen.dat))), "\n")

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
# SUB-STEP 1C: READ ZONE SHAPEFILE
# =============================================================================
# (Trial plan was already read earlier, before Sub-step 1A, so it could be
#  used for the extent sanity check on the NDVI files.)
# =============================================================================

cat("\n--- SUB-STEP 1C: Reading zone shapefile ---\n")

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
  site_number == "7.Wharminda_Bonanza"            ~ "cluster",#"DN"
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
# NOTE: filter(treat != "Buffer") below does not currently match anything —
# the real value is "BUFF" (see header note). Flagged, not yet fixed.
# =============================================================================

cat("\n--- SUB-STEP 1D: Intersecting treatment strips x zones ---\n")

trial_plan_reproj <- trial_plan %>%
  st_transform(crs = st_crs(terra::crs(sen.dat)))

zone_shp_reproj <- zone_shp %>%
  st_transform(crs = st_crs(terra::crs(sen.dat))) %>%
  rename(zone = all_of(zone_field))

treat_zone <- sf::st_intersection(
  trial_plan_reproj %>% dplyr::select(treat, treat_desc),
  zone_shp_reproj   %>% dplyr::select(zone)
) %>%
  filter(treat != "Buffer")   # drop buffer strips — NOTE: real value is "BUFF", see header

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
  dplyr::mutate(
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
  dplyr::select(site, date, DAP, treat, treat_desc, zone, mean_ndvi, AUC)

out_file <- file.path(saveDir, paste0(site_name, "_NDVI_treatment_zone_DAP.csv"))
write.csv(ndvi_out, file = out_file, row.names = FALSE)
cat("Saved:", out_file, "\n")

# =============================================================================
# SUB-STEP 1G: EXTRACT MEAN NDVI PER TREATMENT ONLY (NO ZONE SPLIT)
# =============================================================================
# Pixels are extracted fresh from the raster for each dissolved treatment
# polygon — this avoids any averaging-of-averages bias from the zone data.
# NOTE: filter(treat != "Buffer") below has the same "BUFF" mismatch as
# Sub-step 1D — see header note.
# =============================================================================

cat("\n--- SUB-STEP 1G: Extracting mean NDVI (treatment only) ---\n")

# Dissolve trial plan to one polygon per treatment (merges replicate strips)
treat_only <- trial_plan_reproj %>%
  filter(treat != "Buffer") %>%
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
  dplyr::mutate(
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
  dplyr::select(site, date, DAP, treat, treat_desc, zone, mean_ndvi, AUC)

out_file_treat <- file.path(saveDir,
                            paste0(site_name, "_NDVI_treatment_only_DAP.csv"))
write.csv(ndvi_treat_out, file = out_file_treat, row.names = FALSE)
cat("Saved:", out_file_treat, "\n")

cat("\n=== Script 1 complete ===\n")
cat("Outputs written to:", saveDir, "\n")



# =============================================================================
# SUB-STEP 1H : SAVE CLIPPED RASTER STACK FOR GIF/PNG RENDERING
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