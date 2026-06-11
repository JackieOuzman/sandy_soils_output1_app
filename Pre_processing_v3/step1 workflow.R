# =============================================================================
# SCRIPT 1: NDVI Time Series Data Processing — af-sandysoils-ii
# =============================================================================
# Purpose:
#   Reads per-image Sentinel NDVI  files for a selected trial site,
#   stacks them into a multi-layer raster, removes duplicate dates, sorts
#   chronologically, and calculates days after planting (DAP) using sowing
#   dates from the project metadata file. Extracts mean NDVI per treatment
#   strip and per soil zone (from separate shapefiles), then computes the
#   area under the NDVI curve (AUC) for each treatment × zone combination
#   using the trapezoidal rule. Saves one CSV per site ready for Script 2
#   (plotting).
#
# Inputs:
#   - Per-image NDVI TIFs:  headDir/7.In_Season_data/YY/8.Sentinel_QGIS_Jackie/
#   - Trial plan shapefile: path from metadata sheet "file location etc",
#                           variable == "trial.plan"
#   - Zone shapefile:       path from metadata sheet "file location etc",
#                           variable == "location of zone shp"
#   - Sowing date:          metadata sheet "seasons", column "Sowing date"
#   - Zone field name:      metadata sheet "file location etc",
#                           variable == "zone names clm heading name"
#
# Outputs:
#   - CSV: headDir/7.In_Season_data/YY/8.Sentinel_QGIS_Jackie/
#          Growth_curves_output/<site_name>_NDVI_treatment_zone_DAP.csv
#     Columns: site, date, DAP, treat, treat_desc, zone, mean_ndvi, AUC
#
# Author:  Jackie Ouzman, CSIRO Agriculture & Food
# Project: af-sandysoils-ii
# Created: June 2025
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
# USER INPUT — change site number only
# =============================================================================

site_number_input <- 1   # 1 through 6

# =============================================================================
# SITE LOOKUP TABLE
# =============================================================================

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

# =============================================================================
# PATHS AND DIRECTORIES
# =============================================================================

year_of_analysis <- 2025
yr_short         <- substr(as.character(year_of_analysis), 3, 4)   # "25"

dir           <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
headDir       <- file.path(dir, "work", "Output-1", site_number)
metadata_path <- file.path(dir, "work", "Output-1", "0.Site-info")
metadata_file <- "names of treatments per site 2025 metadata and other info.xlsx"

ndvi_dir <- file.path(headDir,
                      "7.In_Season_data", yr_short,
                      "8.Sentinel_QGIS_Jackie")

saveDir  <- file.path(ndvi_dir, "Growth_curves_output")

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

# List files matching *NDVI*10m.tif
ndvi_files <- list.files(
  path       = ndvi_dir,
  pattern    = "NDVI.*10m\\.tif$",
  full.names = TRUE
)

if (length(ndvi_files) == 0) {
  stop("No NDVI TIF files found in: ", ndvi_dir)
}
cat("Found", length(ndvi_files), "NDVI TIF files\n")

# Parse dates from filenames — handles both yyyy-mm-dd and yyyymmdd
fnames <- basename(ndvi_files)

# Try dash-separated first (yyyy-mm-dd), then compact (yyyymmdd)
dates_parsed <- suppressWarnings(
  as.Date(
    str_extract(fnames, "\\d{4}-\\d{2}-\\d{2}"),
    format = "%Y-%m-%d"
  )
)

# Where dash format failed, try compact 8-digit
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
dup_flag  <- duplicated(dates_parsed)
dup_flag
if (any(dup_flag)) {
  cat("Removing", sum(dup_flag), "duplicate date(s):\n  ",
      paste(fnames[dup_flag], collapse = "\n  "), "\n")
  ndvi_files   <- ndvi_files[!dup_flag]
  dates_parsed <- dates_parsed[!dup_flag]
}

# Sort chronologically
ord          <- order(dates_parsed)
ndvi_files   <- ndvi_files[ord]
img_dates    <- dates_parsed[ord]

cat("Dates after deduplication and sorting:\n  ",
    paste(format(img_dates), collapse = ", "), "\n")

# Stack into a single SpatRaster (one layer per date)
sen.dat <- terra::rast(ndvi_files)
names(sen.dat) <- format(img_dates, "%Y-%m-%d")

cat("Raster stack: ", nlyr(sen.dat), "layers,",
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
# Handle all three cases
sow_raw <- seasons$`Sowing date`[1]

plant_date <- if (inherits(sow_raw, "Date") || inherits(sow_raw, "POSIXct")) {
  as.Date(sow_raw)
} else if (is.numeric(sow_raw)) {
  as.Date(sow_raw, origin = "1899-12-30")
} else {
  # Character — try multiple formats
  sow_char <- trimws(as.character(sow_raw))
  parsed <- suppressWarnings(
    tryCatch({
      # Try numeric serial stored as string first (e.g. "45774")
      if (grepl("^\\d{5}$", sow_char)) {
        as.Date(as.numeric(sow_char), origin = "1899-12-30")
      } else {
        # Try common date string formats
        lubridate::parse_date_time(sow_char,
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

if (!file.exists(trial_shp_path)) {
  stop("Trial plan shapefile not found: ", trial_shp_path)
}
trial_plan <- sf::st_read(trial_shp_path, quiet = TRUE)

# --- Zone shapefile ---
zone_shp_path <- paste0(headDir, meta_val("location of zone shp"))
cat("Zone shapefile:", zone_shp_path, "\n")

if (!file.exists(zone_shp_path)) {
  stop("Zone shapefile not found: ", zone_shp_path)
}
zone_shp <- sf::st_read(zone_shp_path, quiet = TRUE)

# --- Zone field name: metadata value is unreliable, use site-specific lookup ---
zone_field <- case_when(
  site_number == "1.Walpeup_MRS125"            ~ "gridcode",
  site_number == "2.Crystal_Brook_Brians_House" ~ "cluster",
  site_number == "3.Wynarka_Mervs_West"         ~ "fcl_mdl",
  site_number == "4.Wharminda_Woodys"           ~ "fcl_mdl",
  site_number == "5.Walpeup_Gums"               ~ "cluster3",
  site_number == "6.Crystal_Brook_Randals"       ~ "cluster",
  site_number == "7.Wharminda_Bonanza"           ~ "DN",
  site_number == "8.Wynarka_Tanks"               ~ "zone",
  TRUE ~ NA_character_
)

cat("Zone field being used:", zone_field, "\n")

if (is.na(zone_field)) {
  stop("No zone field defined for site: ", site_number)
}

if (!zone_field %in% names(zone_shp)) {
  stop("Zone field '", zone_field, "' not found in zone shapefile.\n",
       "Available fields: ", paste(names(zone_shp), collapse = ", "))
}

# =============================================================================
# SUB-STEP 1D: ALIGN CRS, INTERSECT TREATMENTS × ZONES
# =============================================================================

cat("\n--- SUB-STEP 1D: Intersecting treatment strips x zones ---\n")

# Reproject to raster CRS if needed (both already 7854 so this is a safety check)
trial_plan_reproj <- trial_plan %>%
  st_transform(crs = st_crs(terra::crs(sen.dat)))

zone_shp_reproj <- zone_shp %>%
  st_transform(crs = st_crs(terra::crs(sen.dat)))

# Rename zone field to consistent internal name
zone_shp_reproj <- zone_shp_reproj %>%
  rename(zone = all_of(zone_field))

# Intersect: each resulting polygon has both treat and zone attributes
treat_zone <- sf::st_intersection(
  trial_plan_reproj %>% select(treat, treat_desc),
  zone_shp_reproj   %>% select(zone)
)




# Drop Buffer treatment — not part of the trial analysis
treat_zone <- treat_zone %>%
  filter(treat != "B")

cat("After removing Buffer:", nrow(treat_zone), "combinations\n")

# Convert to terra vect for extraction
treat_zone_v <- terra::vect(treat_zone)




# =============================================================================
# SUB-STEP 1E: EXTRACT MEAN NDVI PER TREATMENT x ZONE x DATE
# =============================================================================

cat("\n--- SUB-STEP 1E: Extracting mean NDVI ---\n")

# Extract mean NDVI for each treat x zone polygon across all image dates
extracted <- terra::extract(
  x     = sen.dat,
  y     = treat_zone_v,
  fun   = mean,
  na.rm = TRUE
)

cat("Dimensions of extracted data:", nrow(extracted), "rows,", ncol(extracted), "cols\n")

# Bind treat/zone attributes back onto extracted values (drop the ID column)
poly_attrs <- as.data.frame(treat_zone_v)[, c("treat", "treat_desc", "zone")]

ndvi_wide <- cbind(poly_attrs, extracted[, -1, drop = FALSE])

# Pivot to long format: one row per polygon x date
ndvi_long <- ndvi_wide %>%
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

cat("Rows in long-format data:", nrow(ndvi_long), "\n")
cat("Date range:", format(min(ndvi_long$date)), "to", format(max(ndvi_long$date)), "\n")
cat("DAP range: ", min(ndvi_long$DAP), "to", max(ndvi_long$DAP), "\n")
print(head(ndvi_long, 10))

# =============================================================================
# SUB-STEP 1F: AREA UNDER CURVE (AUC) — TRAPEZOIDAL RULE, PER TREAT x ZONE
# =============================================================================

cat("\n--- SUB-STEP 1F: Calculating AUC (trapezoidal rule) ---\n")

# Trapezoidal AUC: area between DAP points weighted by NDVI values
trap_auc <- function(dap, ndvi) {
  ok   <- !is.na(ndvi) & !is.na(dap)
  dap  <- dap[ok]
  ndvi <- ndvi[ok]
  if (length(dap) < 2) return(NA_real_)
  ord  <- order(dap)
  dap  <- dap[ord]
  ndvi <- ndvi[ord]
  sum(diff(dap) * (ndvi[-length(ndvi)] + ndvi[-1]) / 2)
}

auc_summary <- ndvi_long %>%
  group_by(site, treat, treat_desc, zone) %>%
  summarise(AUC = trap_auc(DAP, mean_ndvi), .groups = "drop")

cat("AUC calculated for", nrow(auc_summary), "treatment x zone combinations\n")
print(auc_summary)


# =============================================================================
# JOIN AUC BACK AND SAVE OUTPUT
# =============================================================================

# Join AUC back onto the long-format table
ndvi_out <- ndvi_long %>%
  left_join(auc_summary, by = c("site", "treat", "treat_desc", "zone")) %>%
  select(site, date, DAP, treat, treat_desc, zone, mean_ndvi, AUC)

cat("Final output dimensions:", nrow(ndvi_out), "rows,", ncol(ndvi_out), "cols\n")
cat("Columns:", paste(names(ndvi_out), collapse = ", "), "\n")

# Save
out_file <- file.path(saveDir,
                      paste0(site_name, "_NDVI_treatment_zone_DAP.csv"))

write.csv(ndvi_out, file = out_file, row.names = FALSE)
cat("\nOutput saved to:\n ", out_file, "\n")
cat("\n=== Script 1 complete ===\n")
