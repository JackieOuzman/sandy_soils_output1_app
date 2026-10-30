# =============================================================================
# SCRIPT 4: Shiny App Data Packaging — af-sandysoils-ii
# =============================================================================
# Purpose:
#   Collects the analysis outputs produced by Scripts 1–2 from their deep
#   nested site folders on the project drive ({af-sandysoils-ii}) and copies
#   them into the flat folder the Shiny app reads
#   ({ss-output1-app-data}/work/Output-1/shiny_app_data, the U: drive).
#   There is no separate transfer step.
#
#   Also exports a single site_metadata.csv from the project Excel file,
#   replacing the sheets (treatment names, zone_details, seasons) that the
#   Shiny app would otherwise need direct access to. The "Comment" column of
#   the "seasons" sheet becomes season_note, and Script 4 adds
#   "Latest image: dd Mon yyyy" to it, read from the Script 1 CSVs.
#
# What this script does NOT do:
#   - Re-run any analysis (Scripts 1–2 must already be complete for all sites)
#   - Modify any files in the original folder structure
#   - Copy the Word/PDF documents: these are edited by hand in
#     shiny_app_data/docs (User Guide, Metadata, Copyright/Disclaimer, logos)
#
# Before running:
#   - Excel metadata saved and closed (treatment names, zone details, seasons
#     with sowing/harvest dates and Comment notes, and shapefile locations).
#   - Treatment names in each trial plan shapefile must match the metadata
#     exactly (no trailing spaces or new-lines), or the app map will show
#     strips without colours.
#
# Safety features:
#   - Backs up the current app data folder first (skips if today's backup
#     exists; set do_backup <- FALSE for quick re-runs).
#   - Each shapefile folder is cleared and only the file named in the metadata
#     is copied, so stale shapefiles cannot sit beside the new one. The script
#     checks there is exactly one .shp per folder and prints [CHECK] if not.
#   - site_metadata.csv only lists site x year combinations that are packaged
#     (set in site_lookup, e.g. Bonanza has 2026 only).
#
# Inputs (per site, from existing folder structure):
#   - <site_name>_NDVI_treatment_only_DAP.csv       (required)
#   - <site_name>_NDVI_treatment_zone_DAP.csv        (required)
#   - <site_name>_NDVI_stack.tif                     (optional)
#   - Plot PNGs from Script 2                        (optional)
#   - Shapefiles (paths read from metadata Excel, sheet "file location etc"):
#       boundary_shapefile
#       trial.plan
#       location of zone shp
#   - Metadata Excel: "names of treatments per site 2025 metadata..."
#     Sheets used: "treatment names", "zone_details", "seasons",
#                  "file location etc"
#
# Outputs (all written to shiny_app_data/):
#   shiny_app_data/
#   ├── site_metadata.csv
#   ├── docs/            (edited by hand, not touched by this script)
#   └── <site_name>/
#       ├── <year>/
#       │   ├── <site_name>_NDVI_treatment_only_DAP.csv
#       │   ├── <site_name>_NDVI_treatment_zone_DAP.csv
#       │   ├── <site_name>_NDVI_stack.tif              (if present)
#       │   ├── <site_name>_growth_curve_treatment.png
#       │   ├── <site_name>_growth_curve_zone.png
#       │   ├── <site_name>_cumulative_ndvi_treatment.png
#       │   ├── <site_name>_cumulative_ndvi_zone.png
#       │   ├── <site_name>_AUC_treatment.png
#       │   └── <site_name>_AUC_zone.png
#       └── shapefiles/
#           ├── boundary/    (.shp .dbf .prj .shx .cpg if present)
#           ├── trial_plan/
#           └── zones/
#
# Usage:
#   Run top to bottom in RStudio (the backup runs first and takes a few
#   minutes). Each section prints a progress summary; the final summary should
#   show 0 required items missing and no [CHECK] lines.
#   Re-run Script 4 any time Scripts 1–2 or the metadata have been updated.
#   If only the metadata changed, running Chunk 3 alone is enough.
#
# After running:
#   The app reads site_metadata.csv ONCE, when the app starts. Plots, CSVs and
#   shapefiles update straight away, but season notes, treatment names and
#   zone labels only change after the app restarts (or has been idle long
#   enough to restart). Ask IT for a restart if it still shows old metadata.
#
# Author:  Jackie Ouzman, CSIRO Agriculture & Food
# Project: af-sandysoils-ii
# Created: June 2026
# Modified: June 2026 — added growth_curve_by_treatment.png and
#                        cumulative_ndvi_by_treatment.png (faceted by
#                        treatment with Control as reference line)
# Modified: Oct 2026 — by_treatment PNGs removed; backup added; shapefile
#                        folders cleared before copying and checked for one
#                        .shp each; site_metadata drops site x year combinations
#                        not packaged (e.g. Bonanza 2025) and adds
#                        "Latest image: dd Mon yyyy" to season_note; data now
#                        written directly to the app folder on the U: drive
# =============================================================================
# =============================================================================
#  BACKUP
# =============================================================================
# START: back up the current app data before anything is overwritten
do_backup <- TRUE    # set to FALSE for quick re-runs once today's backup exists

# <<< NEW: define the paths here so this block runs on its own, before Chunk 1
proj_dir_app_data <- "//fs1-cbr.nexus.csiro.au/{ss-output1-app-data}"
output_root <- file.path(proj_dir_app_data, "work", "Output-1", "shiny_app_data")

if (do_backup) {
  bak_root <- file.path(proj_dir_app_data, "work", "Output-1",
                        paste0("shiny_app_data_BACKUP_", format(Sys.Date(), "%Y-%m-%d")))
  
  if (!dir.exists(output_root)) {
    cat("No existing shiny_app_data to back up.\n")
  } else if (dir.exists(bak_root)) {
    cat("Backup for today already exists, skipping:\n ", bak_root, "\n")
  } else {
    cat("Backing up current app data (this can take a few minutes)...\n")
    dir.create(bak_root)
    file.copy(output_root, bak_root, recursive = TRUE)
    n_src <- length(list.files(output_root, recursive = TRUE))
    n_bak <- length(list.files(file.path(bak_root, basename(output_root)), recursive = TRUE))
    cat("Files in app data:", n_src, "| files in backup:", n_bak, "\n")
    if (n_src != n_bak) stop("Backup file counts do not match, stopping before anything is overwritten.")
  }
}

# =============================================================================
# CHUNK 1: Libraries and user settings and back up
# =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(tidyr)
  library(lubridate)
})

# --- Root paths (same as Scripts 1 and 2) ---
proj_dir      <-     "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
metadata_path <- file.path(proj_dir, "work", "Output-1", "0.Site-info")
metadata_file <- "names of treatments per site 2025 metadata and other info.xlsx"

# --- Where to write the packaged output ---
proj_dir_app_data <- "//fs1-cbr.nexus.csiro.au/{ss-output1-app-data}" 
#output_root1 <- file.path(proj_dir,           "work", "Output-1", "shiny_app_data")
output_root <-  file.path(proj_dir_app_data,"work", "Output-1", "shiny_app_data")
cat("Output will be written to:\n ", output_root, "\n")






# =============================================================================
# CHUNK 2: Site lookup table with years and zone fields
# =============================================================================
# years column controls which season folders are packaged per site.
# zone_field records the attribute field name used in each site's zone shapefile.
# Add 2027 etc to years when the time comes.
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
  zone_field = c(
    "gridcode",   # 1. Walpeup_MRS125
    "cluster",    # 2. Crystal_Brook_Brians_House
    "fcl_mdl",    # 3. Wynarka_Mervs_West
    "fcl_mdl",    # 4. Wharminda_Woodys
    "cluster3",   # 5. Walpeup_Gums
    "cluster",    # 6. Crystal_Brook_Randals
    #"DN",         # 7. Wharminda_Bonanza
    "cluster",    # 7. Wharminda_Bonanza   # <<< CHANGED from "DN"
    "zone"        # 8. Wynarka_Tanks
  ),
  years = I(list(
    c(2025, 2026),   # 1. Walpeup_MRS125
    c(2025, 2026),   # 2. Crystal_Brook_Brians_House
    c(2025, 2026),   # 3. Wynarka_Mervs_West
    c(2025, 2026),   # 4. Wharminda_Woodys
    c(2025, 2026),   # 5. Walpeup_Gums
    c(2025, 2026),   # 6. Crystal_Brook_Randals
    c(2026),         # 7. Wharminda_Bonanza
    c(2025, 2026)    # 8. Wynarka_Tanks   # <<< CHANGED: added 2025
  )),
  stringsAsFactors = FALSE
)

cat("Sites and years to package:\n")
for (i in seq_len(nrow(site_lookup))) {
  cat(" ", site_lookup$site_name[i], "—",
      paste(site_lookup$years[[i]], collapse = ", "),
      "| zone field:", site_lookup$zone_field[i], "\n")
}


# =============================================================================
# CHUNK 3: Export site_metadata.csv from the Excel file
# =============================================================================
# Reads four sources and joins them into one flat file the Shiny app can use
# instead of needing direct access to the Excel on the network drive.
# zone_field is carried through from site_lookup so the app knows which
# shapefile attribute to use when rendering the zone basemap.
# =============================================================================

# =============================================================================
# CHUNK 3: Export site_metadata.csv from the Excel file
# =============================================================================

cat("\n--- Reading metadata Excel ---\n")

# Sheet 1: treatment names and colours
treat_meta <- readxl::read_excel(
  file.path(metadata_path, metadata_file),
  sheet = "treatment names"
) %>%
  dplyr::select(Site, treat, treat_desc = `Treatment Name`, hex = Hex) %>%
  distinct()

# Drop auxiliary/reference sites (e.g. "98.Auxillary_Sites/...") that aren't
# part of the 8 Output 1 trial sites
treat_meta <- treat_meta %>% filter(Site %in% site_lookup$site_number)

cat("Treatment metadata rows:", nrow(treat_meta), "\n")

# Sheet 2: zone details
zone_meta <- readxl::read_excel(
  file.path(metadata_path, metadata_file),
  sheet = "zone_details"
) %>%
  dplyr::select(
    Site,
    zone       = `zone names`,
    zone_label = `zone label names`,
    zone_hex   = `Hex Code`
  ) %>%
  mutate(zone = as.character(zone)) %>%
  distinct()
zone_meta  <- zone_meta  %>% filter(Site %in% site_lookup$site_number)

cat("Zone metadata rows:", nrow(zone_meta), "\n")

# Sheet 3: sowing dates — now includes crop, variety, harvest date and comment
season_meta <- readxl::read_excel(
  file.path(metadata_path, metadata_file),
  sheet = "seasons"
) %>%
  filter(Year %in% unlist(site_lookup$years)) %>%
  dplyr::select(Site, Year,
                crop         = Crop,
                variety      = Variety,
                sowing_date  = `Sowing date`,
                harvest_date = `Harvest date`,
                season_note  = Comment) %>%
  mutate(
    sowing_date = case_when(
      inherits(sowing_date, "Date")    ~ as.Date(sowing_date),
      inherits(sowing_date, "POSIXct") ~ as.Date(sowing_date),
      is.numeric(sowing_date)          ~ as.Date(sowing_date, origin = "1899-12-30"),
      TRUE ~ as.Date(lubridate::parse_date_time(
        trimws(as.character(sowing_date)),
        orders = c("dmy", "ymd", "mdy"),
        quiet  = TRUE
      ))
    ),
    harvest_date = case_when(
      inherits(harvest_date, "Date")    ~ as.Date(harvest_date),
      inherits(harvest_date, "POSIXct") ~ as.Date(harvest_date),
      is.numeric(harvest_date)          ~ as.Date(harvest_date, origin = "1899-12-30"),
      TRUE ~ as.Date(lubridate::parse_date_time(
        trimws(as.character(harvest_date)),
        orders = c("dmy", "ymd", "mdy"),
        quiet  = TRUE
      ))
    )
  )

cat("Season metadata rows:", nrow(season_meta), "\n")

# Join all together including zone_field and season_note
site_metadata <- treat_meta %>%
  left_join(zone_meta,   by = "Site") %>%
  left_join(season_meta, by = "Site") %>%
  left_join(
    site_lookup %>% dplyr::select(site_number, site_name, zone_field),
    by = c("Site" = "site_number")
  ) %>%
  dplyr::select(site_name, Site, Year, crop, variety,
                treat, treat_desc, hex,
                zone, zone_label, zone_hex, zone_field,
                sowing_date, harvest_date, season_note) %>%
  arrange(site_name, Year, treat, zone)

# <<< NEW: keep only site x year combinations that are actually packaged
# (drops e.g. Bonanza 2025, which has no trial and would show an empty year in the app)
valid_site_years <- dplyr::bind_rows(lapply(seq_len(nrow(site_lookup)), function(i)
  data.frame(Site = site_lookup$site_number[i], Year = site_lookup$years[[i]])))
site_metadata <- site_metadata %>% dplyr::semi_join(valid_site_years, by = c("Site", "Year"))

# <<< NEW: add "Latest image: dd Mon yyyy" to the season note (read from the Script 1 CSVs)
latest_img <- dplyr::bind_rows(lapply(seq_len(nrow(site_lookup)), function(i) {
  dplyr::bind_rows(lapply(site_lookup$years[[i]], function(yr) {
    f <- file.path(proj_dir, "work", "Output-1", site_lookup$site_number[i],
                   "7.In_Season_data", substr(as.character(yr), 3, 4),
                   "8.Sentinel_QGIS_Jackie", "Growth_curves_output",
                   paste0(site_lookup$site_name[i], "_NDVI_treatment_only_DAP.csv"))
    if (!file.exists(f)) return(NULL)
    data.frame(Site = site_lookup$site_number[i], Year = yr,
               last_img = max(as.Date(read.csv(f)$date)))
  }))
}))

site_metadata <- site_metadata %>%
  dplyr::left_join(latest_img, by = c("Site", "Year")) %>%
  dplyr::mutate(
    season_note = as.character(season_note),
    img_text    = ifelse(is.na(last_img), NA_character_,
                         paste0("Latest image: ", format(last_img, "%d %b %Y"))),
    season_note = dplyr::case_when(
      is.na(img_text)                        ~ season_note,
      is.na(season_note) | season_note == "" ~ img_text,
      TRUE                                   ~ paste(season_note, img_text, sep = " | ")
    )
  ) %>%
  dplyr::select(-last_img, -img_text)

cat("Combined metadata rows:", nrow(site_metadata), "\n")
cat("Sites represented:", n_distinct(site_metadata$site_name), "\n")

# Sense check
cat("\nSowing dates, crop and variety by site and year:\n")
site_metadata %>%
  distinct(site_name, Year, crop, variety, sowing_date,
           harvest_date, season_note) %>%
  arrange(site_name, Year) %>%
  print(n = Inf)

# Write to staging folder
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

metadata_out <- file.path(output_root, "site_metadata.csv")
write.csv(site_metadata, metadata_out, row.names = FALSE)
cat("\nSaved:", metadata_out, "\n")



# =============================================================================
# CHUNK 4: Read shapefile paths from metadata Excel
# =============================================================================
# The "file location etc" sheet stores relative paths to each site's
# shapefiles, the same way Script 1 reads them. We pull them here so
# Chunk 5 knows what to copy.
# =============================================================================

cat("\n--- Reading shapefile paths from metadata Excel ---\n")

file_locs <- readxl::read_excel(
  file.path(metadata_path, metadata_file),
  sheet = "file location etc"
)

cat("Variables found in 'file location etc' sheet:\n")
print(unique(file_locs$variable))

# Helper: pull a path value for a given site and variable name
get_shp_path <- function(site_num, var_name) {
  file_locs %>%
    dplyr::filter(Site == site_num, variable == var_name) %>%
    dplyr::pull(`file path`) %>%
    .[1]
}

# Quick sanity check — print what we find for site 1
cat("\nSample paths for site 1 (", site_lookup$site_number[1], "):\n")
cat("  boundary_shapefile  :", get_shp_path(site_lookup$site_number[1], "boundary_shapefile"), "\n")
cat("  trial.plan          :", get_shp_path(site_lookup$site_number[1], "trial.plan"), "\n")
cat("  location of zone shp:", get_shp_path(site_lookup$site_number[1], "location of zone shp"), "\n")


## =============================================================================
# CHUNK 5: Copy CSV, TIF, PNG and shapefile files for all sites
# =============================================================================
# For each site loops over its years (from site_lookup) and copies:
#   - CSVs, TIF and pre-rendered PNGs into shiny_app_data/<site_name>/<year>/
# Then copies shapefiles once per site (they don't change between seasons) into
#   - shiny_app_data/<site_name>/shapefiles/boundary|trial_plan|zones/
# =============================================================================

# Helper: copy all sidecar files for a shapefile (.shp .dbf .prj .shx .cpg)
copy_shapefile <- function(src_shp_path, dest_folder) {
  
  if (is.na(src_shp_path) || src_shp_path == "") {
    return(list(status = "MISSING - no path in metadata", files_copied = 0))
  }
  
  base       <- tools::file_path_sans_ext(src_shp_path)
  extensions <- c(".shp", ".dbf", ".prj", ".shx", ".cpg")
  
  dir.create(dest_folder, recursive = TRUE, showWarnings = FALSE)
  
  # <<< NEW: don't clear the destination unless the source .shp really exists
  if (!file.exists(paste0(base, ".shp"))) {
    return(list(status = "MISSING - source .shp not found", files_copied = 0))
  }
  
  # <<< NEW: remove any old files so stale shapefiles can't sit beside the new one
  old_files <- list.files(dest_folder, full.names = TRUE)
  if (length(old_files) > 0) file.remove(old_files)
  
  n_copied  <- 0
  n_missing <- 0
  
  for (ext in extensions) {
    src_file <- paste0(base, ext)
    if (file.exists(src_file)) {
      file.copy(src_file,
                file.path(dest_folder, basename(src_file)),
                overwrite = TRUE)
      n_copied <- n_copied + 1
    } else if (ext %in% c(".shp", ".dbf", ".shx")) {
      n_missing <- n_missing + 1
      cat("      [MISSING]", basename(src_file), "\n")
    }
  }
  
  # <<< NEW: check exactly one .shp ended up in the app folder
  n_shp <- length(list.files(dest_folder, pattern = "\\.shp$", ignore.case = TRUE))
  if (n_shp != 1) cat("      [CHECK] expected 1 .shp in", dest_folder, "but found", n_shp, "\n")
  
  if (n_missing > 0) {
    list(status = "MISSING - incomplete shapefile", files_copied = n_copied)
  } else {
    list(status = "copied", files_copied = n_copied)
  }
}

copy_log <- list()

for (i in seq_len(nrow(site_lookup))) {
  
  sn    <- site_lookup$site_number[i]
  snm   <- site_lookup$site_name[i]
  years <- site_lookup$years[[i]]
  
  headDir      <- file.path(proj_dir, "work", "Output-1", sn)
  dest_dir     <- file.path(output_root, snm)
  dest_shp_dir <- file.path(dest_dir, "shapefiles")
  
  dir.create(dest_dir,     recursive = TRUE, showWarnings = FALSE)
  dir.create(dest_shp_dir, recursive = TRUE, showWarnings = FALSE)
  
  cat("\n[Site", i, "]", snm, "\n")
  
  # --- CSVs, TIF and PNGs: loop over years ---
  for (yr in years) {
    
    yr_short <- substr(as.character(yr), 3, 4)
    
    src_dir <- file.path(
      headDir,
      "7.In_Season_data", yr_short,
      "8.Sentinel_QGIS_Jackie", "Growth_curves_output"
    )
    
    dest_yr_dir <- file.path(dest_dir, as.character(yr))
    dir.create(dest_yr_dir, recursive = TRUE, showWarnings = FALSE)
    
    cat("  Year:", yr, "\n")
    
    csv_tif_files <- list(
      
      # --- CSVs (required) ---
      list(name = paste0(snm, "_NDVI_treatment_only_DAP.csv"), required = TRUE),
      list(name = paste0(snm, "_NDVI_treatment_zone_DAP.csv"), required = TRUE),
      
      # --- TIF (optional) ---
      list(name = paste0(snm, "_NDVI_stack.tif"),              required = FALSE),
      
      # --- Pre-rendered PNGs (optional) ---
      list(name = paste0(snm, "_growth_curve_treatment.png"),         required = FALSE),
      list(name = paste0(snm, "_growth_curve_zone.png"),              required = FALSE),
      #list(name = paste0(snm, "_growth_curve_by_treatment.png"),      required = FALSE),  # remove
      list(name = paste0(snm, "_cumulative_ndvi_treatment.png"),      required = FALSE),
      list(name = paste0(snm, "_cumulative_ndvi_zone.png"),           required = FALSE),
      #list(name = paste0(snm, "_cumulative_ndvi_by_treatment.png"),   required = FALSE),  # remove
      list(name = paste0(snm, "_AUC_treatment.png"),                  required = FALSE),
      list(name = paste0(snm, "_AUC_zone.png"),                       required = FALSE)
    )
    
    for (f in csv_tif_files) {
      src_file <- file.path(src_dir, f$name)
      dst_file <- file.path(dest_yr_dir, f$name)
      
      if (file.exists(src_file)) {
        file.copy(src_file, dst_file, overwrite = TRUE)
        cat("    [OK]     ", f$name, "\n")
        copy_log <- append(copy_log, list(
          data.frame(site = snm, year = yr, type = "data",
                     file = f$name, status = "copied")))
      } else if (f$required) {
        cat("    [MISSING]", f$name, "<-- required!\n")
        copy_log <- append(copy_log, list(
          data.frame(site = snm, year = yr, type = "data",
                     file = f$name, status = "MISSING - required")))
      } else {
        cat("    [skip]   ", f$name, "(optional — not present)\n")
        copy_log <- append(copy_log, list(
          data.frame(site = snm, year = yr, type = "data",
                     file = f$name, status = "not found (optional)")))
      }
    }
  }  # end year loop
  
  # --- Shapefiles: copied once per site ---
  cat("  Shapefiles:\n")
  
  shapefiles_to_copy <- list(
    list(var = "boundary_shapefile",   label = "boundary",   subfolder = "boundary",   required = TRUE),
    list(var = "trial.plan",           label = "trial plan", subfolder = "trial_plan", required = TRUE),
    list(var = "location of zone shp", label = "zones",      subfolder = "zones",      required = TRUE)
  )
  
  for (s in shapefiles_to_copy) {
    rel_path  <- get_shp_path(sn, s$var)
    full_path <- paste0(headDir, rel_path)
    dest_sub  <- file.path(dest_shp_dir, s$subfolder)
    
    cat("    Copying", s$label, "...\n")
    result <- copy_shapefile(full_path, dest_sub)
    cat("      Status:", result$status,
        "| Files copied:", result$files_copied, "\n")
    
    copy_log <- append(copy_log, list(
      data.frame(site   = snm,
                 year   = NA_integer_,
                 type   = paste0("shapefile/", s$subfolder),
                 file   = basename(tools::file_path_sans_ext(full_path)),
                 status = result$status)))
  }
  
}  # end site loop




# =============================================================================
# CHUNK 6: Summary checklist
# =============================================================================
# Prints a final pass/fail table across all sites and years so you can see
# at a glance whether anything needs attention before transferring.
# =============================================================================

cat("\n=== PACKAGING SUMMARY ===\n\n")

all_log <- dplyr::bind_rows(copy_log)

required_log <- all_log %>%
  filter(!grepl("optional", status))

n_ok      <- sum(required_log$status == "copied")
n_missing <- sum(grepl("MISSING", required_log$status))

print(dplyr::as_tibble(required_log %>% dplyr::select(site, year, type, file, status)), n = Inf)   # <<< CHANGED: tibble so n = Inf works

cat("\nRequired items copied: ", n_ok, "\n")
cat("Required items missing:", n_missing, "\n")

if (n_missing == 0) {
  cat("\nAll required files present. shiny_app_data/ is ready to transfer.\n")
} else {
  cat("\nWARNING: Missing files above — re-run Script 1 for affected sites\n",
      "or check shapefile paths in the metadata Excel before transferring.\n")
}

cat("\nFolder size summary:\n")
for (snm in site_lookup$site_name) {
  site_dir <- file.path(output_root, snm)
  if (dir.exists(site_dir)) {
    n_files <- length(list.files(site_dir, recursive = TRUE))
    cat(" ", snm, "—", n_files, "files\n")
  }
}

cat("\nTransfer command (edit server path to suit):\n")
cat("  rsync -av", output_root,
    "<your-username>@shiny.csiro.au:/srv/shiny-server/sandysoils/data/\n")

cat("\n=== Script 4 complete ===\n")
