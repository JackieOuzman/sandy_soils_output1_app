rm(list=ls())
# Pre-process data for the Site Viewer (all layers in EPSG:4326)
suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(lubridate)
  library(ggtext)
  library(scales)
  library(ggplot2)
  library(fs)
})

# ====================== Sites ======================
# site <- "1.Walpeup_MRS125"
# site_number <- "1.Walpeup_MRS125"
# site_name <- "Walpeup_MRS125"

# site <-"2.Crystal_Brook_Brians_House"
# site_number <-"2.Crystal_Brook_Brians_House"
# site_name <-  "Crystal_Brook_Brians_House"

# site_number <- "3.Wynarka_Mervs_West"
# site_name <- "Wynarka_Mervs_West"

# site_number <- "4.Wharminda"
# site_name <- "Wharminda"

# site <- "5.Walpeup_Gums"
# site_number <- "5.Walpeup_Gums"
# site_name <- "Walpeup_Gums"

site <- "6.Crystal_Brook_Randals"
site_number <- "6.Crystal_Brook_Randals"
site_name <- "Crystal_Brook_Randals"

# ====================== Year ======================
year_of_analysis <- 2025
# ====================== PATHS ======================

dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
headDir <- paste0(dir, "/work/Output-1/", site_number)

metadata_path <- paste0(dir,"/work/Output-1/0.Site-info/")
metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"

saveDir <- paste0(dir, "/work/Output-1/", site_number,"/7.In_Season_data/25/7.Growth_curves")
readDir <- paste0("//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}/work/Output-1/", site)

## JACKIE - Change the saved directory

#Dir <- headDir_Jaxs

#saveDir_year <- file.path(Dir, as.character(year_of_analysis))
#fs::dir_create(saveDir_year)

#Dir <- paste0("//fs1-cbr.nexus.csiro.au/{lw-soildatarepo}/work/Shiny/Apps/Stirling/GRDCSandySoilsII/Output1Viewer/Current/Files/",site_1)



# ====================== read in collated data from step1=====================
site.info <- readRDS(paste0(saveDir, "/site_info.rds"))

#========= each site has a different extension for sential images=============
extension_map <- c(
  "1.Walpeup_MRS125"              = "MRS125",
  "2.Crystal_Brook_Brians_House"  = "BHO",
  "3.Wynarka_Mervs_West"          = "MER",
  "4.Wharminda"                   = "WOD",
  "5.Walpeup_Gums"                = "GUM",
  "6.Crystal_Brook_Randals"       = "RAN"
)


extension <- extension_map[[site_number]]

#========= each site has a different name for the zones=============
grid_code_details <- c(
  "1.Walpeup_MRS125"              = "gridcode",
  "2.Crystal_Brook_Brians_House"  = "cluster",
  "3.Wynarka_Mervs_West"          = "xx",
  "4.Wharminda"                   = "xx",
  "5.Walpeup_Gums"                = "cluster3",
  "6.Crystal_Brook_Randals"       = "cluster"
)


grid_code <- grid_code_details[[site_number]]

# ====================== METADATA ======================

# file_path_details <- readxl::read_excel(
#   paste0(metadata_path,metadata_file_name),
#   sheet = "file location etc") %>% 
#   filter(Site == site)
# 
# site_extension <- file_path_details$`sential file name extension`



# ====================== Zones shape file and tif ======================  
zones <- site.info$zones #?not sure this is working
zones_sf <- site.info$zones_sf
zones_sf <- zones_sf %>% rename(zone = !!grid_code)

# ====================== list of bad dates for satellite ======================
bad_dates <- readxl::read_excel(
  paste0(file.path(readDir, "7.In_Season_data", 
                   "Sentinel_list_bad_dates.xlsx") ))
bad_dates$Dates <- as.character(bad_dates$Dates)

# Convert to a list where each column becomes a list element
bad_dates_list <- as.list(bad_dates)






################################################################################
####################### 3) Build Growth Curves  ################################
################################################################################

# ======================== 3.1 ) Define Functions========================

# Polygon (trial.plan) mean time-series
polygon_mean_timeseries <- function(rasters, polygons) {
  n_layers <- nlyr(rasters)
  out <- matrix(NA_real_, nrow = n_layers, ncol = nrow(polygons))
  for (k in seq_len(n_layers)) {
    vals <- terra::extract(rasters[[k]], polygons, fun = mean, na.rm = TRUE)
    out[k, ] <- vals[, 2]
  }
  colnames(out) <- polygons$treat_desc
  as.data.frame(out)
}



ensure_common_crs <- function(polygons, rasters) {
  if (!inherits(rasters, "SpatRaster")) stop("`rasters` must be a SpatRaster.")
  if (inherits(polygons, "sf")) {
    pv <- terra::vect(polygons)
  } else if (inherits(polygons, "SpatVector")) {
    pv <- polygons
  } else {
    stop("`polygons` must be an sf or SpatVector.")
  }
  cr_r <- terra::crs(rasters, proj = TRUE)
  if (!identical(terra::crs(pv, proj = TRUE), cr_r)) {
    pv <- terra::project(pv, cr_r)
  }
  pv
}


# =================== 3.2 ) PROCESS SEASONS ===================
# Map years to folder suffixes
# Processes Planet and Sentinel Data


  seasons <- site.info$seasons
  seasons <- seasons %>%
    dplyr::filter(Year == year_of_analysis) %>%
    dplyr::mutate(yr = as.numeric(Year),
           plant_date = as.Date(`Sowing date`)
           )
           
  
  ################################################################################
  ############################  Sentinel-2   #############################
  ################################################################################
  
 
  
  ratio_name <- 
    "NDVI" 
  #"EVI2" 
  # "ExG" 
  # "NDMI" 
  # "NDRE" 
  
  ratio_type <- paste0(ratio_name , "_Stack")
  
  
  
  # Path to your precomputed Sentinel ratio stack
  sen_path <- file.path(readDir, "7.In_Season_data", 
                        substr(year_of_analysis, 3, 4), 
                        "2.Satellite_Imagery",
                        "Sentinel", 
                         paste0(ratio_type, "_", extension, ".tif"))
                                                                  
  if (!file.exists(sen_path)) stop("Sentinel stack not found: ", sen_path)
  
  sen.dat <- terra::rast(sen_path)
  sen.dat <- terra::project(sen.dat,'epsg:7854')
  #sen.dat <- terra::project(sen.dat,'epsg:4326')
  nm <- names(sen.dat)
  
 
  
  ##### REMOVE CLOUD IMAGES - Future task to automate this!!
  
  # ---- 1) Parse acquisition dates from layer names (robust) ----
  
  # prefer 8-digit yyyymmdd anywhere in the name; fallback to yyyy-mm-dd
  dates_8   <- stringr::str_extract(nm, "(?<!\\d)\\d{8}(?!\\d)")
  dates_hy  <- stringr::str_extract(nm, "\\d{4}-\\d{2}-\\d{2}")
  dates_chr <- ifelse(!is.na(dates_8), dates_8, gsub("-", "", dates_hy))
  
  if (any(is.na(dates_chr))) {
    stop("Could not parse dates for layers: ", paste(nm[is.na(dates_chr)], collapse = ", "))
  }
  
  img_dates_sen <- as.Date(dates_chr, format = "%Y%m%d")
  
  # ---- 2) Make order deterministic: sort by date (oldest to newest) ----
  o <- order(img_dates_sen)
  sen.dat        <- sen.dat[[o]]
  img_dates_sen  <- img_dates_sen[o]
  
  # Give layers clean, informative names
  names(sen.dat) <- format(img_dates_sen, "%Y-%m-%d")
  
 
  
bad_dates_year <- bad_dates_list$Dates 
if (is.null(bad_dates_year)) bad_dates_year <- character(0)  
# Accept both "YYYY-mm-dd" and "YYYYmmdd"
bad_dates_year <- as.Date(bad_dates_year, tryFormats = c("%Y-%m-%d", "%Y%m%d"))
drop_idx <- which(img_dates_sen %in% bad_dates_year)  

if (length(drop_idx)) {
  message("Dropping ", length(drop_idx), " Sentinel layers by date: ",
          paste(format(img_dates_sen[drop_idx], "%Y-%m-%d"), collapse = ", "))
  sen.dat       <- sen.dat[[-drop_idx]]
  img_dates_sen <- img_dates_sen[-drop_idx]
  names(sen.dat) <- format(img_dates_sen, "%Y-%m-%d")
}  

## I am having trouble writing this new raster 
sen.dat
# #Note: Crystal Brook straddles 2x tiles, but the difference is not noticeable
# #We can simply just drop one of the tile sets.
 nm <- names(sen.dat)

# hqd_idx <- grep("HQD", nm, perl = TRUE)
# # If your names always have underscores around the tile, this also works:
#  hqd_idx <- grep("_T53HQD(_|$)", nm)
# 
# if (length(hqd_idx)) {
#   message("Dropping ", length(hqd_idx), " layers from tile T53HQD")
#   sen.dat <- sen.dat[[-hqd_idx]]
# } else {
#   message("No T53HQD layers present; nothing to drop.")
# }

 
# This might also work

# Keep only the first occurrence of each unique name
sen.dat <- sen.dat[[!duplicated(nm)]]

   
  # --- align polygons CRS to raster CRS -----------------------------------------
  trial.plan <- site.info$trial_plan
  
  trial.plan.v <- ensure_common_crs(trial.plan, sen.dat)
  polys_treat <- tryCatch(
    terra::dissolve(trial.plan.v, "treat_desc"),
    error = function(e) terra::aggregate(trial.plan.v, by = "treat_desc")
  )
  
  # --- DAP & window --------------------------------------------------------------
  
  plant_date <- seasons$plant_date
  ## Need to remove duplication in dates
  str(img_dates_sen)
  img_dates_sen <- unique(img_dates_sen) 
  dap_sen <- as.numeric(img_dates_sen - plant_date)
  
  
  
  # --- polygon means -------------------------------------------------------------
  
  ts_df_sen <- polygon_mean_timeseries(sen.dat, polys_treat)
  
  # make names unique so dplyr will work
  ts_df_sen <- as_tibble(ts_df_sen, .name_repair = "unique")
  
  # Long table (Â±50 DAP) and cleaning
  long_df_sen <- ts_df_sen %>%
    mutate(dap = dap_sen) %>%
    filter(dap >= -20 & dap <= 250) %>%
    #pivot_longer(-dap, names_to = "treat_desc", values_to = "ratio") %>%
    pivot_longer(-dap, names_to = "treat_desc", values_to = "ratio") %>% # JACKIE
    mutate(treat_desc = str_remove(treat_desc, "\\.\\d+$")) %>% # strip .1/.2 suffixes
    filter(!str_starts(treat_desc, "Buffer"),
           !str_starts(treat_desc, "Outside Control")) %>%
    arrange(treat_desc, dap)
  
  # Cumulative ratio (AUC via irregular day spacing)
  last_date_sen <- max(img_dates_sen, na.rm = TRUE)
  
  long_df_cum_sen <- long_df_sen %>%
    filter(dap >= 0) %>%                                # keep only non-negative days
    group_by(treat_desc, dap) %>%
    summarise(ratio = mean(ratio, na.rm = TRUE), .groups = "drop") %>%
    arrange(treat_desc, dap) %>%
    group_by(treat_desc) %>%
    mutate(
      ratio_lag = lag(ratio, default = first(ratio)),
      dap_lag  = lag(dap, default = 0),
      seg_area = 0.5 * (ratio + ratio_lag) * (dap - dap_lag),
      cum_ratio = cumsum(seg_area)
    ) %>%
    ungroup()
  
  
  
  
  # --- Save outputs in year folder  ---
  
out_csv_sentinel      <- file.path(saveDir, paste0("/",ratio_name , "_growth_curves_sentinel_", year_of_analysis,".csv"))
out_csv_cum_sentinel  <- file.path(saveDir, paste0("/",ratio_name , "_growth_curves_cumulative_sentinel_", year_of_analysis,".csv"))
  
write.csv(long_df_sen,     out_csv_sentinel,     row.names = FALSE)
write.csv(long_df_cum_sen, out_csv_cum_sentinel, row.names = FALSE)
  
  
metadata_processing <- file.path(saveDir, paste0("metadata_growth_curves_sentinel", year_of_analysis,".csv"))
write.csv(last_date_sen,     metadata_processing,     row.names = FALSE)
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ## COMPUTE BY ZONES
  # ================== 3.x ) Split ratio growth curves by zones ==================
  
  # 1. Ensure zones raster matches CRS and extent
  
  
  # zones <- terra::project(zones, sen.dat,method = 'near')
  # names(zones) <- "zone_id"
  # 
  # ================= ratio growth curves by ZONES (fresh, minimal) =================


zones_sf  
trial.plan  


# --- 1) Read zones (sf) and intersect with strips to get treatment×zone polys --
zones_sf <- sf::st_transform(zones_sf, sf::st_crs(trial.plan))  # align CRS
  
# keep only the zone id column + geometry
zones_sf <- zones_sf %>% dplyr::select(zone)
#stopifnot(zone_field %in% names(zones_sf))
#zones_sf <- zones_sf[, c(zone_field, attr(zones_sf, "sf_column"))]
  
  # intersection: each strip gets cut by zone polygons
  # Make geometries valid before intersection
trial.plan <- sf::st_make_valid(trial.plan)
zones_sf <-   sf::st_make_valid(zones_sf)
names(zones_sf)
names(trial.plan)
  
  # Then run intersection
tz_sf <- suppressWarnings(sf::st_intersection(trial.plan, zones_sf))
plot(tz_sf) 
tz_sf <- dplyr::mutate(tz_sf,
                         #zone_id    = .data[[zone_field]],#I removed the zone_field
                         zone_id    = zone,
                         treat_zone = paste0(.data$treat_desc, "__Z", .data$zone_id))
  
  # (optional) drop tiny slivers if they exist (skip if you prefer)
  # tz_sf <- tz_sf[sf::st_area(tz_sf) > units::set_units(50, "m^2"), ]  # only if CRS is metric
  
# convert to terra SpatVector in the same CRS as the raster
polys_treat_zone <- terra::vect(sf::st_transform(tz_sf, sf::st_crs(sen.dat)))
  
  # dissolve by treat_zone to avoid duplicates
  polys_treat_zone <- tryCatch(
    terra::dissolve(polys_treat_zone, "treat_zone"),
    error = function(e) terra::aggregate(polys_treat_zone, by = "treat_zone")
  )
  
  # --- 2) Extract mean ratio for each layer / polygon (one pass) ------------------
  means_list <- lapply(seq_len(terra::nlyr(sen.dat)), function(k) {
    terra::extract(sen.dat[[k]], polys_treat_zone, fun = mean, na.rm = TRUE)[, 2]
  })
  mat <- do.call(rbind, means_list)                                # rows = layers, cols = polygons
  colnames(mat) <- as.character(polys_treat_zone$treat_zone)
  
  # --- 3) Build dates & DAP from layer names -------------------------------------
  #Done above
  # --- 4) Tidy long with zone kept, window DAP, drop buffers ---------------------
  long_zone <- tibble::as_tibble(mat, .name_repair = "unique") |>
    dplyr::mutate(dap = dap_sen) |>
    tidyr::pivot_longer(-dap, names_to = "treat_zone", values_to = "ratio") |>
    tidyr::separate(treat_zone, into = c("treat_desc","zone_id"), sep = "__Z", remove = TRUE) |>
    dplyr::mutate(zone_id = factor(zone_id)) |>
    dplyr::filter(dap >= -20, dap <= 250) |>
    dplyr::filter(!stringr::str_starts(treat_desc, "Buffer"),
                  !stringr::str_starts(treat_desc, "Outside Control")) |>
    dplyr::arrange(zone_id, treat_desc, dap)
  
  # --- 5) Cumulative ratio per (zone, treatment), trapezoid from DAP >= 0 ---------
  long_cum_zone <- long_zone |>
    dplyr::filter(dap >= 0) |>
    dplyr::group_by(zone_id, treat_desc, dap) |>
    dplyr::summarise(ratio = mean(ratio, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(zone_id, treat_desc, dap) |>
    dplyr::group_by(zone_id, treat_desc) |>
    dplyr::mutate(
      ratio_lag = dplyr::lag(ratio, default = dplyr::first(ratio)),
      dap_lag  = dplyr::lag(dap,  default = 0),
      seg_area = 0.5 * (ratio + ratio_lag) * (dap - dap_lag),
      cum_ratio = cumsum(seg_area)
    ) |>
    dplyr::ungroup()
  
 
  
  # --- Save outputs in year folder  ---
 
out_csv_zonesentinel      <- file.path(saveDir, paste0(ratio_name, "_growth_curves_sentinel_ZONE_", year_of_analysis, ".csv"))
out_csv_zonecum_sentinel  <- file.path(saveDir, paste0(ratio_name, "_growth_curves_cumulative_sentinel_ZONE_", year_of_analysis, ".csv"))
  
  
  
write.csv(long_zone,     out_csv_zonesentinel,     row.names = FALSE)
write.csv(long_cum_zone, out_csv_zonecum_sentinel, row.names = FALSE)
  
  
  
  
  
  








