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

# ====================== PATHS ======================
site_1 <- "1.Walpeup_MRS125"
site_extension <- "MRS125"
readDir <- paste0("//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}/work/Output-1/", site_1)
#saveDir <- paste0("//fs1-cbr.nexus.csiro.au/{lw-soildatarepo}/work/Shiny/Apps/Stirling/GRDCSandySoilsII/Output1Viewer/Current/Files/",site_1)

Dir <- paste0("C:/Users/ouz001/working_from_home_post_Sep2022/sandy_soils_output1_app/Pre_processing_v2/",
                  site_1, "/preprocessing_output")


# ====================== METADATA ======================
site.info <- readRDS(paste0(Dir, "/site_info.rds"))


# ====================== Zones ======================  
zones <- rast(file.path(readDir, 
                        paste0("3.Covariates/6.Clusters_Zones/FINAL/",
                               site_extension,
                               "_Zones_round_wgs84_smooth.tif")))
zones.sf <- st_read(paste0(readDir,"/3.Covariates/6.Clusters_Zones/FINAL/",
                              site_extension,
                               "_Zones_round_wgs84_smooth.shp"))


################################################################################
####################### 3) Build Growth Curves  ################################
################################################################################

# ======================== 3.1 ) Define Functions========================

# # PlanetScope SR 8-band convention: Red = band 6, NIR = band 8
# ratio_from_stack <- function(x) {
#   red <- x[[6]]; nir <- x[[8]]
#   (nir - red) / (nir + red)
# }

# Read Planet 8b SR clips in a season folder, compute ratio, align & reproject to EPSG:4326
read_planet_ratio <- function(season_dir) {
  fls <- list.files(
    path = season_dir,
    pattern = "3B_AnalyticMS_SR_8b_clip\\.tif$",
    full.names = TRUE
  )
  if (length(fls) == 0L) stop("No Planet 8b SR clips found in: ", season_dir)
  
  # Sort by yyyymmdd at start of filename
  dates_chr <- str_extract(basename(fls), "^\\d{8}")
  ord <- order(dates_chr)
  fls <- fls[ord]; dates_chr <- dates_chr[ord]
  
  rs <- lapply(fls, rast)
  ratio_list <- lapply(rs, ratio_from_stack)
  
  template <- ratio_list[[1]]
  ratio_aligned <- lapply(ratio_list, function(r) terra::project(r, template))
  ratio_stack   <- rast(ratio_aligned)
  ratio_4326    <- terra::project(ratio_stack, "EPSG:4326")
  
  list(ratio = ratio_4326, dates = as.Date(dates_chr, format = "%Y%m%d"))
}

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

year_to_suffix <- function(y) sprintf("%02d", y %% 100)

i <- 2 ###JACKIE CHANGES THIS
#for (i in seq_len(nrow(site.info$seasons))) {
  yr         <- as.numeric(site.info$seasons$year[i])
  plant_date <- lubridate::dmy(site.info$seasons$plant_date[i])
  crop_type  <- site.info$seasons$crop_type[i]
  
  # if (is.na(plant_date)) {
  #   message("Skipping ", yr, " (no plant date)."); next
  # }
  
 
  
  
  ################################################################################
  ############################  Sentinel-2   #############################
  ################################################################################
  
 # if (is.na(plant_date)) stop("Season row 2 has no plant_date; cannot compute DAP.")
  
  saveDir_year <- file.path(Dir, as.character(yr))
  fs::dir_create(saveDir_year)
  
  ratio_name <- 
  #  "NDVI" 
  #"EVI2" 
  # "ExG" 
  # "NDMI" 
   "NDRE" 
  
  ratio_type <- paste0(ratio_name , "_Stack")
  
  
  
  # Path to your precomputed Sentinel ratio stack
  sen_path <- file.path(readDir, "7.In_Season_data", sprintf("%02d", yr %% 100),
                        "2.Satellite_Imagery", "Sentinel", 
                        paste0(ratio_type,"_",site_extension, ".tif")
                                                )
  if (!file.exists(sen_path)) stop("Sentinel stack not found: ", sen_path)
  
  sen.dat <- terra::rast(sen_path)
  sen.dat <- terra::project(sen.dat,'epsg:4326')
  
  # #Note: Crystal Brook strattles 2x tiles, but the difference is not noticable
  # #We can simply justdrop one of the tile sets.
  # nm <- names(sen.dat)
  # 
  # hqd_idx <- grep("HQD", nm, perl = TRUE)
  # # If your names always have underscores around the tile, this also works:
  # # hqd_idx <- grep("_T53HQD(_|$)", nm)
  # 
  # if (length(hqd_idx)) {
  #   message("Dropping ", length(hqd_idx), " layers from tile T53HQD")
  #   sen.dat <- sen.dat[[-hqd_idx]]
  # } else {
  #   message("No T53HQD layers present; nothing to drop.")
  # }
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
  
  # ---- 2) Make order deterministic: sort by date (oldest â newest) ----
  o <- order(img_dates_sen)
  sen.dat        <- sen.dat[[o]]
  img_dates_sen  <- img_dates_sen[o]
  
  # Give layers clean, informative names
  names(sen.dat) <- format(img_dates_sen, "%Y-%m-%d")
  
  # ---- 3) Drop by DATE (stable across any reordering) ----
  #plot(sen.dat)
  nlyr(sen.dat)
  plot(sen.dat[[43:46]])
  bad_dates_map <- list(
    "2024" = c("2024-04-03","2024-05-08","2024-06-12","2024-07-07","2024-07-12",
               "2024-07-17","2024-07-22","2024-07-27","2024-08-16","2024-08-21",
               "2024-08-26","2024-08-31","2024-09-05","2024-09-10","2024-09-20",
               "2024-09-25","2024-09-30","2024-10-05","2024-10-15","2024-11-09",
               "2024-11-24","2024-11-29"), #CHECK THESE
    "2025" = c("2025-04-07","2025-04-27","2025-05-02","2025-05-04","2025-05-17","2025-05-27",
               "2025-06-06","2025-06-13","2025-06-16","2025-07-26","2025-08-12",
               "2025-08-15","2025-08-25","2025-09-01","2025-09-04","2025-09-19",
               "2025-09-21","2025-09-24")
  )
  
  bad_dates_year <- bad_dates_map[[as.character(yr)]]
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
  
  
  # --- align polygons CRS to raster CRS -----------------------------------------
  trial.plan <- site.info$trial_plan
  
  trial.plan.v <- ensure_common_crs(trial.plan, sen.dat)
  polys_treat <- tryCatch(
    terra::dissolve(trial.plan.v, "treat_desc"),
    error = function(e) terra::aggregate(trial.plan.v, by = "treat_desc")
  )
  
  # --- DAP & window --------------------------------------------------------------
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
  
   out_csv_sentinel      <- file.path(saveDir_year, paste0(ratio_name , "_growth_curves_sentinel_", yr,".csv"))
   out_csv_cum_sentinel  <- file.path(saveDir_year, paste0(ratio_name , "_growth_curves_cumulative_sentinel_", yr,".csv"))
  
  write.csv(long_df_sen,     out_csv_sentinel,     row.names = FALSE)
  write.csv(long_df_cum_sen, out_csv_cum_sentinel, row.names = FALSE)
  
  
  metadata_processing <- file.path(saveDir_year, paste0("metadata_growth_curves_sentinel", yr,".csv"))
  write.csv(last_date_sen,     metadata_processing,     row.names = FALSE)
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ## COMPUTE BY ZONES
  # ================== 3.x ) Split ratio growth curves by zones ==================
  
  # 1. Ensure zones raster matches CRS and extent
  
  zones <- terra::project(zones, sen.dat,method = 'near')
  names(zones) <- "zone_id"
  
  # ================= ratio growth curves by ZONES (fresh, minimal) =================
  
  zone_field <- "cluster3"
  # --- 1) Read zones (sf) and intersect with strips to get treatment×zone polys --
  zones_sf <- sf::st_transform(zones.sf, sf::st_crs(trial.plan))  # align CRS
  
  # keep only the zone id column + geometry
  stopifnot(zone_field %in% names(zones_sf))
  zones_sf <- zones_sf[, c(zone_field, attr(zones_sf, "sf_column"))]
  
  # intersection: each strip gets cut by zone polygons
  tz_sf <- suppressWarnings(sf::st_intersection(trial.plan, zones_sf))
  tz_sf <- dplyr::mutate(tz_sf,
                         zone_id    = .data[[zone_field]],
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
  # out_csv_zonesentinel      <- file.path(saveDir_year, sprintf("ndvi_growth_curves_sentinel_ZONE%s.csv", yr))
  # out_csv_zonecum_sentinel  <- file.path(saveDir_year, sprintf("ndvi_growth_curves_cumulative_sentinel_ZONE%s.csv", yr))
  out_csv_zonesentinel      <- file.path(saveDir_year, paste0(ratio_name, "_growth_curves_sentinel_ZONE_", yr, ".csv"))
  out_csv_zonecum_sentinel  <- file.path(saveDir_year, paste0(ratio_name, "_growth_curves_cumulative_sentinel_ZONE_", yr, ".csv"))
  
  
  
  write.csv(long_zone,     out_csv_zonesentinel,     row.names = FALSE)
  write.csv(long_cum_zone, out_csv_zonecum_sentinel, row.names = FALSE)
  
  
  
  
  
  








