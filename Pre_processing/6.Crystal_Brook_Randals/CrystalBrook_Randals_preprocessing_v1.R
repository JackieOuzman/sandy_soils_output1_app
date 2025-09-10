rm(list=ls())
# Pre-process data for the Site Viewer (all layers in EPSG:4326)
suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(fs)
})

# ====================== PATHS ======================
readDir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}/work/Output-1/6.Crystal_Brook_Randals"
saveDir <- "//fs1-cbr.nexus.csiro.au/{lw-soildatarepo}/work/Shiny/Apps/Stirling/GRDCSandySoilsII/Output1Viewer/Current/Files/6.CrystalBrook_Randals"
dir_create(saveDir)

################################################################################
######################## 1) Read in Paddock maps ###############################
################################################################################
soil <- rast(file.path(readDir, "9.Maps/Soil/all_maps_4326.tif"))
#names(soil) <- c("Subsoil_Clay_pct","DepthToB","DepthToClay","DepthToFizz",
#                 "Surface_pH_cacl","Surface_pH_h2O","Repellence","Surface_Carbonates")

zones <- rast(file.path(readDir, "3.Covariates/6.Clusters_Zones/FINAL/RAN_Opt_Clusters_85_integer.tif"))

writeRaster(soil,paste0(saveDir,'/soil.tif'),overwrite=T)
writeRaster(zones,paste0(saveDir,'/zones.tif'),overwrite=T)

################################################################################
######################## 2) Paddock Information  ###############################
################################################################################
boundary   <- suppressMessages(st_read(file.path(readDir, "1.Paddock_Boundary/Crystal_Brook_Randals_Boundary_Masked_4326.shp"), quiet = TRUE))
trial.plan <- suppressMessages(st_read(file.path(readDir, "5.Trial_Plan/FINAL-Trial-Plan/GIS/Crystal_Brook_Randals_PlotStrips_epsg4326.shp"), quiet = TRUE))
stopifnot("treat_desc" %in% names(trial.plan))

seasons <- tribble(
  ~year, ~crop_type, ~plant_date,   ~harvest_date,
  2024, NA,  NA_character_,  NA_character_,
  2025, "Wheat",    "05/06/2025",  NA_character_,
  2026, NA,         NA_character_, NA_character_,
  2027, NA,         NA_character_, NA_character_
) %>%
  mutate(
    year         = as.integer(year),
    plant_date   = dmy(plant_date),
    harvest_date = dmy(harvest_date)
  )

# --- minimal CRS checks (good) ---
stopifnot(st_crs(boundary)$epsg == 4326)
stopifnot(st_crs(trial.plan)$epsg == 4326)
stopifnot("treat_desc" %in% names(trial.plan))

# --- assemble named bundle ---
site.info <- list(
  site_id    = "Crystal_Brook_Randals",
  boundary   = boundary,      # sf
  trial_plan = trial.plan,    # sf
  seasons    = seasons        # tibble
)
class(site.info) <- c("ssii_site", class(site.info))

# --- optional: quick validator/helper ---
validate_site <- function(x) {
  stopifnot(inherits(x, "ssii_site"))
  stopifnot(all(c("site_id","boundary","trial_plan","seasons") %in% names(x)))
  stopifnot(st_crs(x$boundary)$epsg == 4326, st_crs(x$trial_plan)$epsg == 4326)
  stopifnot(is.integer(x$seasons$year))
  invisible(x)
}
validate_site(site.info)

# --- optional: persist ---
saveRDS(site.info, file.path(saveDir, "site_info.rds"))

################################################################################
####################### 3) Build Growth Curves  ################################
################################################################################

# ======================== 3.1 ) Define Functions========================

# PlanetScope SR 8-band convention: Red = band 6, NIR = band 8
ndvi_from_stack <- function(x) {
  red <- x[[6]]; nir <- x[[8]]
  (nir - red) / (nir + red)
}

# Read Planet 8b SR clips in a season folder, compute NDVI, align & reproject to EPSG:4326
read_planet_ndvi <- function(season_dir) {
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
  ndvi_list <- lapply(rs, ndvi_from_stack)
  
  template <- ndvi_list[[1]]
  ndvi_aligned <- lapply(ndvi_list, function(r) terra::project(r, template))
  ndvi_stack   <- rast(ndvi_aligned)
  ndvi_4326    <- terra::project(ndvi_stack, "EPSG:4326")
  
  list(ndvi = ndvi_4326, dates = as.Date(dates_chr, format = "%Y%m%d"))
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

for (i in seq_len(nrow(site.info$seasons))) {
  yr         <- site.info$seasons$year[i]
  plant_date <- site.info$seasons$plant_date[i]
  crop_type  <- site.info$seasons$crop_type[i]
  
  if (is.na(plant_date)) {
    message("Skipping ", yr, " (no plant date)."); next
  }
  
  # ================= 3.3 ) Planet Imagery ===================
  
  season_dir   <- file.path(readDir, "7.In_Season_data", year_to_suffix(yr), "2.Satellite_Imagery/Planet/PSScene")
  saveDir_year <- file.path(saveDir, as.character(yr))
  fs::dir_create(saveDir_year)
  
  # --- Read NDVI stack & dates ---
  pl <- read_planet_ndvi(season_dir)
  ndvi <- pl$ndvi
  img_dates <- pl$dates
  
  dap <- as.numeric(img_dates - plant_date)
  
  rm_idx <- integer(0)
  if (length(rm_idx)) {
    ndvi <- ndvi[[-rm_idx]]
    img_dates <- img_dates[-rm_idx]
    dap <- dap[-rm_idx]
  }
  
  names(ndvi) <- as.character(img_dates)
  
  # --- align polygons CRS to raster CRS -----------------------------------------
  trial.plan.v <- ensure_common_crs(trial.plan, ndvi)
  polys_treat <- tryCatch(
    terra::dissolve(trial.plan.v, "treat_desc"),
    error = function(e) terra::aggregate(trial.plan.v, by = "treat_desc")
  )
  
  # --- Polygon means ---
  ts_df <- polygon_mean_timeseries(ndvi, polys_treat)
  
  # make names unique so dplyr will work
  ts_df <- as_tibble(ts_df, .name_repair = "unique")
  
  long_df <- ts_df %>%
    mutate(dap = dap) %>%
    filter(dap >= -50 & dap <= 150) %>%                 # keep only -50..50 DAP
    pivot_longer(-dap, names_to = "treat_desc", values_to = "ndvi") %>%
    mutate(treat_desc = str_remove(treat_desc, "\\.\\d+$")) %>% # strip .1/.2 suffixes
    filter(!str_starts(treat_desc, "Buffer"),
           !str_starts(treat_desc, "Outside Control")) %>%
    arrange(treat_desc, dap)
  
  # --- Cumulative NDVI ---
  long_df_cum <- long_df %>%
    arrange(treat_desc, dap) %>%
    group_by(treat_desc) %>%
    mutate(
      days_diff = c(0, diff(dap)),
      cum_ndvi = cumsum(ndvi * days_diff)  # area-adjusted cumulative NDVI
    ) %>%
    ungroup()
  
  last_date_planet <- max(img_dates, na.rm = TRUE)
  
  p <- ggplot(long_df, aes(x = dap, y = ndvi, color = treat_desc, group = treat_desc)) +
    geom_line(alpha = 0.25) +
    geom_smooth(method = "gam", formula = y ~ s(x, k = 8), se = FALSE) +
    scale_x_continuous(
      name = "Days after planting (DAP)",
      sec.axis = sec_axis(
        trans = ~ plant_date + . ,  # transform DAP into date
        name = "Date",
        labels = function(x) format(as.Date(x, origin = "1970-01-01"), "%d-%b") # e.g., 05-Jun
      )
    ) +
    labs(
      title = paste0("NDVI Timeseries (", yr, ") ",site.info[[1]]),
      y = "Average NDVI",
      color = "Treatment"
    ) +
    theme_minimal() +
    theme(
      plot.title      = element_text(hjust = 0.5),
      axis.title.x    = element_text(size = 16),   # bottom x title
      axis.title.x.top= element_text(size = 16, margin = ggplot2::margin(b = 10)), # top x title
      axis.text.x     = element_text(size = 16),   # bottom x tick labels
      axis.text.x.top = element_text(size = 16),   # top x tick labels
      axis.title.y    = element_text(size = 16),
      axis.text.y     = element_text(size = 16)
    )+
    annotate("text",
             x = max(long_df$dap, na.rm = TRUE),
             y = max(long_df$ndvi, na.rm = TRUE) + 0.05,
             label = paste("Latest cloud free image date:", format(last_date_planet, "%d-%b-%Y")),
             hjust = 1, vjust = 0, size = 4, color = "black")
  
  p
  
  # --- Cumulative NDVI plot ---
  p_cum <- ggplot(long_df_cum, aes(x = dap, y = cum_ndvi, color = treat_desc, group = treat_desc)) +
    geom_line(alpha = 0.25) +
    geom_smooth(method = "gam", formula = y ~ s(x, k = 8), se = FALSE) +
    labs(
      title = paste0("Cumulative NDVI Timeseries (", yr, ") ",site.info[[1]]),
      x = "Days after planting",
      y = "Cumulative NDVI (AUC)",
      color = "Treatment"
    ) +
    theme_minimal() +
    theme(
      plot.title      = element_text(hjust = 0.5),
      axis.title.x    = element_text(size = 16),   # bottom x title
      axis.title.x.top= element_text(size = 16, margin = ggplot2::margin(b = 10)), # top x title
      axis.text.x     = element_text(size = 16),   # bottom x tick labels
      axis.text.x.top = element_text(size = 16),   # top x tick labels
      axis.title.y    = element_text(size = 16),
      axis.text.y     = element_text(size = 16)
    )+
    annotate("text",
             x = max(long_df$dap, na.rm = TRUE),
             y = max(long_df$ndvi, na.rm = TRUE) + 0.05,
             label = paste("Latest cloud free image date:", format(last_date_planet, "%d-%b-%Y")),
             hjust = 1, vjust = 0, size = 4, color = "black")
  
  # --- Save outputs in year folder ---
  out_csv_planet      <- file.path(saveDir_year, sprintf("ndvi_growth_curves_planet_%s.csv", yr))
  out_csv_cum_planet  <- file.path(saveDir_year, sprintf("ndvi_growth_curves_cumulative_planet_%s.csv", yr))
  out_plot_planet     <- file.path(saveDir_year, sprintf("ndvi_growth_curves_planet_%s.png", yr))
  out_plot_cum_planet <- file.path(saveDir_year, sprintf("ndvi_growth_curves_cumulative_planet_%s.png", yr))
  out_rds_planet      <- file.path(saveDir_year, sprintf("ndvi_stack_planet_%s.rds", yr))
  
  write.csv(long_df,     out_csv_planet,     row.names = FALSE)
  write.csv(long_df_cum, out_csv_cum_planet, row.names = FALSE)
  ggsave(out_plot_planet,     p,     width = 8, height = 5, dpi = 300)
  ggsave(out_plot_cum_planet, p_cum, width = 8, height = 5, dpi = 300)
  saveRDS(ndvi, out_rds_planet)
  
  
  
  ################################################################################
  ############################ 3.4) Sentinel-2 NDVI  #############################
  ################################################################################
  
  if (is.na(plant_date)) stop("Season row 2 has no plant_date; cannot compute DAP.")
  
  saveDir_year <- file.path(saveDir, as.character(yr))
  fs::dir_create(saveDir_year)
  
  # Path to your precomputed Sentinel NDVI stack
  sen_path <- file.path(readDir, "7.In_Season_data", sprintf("%02d", yr %% 100),
                        "2.Satellite_Imagery", "Sentinel", "NDVI_Stack_RAN.tif")
  if (!file.exists(sen_path)) stop("Sentinel NDVI stack not found: ", sen_path)
  
  sen.dat <- terra::rast(sen_path)
  sen.dat <- terra::project(sen.dat,'epsg:4326')
  
  #Note: Crystal Brook strattles 2x tiles, but the difference is not noticable
  #We can simply justdrop one of the tile sets.
  nm <- names(sen.dat)
  
  hqd_idx <- grep("HQD", nm, perl = TRUE)
  # If your names always have underscores around the tile, this also works:
  # hqd_idx <- grep("_T53HQD(_|$)", nm)
  
  if (length(hqd_idx)) {
    message("Dropping ", length(hqd_idx), " layers from tile T53HQD")
    sen.dat <- sen.dat[[-hqd_idx]]
  } else {
    message("No T53HQD layers present; nothing to drop.")
  }
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
  plot(sen.dat)
  nlyr(sen.dat)
  
  bad_dates_map <- list(
    "2024" = c("2024-04-03"),
    "2025" = c("2025-04-28","2025-05-28","2025-06-07","2025-06-09",
               "2025-06-12","2025-06-17","2025-06-22","2025-06-27",
               "2025-06-29","2025-07-02",
               "2025-07-07","2025-07-17","2025-07-19","2025-07-27",
               "2025-08-11","2025-08-16","2025-08-21","2025-08-26",
               "2025-08-28","2025-08-31")
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
    filter(dap >= -50 & dap <= 150) %>%
    pivot_longer(-dap, names_to = "treat_desc", values_to = "ndvi") %>%
    mutate(treat_desc = str_remove(treat_desc, "\\.\\d+$")) %>% # strip .1/.2 suffixes
    filter(!str_starts(treat_desc, "Buffer"),
           !str_starts(treat_desc, "Outside Control")) %>%
    arrange(treat_desc, dap)
  
  # Cumulative NDVI (AUC via irregular day spacing)
  long_df_cum_sen <- long_df_sen %>%
    arrange(treat_desc, dap) %>%
    group_by(treat_desc) %>%
    mutate(
      days_diff = c(0, diff(dap)),
      cum_ndvi  = cumsum(ndvi * days_diff)
    ) %>%
    ungroup()
  
  last_date_sen <- max(img_dates_sen, na.rm = TRUE)
  
  
  # Plots (use numeric origin for sec.axis to avoid date hiccups)
  p_sen <- ggplot(long_df_sen, aes(x = dap, y = ndvi, color = treat_desc, group = treat_desc)) +
    geom_line(alpha = 0.25) +
    geom_smooth(method = "gam", formula = y ~ s(x, k = 8), se = FALSE) +
    scale_x_continuous(
      name = "Days after planting (DAP)",
      sec.axis = sec_axis(
        trans  = ~ as.numeric(as.Date(plant_date)) + .,
        name   = "Date",
        labels = function(x) format(as.Date(x, origin = "1970-01-01"), "%d-%b")
      )
    ) +
    labs(
      title = paste0("Sentinel NDVI Timeseries (", yr, ") ",site.info[[1]]),
      y = "Average NDVI",
      color = "Treatment"
    ) +
    theme_minimal() +
    theme(
      plot.title      = element_text(hjust = 0.5),
      axis.title.x    = element_text(size = 16),   # bottom x title
      axis.title.x.top= element_text(size = 16, margin = ggplot2::margin(b = 10)), # top x title
      axis.text.x     = element_text(size = 16),   # bottom x tick labels
      axis.text.x.top = element_text(size = 16),   # top x tick labels
      axis.title.y    = element_text(size = 16),
      axis.text.y     = element_text(size = 16)
    )+
    annotate("text",
             x = max(long_df_sen$dap, na.rm = TRUE),
             y = max(long_df_sen$ndvi, na.rm = TRUE) + 0.05,
             label = paste("Latest cloud free image date:", format(last_date_sen, "%d-%b-%Y")),
             hjust = 1, vjust = 0, size = 4, color = "black")
  
  p_sen
  
  p_cum_sen <- ggplot(long_df_cum_sen, aes(x = dap, y = cum_ndvi, color = treat_desc, group = treat_desc)) +
    geom_line(alpha = 0.25) +
    geom_smooth(method = "gam", formula = y ~ s(x, k = 8), se = FALSE) +
    labs(
      title = paste0("Sentinel Cumulative NDVI Timeseries (", yr, ") ",site.info[[1]]),
      x = "Days after planting",
      y = "Cumulative NDVI (AUC)",
      color = "Treatment"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))+
    annotate("text",
             x = max(long_df_sen$dap, na.rm = TRUE),
             y = max(long_df_sen$ndvi, na.rm = TRUE) + 0.05,
             label = paste("Latest cloud free image date:", format(last_date_sen, "%d-%b-%Y")),
             hjust = 1, vjust = 0, size = 4, color = "black")
  p_cum_sen
  
  # --- Save outputs in year folder (mirrors Planet names) ---
  out_csv_sentinel      <- file.path(saveDir_year, sprintf("ndvi_growth_curves_sentinel_%s.csv", yr))
  out_csv_cum_sentinel  <- file.path(saveDir_year, sprintf("ndvi_growth_curves_cumulative_sentinel_%s.csv", yr))
  out_plot_sentinel     <- file.path(saveDir_year, sprintf("ndvi_growth_curves_sentinel_%s.png", yr))
  out_plot_cum_sentinel <- file.path(saveDir_year, sprintf("ndvi_growth_curves_cumulative_sentinel_%s.png", yr))
  out_rds_sentinel      <- file.path(saveDir_year, sprintf("ndvi_stack_sentinel_%s.rds", yr))
  
  write.csv(long_df_sen,     out_csv_sentinel,     row.names = FALSE)
  write.csv(long_df_cum_sen, out_csv_cum_sentinel, row.names = FALSE)
  ggsave(out_plot_sentinel,     p_sen,     width = 8, height = 5, dpi = 300)
  ggsave(out_plot_cum_sentinel, p_cum_sen, width = 8, height = 5, dpi = 300)
  saveRDS(sen.dat, out_rds_sentinel)
  
  
  message("Saved outputs to: ", saveDir_year)
}




