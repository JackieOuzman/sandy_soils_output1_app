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
saveDir <- "//fs1-cbr.nexus.csiro.au/{lw-soildatarepo}/work/Shiny/Apps/Stirling/GRDCSandySoilsII/Output1Viewer/Current/Files/CrystalBrook_Randals"
#dir_create(saveDir)

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
##################### 3) Planet satellite imagery  #############################
################################################################################

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

# =================== 3.1 ) PROCESS SEASONS ===================
# Map years to folder suffixes
year_to_suffix <- function(y) sprintf("%02d", y %% 100)

for (i in seq_len(nrow(site.info$seasons))) {
  yr         <- site.info$seasons$year[i]
  plant_date <- site.info$seasons$plant_date[i]
  crop_type  <- site.info$seasons$crop_type[i]
  
  if (is.na(plant_date)) {
    message("Skipping ", yr, " (no plant date)."); next
  }
  
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
  
  # --- Polygon means ---
  ts_df <- polygon_mean_timeseries(ndvi, trial.plan)
  
  # make names unique so dplyr will work
  ts_df <- as_tibble(ts_df, .name_repair = "unique")
  
  long_df <- ts_df %>%
    mutate(dap = dap) %>%
    filter(dap >= -50 & dap <= 50) %>%                 # keep only -50..50 DAP
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
      title = paste0("NDVI Timeseries (", yr, ")"),
      y = "Average NDVI",
      color = "Treatment"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.title.x.top = element_text(margin = margin(b = 10))
    )
  
  p
  
  # --- Cumulative NDVI plot ---
  p_cum <- ggplot(long_df_cum, aes(x = dap, y = cum_ndvi, color = treat_desc, group = treat_desc)) +
    geom_line(alpha = 0.25) +
    geom_smooth(method = "gam", formula = y ~ s(x, k = 8), se = FALSE) +
    labs(
      title = paste0("Cumulative NDVI Timeseries (", yr, ")"),
      x = "Days after planting",
      y = "Cumulative NDVI (AUC)",
      color = "Treatment"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # --- Save outputs in year folder ---
  out_csv      <- file.path(saveDir_year, paste0("ndvi_growth_curves_", yr, ".csv"))
  out_csv_cum  <- file.path(saveDir_year, paste0("ndvi_growth_curves_cumulative_", yr, ".csv"))
  out_plot     <- file.path(saveDir_year, paste0("ndvi_growth_curves_", yr, ".png"))
  out_plot_cum <- file.path(saveDir_year, paste0("ndvi_growth_curves_cumulative_", yr, ".png"))
  out_rds      <- file.path(saveDir_year, paste0("ndvi_stack_", yr, ".rds"))
  
  write.csv(long_df, out_csv, row.names = FALSE)
  write.csv(long_df_cum, out_csv_cum, row.names = FALSE)
  ggsave(out_plot, p, width = 8, height = 5, dpi = 300)
  ggsave(out_plot_cum, p_cum, width = 8, height = 5, dpi = 300)
  saveRDS(ndvi, out_rds)
  
  message("Saved outputs to: ", saveDir_year)
}
