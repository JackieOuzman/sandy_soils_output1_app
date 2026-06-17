# =============================================================================
# SCRIPT 3: NDVI Image Export — af-sandysoils-ii
# =============================================================================
# Purpose:
#   Exports NDVI imagery for a selected site in three formats for use in
#   a Shiny application:
#
#   Option A — Individual PNGs (one per image date)
#              Best for: date slider in Shiny, low-bandwidth friendly per image
#              Output: <site_name>/PNG/<site_name>_NDVI_<date>.png
#
#   Option B — Animated GIF (full season time series)
#              Best for: quick overview, sharing in reports/presentations
#              Output: <site_name>_NDVI_animated.gif
#
#   Option C — Multiband GeoTIFF stack (clipped to paddock boundary)
#              Best for: interactive leaflet maps in Shiny, full resolution
#              Output: <site_name>_NDVI_stack.tif  (may already exist from Script 1)
#
#   All outputs clipped to paddock boundary.
#   Consistent NDVI colour ramp applied across all dates and sites:
#   brown (NDVI=0) -> yellow -> green (NDVI=1)
#
# Inputs:
#   - NDVI TIF files: headDir/7.In_Season_data/YY/8.Sentinel_QGIS_Jackie/
#   - Paddock boundary shapefile: from metadata sheet "file location etc",
#                                 variable == "boundary_shapefile"
#   - Clipped stack (if already saved by Script 1 Sub-step 1H)
#
# Outputs saved to:
#   headDir/7.In_Season_data/YY/8.Sentinel_QGIS_Jackie/Growth_curves_output/NDVI_images/
#
# Author:  Jackie Ouzman, CSIRO Agriculture & Food
# Project: af-sandysoils-ii
# Created: June 2025
# =============================================================================

rm(list = ls())
#install.packages("gifski")
#install.packages("magick")
suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(readxl)
  library(ggplot2)
  library(tidyterra)   # for geom_spatraster in ggplot
  library(gifski)      # for GIF rendering
})

# =============================================================================
# USER INPUT — change site number only
# =============================================================================

site_number_input <- 1   # 1 through 6

# Which exports to run — set to FALSE to skip
run_png <- TRUE
run_gif <- TRUE
run_cog <- TRUE

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
# PATHS
# =============================================================================

year_of_analysis <- 2025
yr_short         <- substr(as.character(year_of_analysis), 3, 4)

dir           <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
headDir       <- file.path(dir, "work", "Output-1", site_number)
metadata_path <- file.path(dir, "work", "Output-1", "0.Site-info")
metadata_file <- "names of treatments per site 2025 metadata and other info.xlsx"

ndvi_dir <- file.path(headDir,
                      "7.In_Season_data", yr_short,
                      "8.Sentinel_QGIS_Jackie")

# All outputs go here
saveDir <- file.path(ndvi_dir, "Growth_curves_output", "NDVI_images")
png_dir <- file.path(saveDir, "PNG")

for (d in c(saveDir, png_dir)) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
    message("Created directory: ", d)
  }
}

# =============================================================================
# READ METADATA
# =============================================================================

meta <- readxl::read_excel(
  file.path(metadata_path, metadata_file),
  sheet = "file location etc"
) %>%
  filter(Site == site_number)

meta_val <- function(var_name, col = "file path") {
  meta %>%
    filter(variable == var_name) %>%
    pull(col) %>%
    .[1]
}

# =============================================================================
# LOAD AND SORT NDVI TIFS
# =============================================================================

cat("\n--- Loading NDVI TIF files ---\n")

ndvi_files <- list.files(
  path       = ndvi_dir,
  pattern    = "NDVI.*10m\\.tif$",
  full.names = TRUE
)

if (length(ndvi_files) == 0) stop("No NDVI TIF files found in: ", ndvi_dir)
cat("Found", length(ndvi_files), "files\n")

fnames <- basename(ndvi_files)

dates_parsed <- suppressWarnings(
  as.Date(str_extract(fnames, "\\d{4}-\\d{2}-\\d{2}"), format = "%Y-%m-%d")
)
missing_idx <- which(is.na(dates_parsed))
if (length(missing_idx) > 0) {
  compact <- str_extract(fnames[missing_idx], "(?<!\\d)\\d{8}(?!\\d)")
  dates_parsed[missing_idx] <- as.Date(compact, format = "%Y%m%d")
}

dup_flag     <- duplicated(dates_parsed)
ndvi_files   <- ndvi_files[!dup_flag]
dates_parsed <- dates_parsed[!dup_flag]
ord          <- order(dates_parsed)
ndvi_files   <- ndvi_files[ord]
img_dates    <- dates_parsed[ord]

cat("Image dates:", paste(format(img_dates), collapse = ", "), "\n")

# =============================================================================
# LOAD BOUNDARY AND CLIP RASTER STACK
# =============================================================================

cat("\n--- Clipping to paddock boundary ---\n")

boundary_path <- paste0(headDir, meta_val("boundary_shapefile"))
cat("Boundary:", boundary_path, "\n")

if (!file.exists(boundary_path)) stop("Boundary shapefile not found: ", boundary_path)

boundary   <- sf::st_read(boundary_path, quiet = TRUE)
boundary_v <- terra::vect(boundary)

# Check for pre-existing clipped stack from Script 1 Sub-step 1H
stack_file <- file.path(ndvi_dir, "Growth_curves_output",
                        paste0(site_name, "_NDVI_stack.tif"))

if (file.exists(stack_file)) {
  cat("Loading pre-existing clipped stack:", stack_file, "\n")
  sen_clipped <- terra::rast(stack_file)
  names(sen_clipped) <- format(img_dates, "%Y-%m-%d")
} else {
  cat("No pre-existing stack found — loading and clipping individual TIFs\n")
  sen_raw     <- terra::rast(ndvi_files)
  boundary_v  <- terra::project(boundary_v, terra::crs(sen_raw))
  sen_clipped <- terra::crop(sen_raw, boundary_v) %>%
    terra::mask(boundary_v)
  names(sen_clipped) <- format(img_dates, "%Y-%m-%d")
}

cat("Clipped stack:", nlyr(sen_clipped), "layers\n")

# =============================================================================
# CONSISTENT NDVI COLOUR RAMP
# =============================================================================
# Standard NDVI ramp: brown/tan for bare soil/low NDVI,
# yellow for moderate, bright green for high canopy cover.
# Fixed limits 0 to 1 so colours are consistent across all dates and sites.

ndvi_colours <- c(
  "#8B4513",   # brown      — bare soil / very low NDVI (0.0)
  "#C8A951",   # tan/yellow — sparse cover             (0.2)
  "#FFFF00",   # yellow     — moderate                 (0.4)
  "#ADDE63",   # light green                           (0.6)
  "#41AB5D",   # mid green                             (0.75)
  "#006837"    # dark green — dense canopy             (1.0)
)

ndvi_breaks <- c(0, 0.2, 0.4, 0.6, 0.75, 1.0)

# =============================================================================
# OPTION A: INDIVIDUAL PNGs
# =============================================================================

if (run_png) {
  
  cat("\n--- Option A: Exporting individual PNGs ---\n")
  
  for (i in seq_len(nlyr(sen_clipped))) {
    
    lyr   <- sen_clipped[[i]]
    ddate <- img_dates[i]
    dstr  <- format(ddate, "%Y-%m-%d")
    
    p <- ggplot() +
      tidyterra::geom_spatraster(data = lyr) +
      scale_fill_gradientn(
        colours = ndvi_colours,
        values  = scales::rescale(ndvi_breaks),
        limits  = c(0, 1),
        na.value = "transparent",
        name    = "NDVI"
      ) +
      labs(
        title   = paste0(site_name, " — NDVI"),
        subtitle = format(ddate, "%d %B %Y")
      ) +
      theme_void() +
      theme(
        plot.title       = element_text(face = "bold", size = 11, hjust = 0.5),
        plot.subtitle    = element_text(size = 9, hjust = 0.5, colour = "grey30"),
        legend.position  = "right",
        legend.key.height = unit(2, "cm"),
        legend.title     = element_text(size = 9),
        legend.text      = element_text(size = 8),
        plot.background  = element_rect(fill = "white", colour = NA)
      )
    
    out_png <- file.path(png_dir,
                         paste0(site_name, "_NDVI_", dstr, ".png"))
    ggsave(out_png, plot = p, width = 12, height = 10,
           units = "cm", dpi = 150, bg = "white")
    
    if (i %% 5 == 0 || i == nlyr(sen_clipped)) {
      cat("  Saved", i, "of", nlyr(sen_clipped), "PNGs\n")
    }
  }
  
  cat("PNG export complete. Files in:\n  ", png_dir, "\n")
}

# =============================================================================
# OPTION B: ANIMATED GIF
# =============================================================================

if (run_gif) {
  
  cat("\n--- Option B: Rendering animated GIF ---\n")
  
  gif_file <- file.path(saveDir, paste0(site_name, "_NDVI_animated.gif"))
  
  # Render each frame as a temp PNG then stitch with gifski
  tmp_dir <- file.path(tempdir(), "ndvi_gif_frames")
  if (!dir.exists(tmp_dir)) dir.create(tmp_dir)
  
  frame_files <- c()
  
  for (i in seq_len(nlyr(sen_clipped))) {
    
    lyr   <- sen_clipped[[i]]
    ddate <- img_dates[i]
    
    p <- ggplot() +
      tidyterra::geom_spatraster(data = lyr) +
      scale_fill_gradientn(
        colours  = ndvi_colours,
        values   = scales::rescale(ndvi_breaks),
        limits   = c(0, 1),
        na.value = "transparent",
        name     = "NDVI"
      ) +
      labs(
        title    = paste0(site_name, " — NDVI"),
        subtitle = format(ddate, "%d %B %Y")
      ) +
      theme_void() +
      theme(
        plot.title        = element_text(face = "bold", size = 11, hjust = 0.5),
        plot.subtitle     = element_text(size = 9, hjust = 0.5, colour = "grey30"),
        legend.position   = "right",
        legend.key.height = unit(2, "cm"),
        legend.title      = element_text(size = 9),
        legend.text       = element_text(size = 8),
        plot.background   = element_rect(fill = "white", colour = NA)
      )
    
    frame_path <- file.path(tmp_dir, sprintf("frame_%03d.png", i))
    ggsave(frame_path, plot = p, width = 12, height = 10,
           units = "cm", dpi = 120, bg = "white")
    frame_files <- c(frame_files, frame_path)
    
    if (i %% 5 == 0 || i == nlyr(sen_clipped)) {
      cat("  Rendered frame", i, "of", nlyr(sen_clipped), "\n")
    }
  }
  
  gifski::gifski(
    png_files = frame_files,
    gif_file  = gif_file,
    width     = 480,
    height    = 400,
    delay     = 0.8    # seconds per frame — adjust for faster/slower playback
  )
  
  # Clean up temp frames
  unlink(tmp_dir, recursive = TRUE)
  cat("GIF saved to:\n  ", gif_file, "\n")
  cat("File size:", round(file.size(gif_file) / 1e6, 1), "MB\n")
}

# =============================================================================
# OPTION C: CLOUD OPTIMISED GEOTIFF STACK
# =============================================================================

if (run_cog) {
  
  cat("\n--- Option C: Saving clipped multiband GeoTIFF stack ---\n")
  
  cog_file <- file.path(saveDir,
                        paste0(site_name, "_NDVI_stack.tif"))
  
  # Write with LZW compression and tiling for efficient partial reads
  terra::writeRaster(
    sen_clipped,
    filename  = cog_file,
    overwrite = TRUE,
    gdal      = c("COMPRESS=LZW",
                  "TILED=YES",
                  "BLOCKXSIZE=256",
                  "BLOCKYSIZE=256",
                  "COPY_SRC_OVERVIEWS=YES")
  )
  
  cat("GeoTIFF stack saved to:\n  ", cog_file, "\n")
  cat("File size:", round(file.size(cog_file) / 1e6, 1), "MB\n")
  cat("Layers:", nlyr(sen_clipped), "| Names:", paste(names(sen_clipped), collapse = ", "), "\n")
}

cat("\n=== Script 3 complete ===\n")
cat("All outputs saved to:\n  ", saveDir, "\n")
