
### At the moment this is for just one site but I need to change it so it runs multiple sites




rm(list=ls())
# Pre-process data for the Site Viewer (all layers in EPSG:4326)
#install.packages("tidyverse")
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
  library(tidyverse)
})

# ====================== PATHS ======================
site_1 <- "1.Walpeup_MRS125"

readDir <- paste0("//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}/work/Output-1/", site_1)
#saveDir <- paste0("//fs1-cbr.nexus.csiro.au/{lw-soildatarepo}/work/Shiny/Apps/Stirling/GRDCSandySoilsII/Output1Viewer/Current/Files/",site_1)

saveDir <- paste0("C:/Users/ouz001/working_from_home_post_Sep2022/sandy_soils_output1_app/Pre_processing_v2/",
                  site_1, "/preprocessing_output")

metadata_path <- "//fs1-cbr.nexus.csiro.au/{lw-soildatarepo}/work/Shiny/Apps/Stirling/GRDCSandySoilsII/Output1Viewer/"

################################################################################
######################## 1) Read in Paddock maps ###############################
################################################################################
soil <- rast(file.path(readDir, "9.Maps/Soil/keymaps/keysoil.tif"))
names(soil) <- c("Surface_pH","Subsoil_Clay_pct","Repellence","DepthToFizz","DepthToClay","DepthToNeutral")

zones <- rast(file.path(readDir, "3.Covariates/6.Clusters_Zones/FINAL/MRS125_Zones_round_wgs84_smooth.tif"))
zones.sf <- st_read(paste0(readDir,"/3.Covariates/6.Clusters_Zones/FINAL/MRS125_Zones_round_wgs84_smooth.shp"))

#writeRaster(soil,paste0(saveDir,'/soil.tif'),overwrite=T)
#writeRaster(zones,paste0(saveDir,'/zones.tif'),overwrite=T)

################################################################################
######################## 2) Paddock Information  ###############################
################################################################################
boundary   <- suppressMessages(st_read(file.path(readDir, "1.Paddock_Boundary/Walpeup_MRS125_Boundary_Masked_4326.shp"), quiet = TRUE))
trial.plan <- suppressMessages(st_read(file.path(readDir, "5.Trial_Plan/FINAL-Trial-Plan/GIS/MRS125_Strips_FINAL_wgs84.shp"), quiet = TRUE))
stopifnot("treat_desc" %in% names(trial.plan))
seasons <- read_csv(paste0(metadata_path, "metadata_all_sites.csv"))
seasons$year <- as.integer(seasons$year)

# seasons <- tribble(
#   ~year, ~crop_type, ~plant_date,   ~harvest_date,
#   2024, "Lentils",  "29/05/2024",  NA_character_,
#   2025, "Wheat",    "27/04/2025",  NA_character_,
#   2026, NA,         NA_character_, NA_character_,
#   2027, NA,         NA_character_, NA_character_
# ) %>%
#   mutate(
#     year         = as.integer(year),
#     plant_date   = dmy(plant_date),
#     harvest_date = dmy(harvest_date)
#   )

# Zone labels used only for facet strip text
zone_desc <- c("1" = "Transition", "2" = "Dune","3" = "Swale")  # others keep their ID
zone_labeller <- ggplot2::labeller(
  zone_id = function(z) {
    zc <- as.character(z)
    desc <- zone_desc[zc]
    ifelse(is.na(desc), zc, paste0(zc, " — ", desc))
  }
)


# --- minimal CRS checks (good) ---
stopifnot(st_crs(boundary)$epsg == 4326)
stopifnot(st_crs(trial.plan)$epsg == 4326)
stopifnot("treat_desc" %in% names(trial.plan))

# --- assemble named bundle ---
site.info <- list(
  site_id    = "Walpeup_MRS125",
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
