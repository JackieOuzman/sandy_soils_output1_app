
### This script create metadata rds and collection of files that will be used later.
### It uses a excel file which has the file location and names (they are not the same so I cant make a generic script)
### I have removed a fe things which I might need later?
## These are the names of the soil grids 
## Checking the coordinate system, this should be the same for all sites but its not:(

## also review the directory so the output sit on Ross's server (not my computer)


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

# ====================== Sites ======================
#site <- "1.Walpeup_MRS125"
#site <- "2.Crystal_Brook_Brians_House"
# site <- "3.Wynarka_Mervs_West"
# site <- "4.Wharminda"
# site <- "5.Walpeup_Gums"
 site <- "6.Crystal_Brook_Randals"

# ====================== PATHS ======================
readDir <- paste0("//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}/work/Output-1/", site)
metadata_path <- "//fs1-cbr.nexus.csiro.au/{lw-soildatarepo}/work/Shiny/Apps/Stirling/GRDCSandySoilsII/Output1Viewer/"

## JACKIE - Change the saved directory

saveDir <- paste0("C:/Users/ouz001/working_from_home_post_Sep2022/sandy_soils_output1_app/Pre_processing_v2/",
                  site, "/preprocessing_output")
#saveDir <- paste0("//fs1-cbr.nexus.csiro.au/{lw-soildatarepo}/work/Shiny/Apps/Stirling/GRDCSandySoilsII/Output1Viewer/Current/Files/",site_1)

################################################################################
######################## 1) Read in metadata info file names and path ##########
################################################################################

file_path_details <- readxl::read_excel(
  paste0(metadata_path,"names of treatments per site 2025 metadata and other info.xlsx"),
  sheet = "location of file and details") %>% 
  filter(Site == site)

seasons <- readxl::read_excel(
  paste0(metadata_path,"names of treatments per site 2025 metadata and other info.xlsx"),
  sheet = "seasons") %>% 
  filter(Site == site)


################################################################################
######################## 1) Read in Paddock maps ###############################
################################################################################

soil <- rast(file.path(readDir, file_path_details$`location of key soil tif`))

#names(soil) <- c("Surface_pH","Subsoil_Clay_pct","Repellence","DepthToFizz","DepthToClay","DepthToNeutral")
## Not sure I need this??


zones.sf <- st_read(paste0(readDir,file_path_details$`location of zone shp`))
zones <- rast(file.path(readDir, file_path_details$`location of zone tif`))



################################################################################
######################## 2) Paddock Information  ###############################
################################################################################

boundary   <- suppressMessages(st_read(file.path(readDir, file_path_details$boundary), quiet = TRUE))
trial.plan <- suppressMessages(st_read(file.path(readDir, file_path_details$trial.plan), quiet = TRUE))



# --- minimal CRS checks (good) ---
# stopifnot(st_crs(boundary)$epsg == 4326)
# stopifnot(st_crs(trial.plan)$epsg == 4326)
# stopifnot("treat_desc" %in% names(trial.plan))

# --- assemble named bundle ---
site.info <- list(
  site_id    = site,
  boundary   = boundary,      # sf
  trial_plan = trial.plan,    # sf
  seasons    = seasons,        # tibble
  zones_sf = zones.sf,         # shapefile
  zones =     zones           #raster /tif
)
class(site.info) <- c("ssii_site", class(site.info))

# --- optional: quick validator/helper ---
validate_site <- function(x) {
  stopifnot(inherits(x, "ssii_site"))
  stopifnot(all(c("site_id","boundary","trial_plan","seasons", "zones", "zones_sf") %in% names(x)))
  stopifnot(st_crs(x$boundary)$epsg == 4326, st_crs(x$trial_plan)$epsg == 4326)
  stopifnot(is.double(x$seasons$year))
  invisible(x)
}
validate_site(site.info)

# --- save ---
saveRDS(site.info, file.path(saveDir, "site_info.rds"))

