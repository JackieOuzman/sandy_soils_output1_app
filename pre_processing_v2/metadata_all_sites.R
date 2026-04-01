
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
# site <- "1.Walpeup_MRS125"
# site_number <- "1.Walpeup_MRS125"
# site_name <- "Walpeup_MRS125"

# site <-"2.Crystal_Brook_Brians_House"
# site_number <-"2.Crystal_Brook_Brians_House"
# site_name <-  "Crystal_Brook_Brians_House"

site <- "3.Wynarka_Mervs_West"
site_number <- "3.Wynarka_Mervs_West"
site_name <- "Wynarka_Mervs_West"

# site <- "4.Wharminda"
# site_number <- "4.Wharminda"
# site_name <- "Wharminda"

# site <- "5.Walpeup_Gums"
# site_number <- "5.Walpeup_Gums"
# site_name <- "Walpeup_Gums"

# site <- "6.Crystal_Brook_Randals"
# site_number <- "6.Crystal_Brook_Randals"
# site_name <- "Crystal_Brook_Randals"
# ====================== PATHS ======================
readDir <- paste0("//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}/work/Output-1/", site)

dir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
headDir <- paste0(dir, "/work/Output-1/", site_number,"/7.In_Season_data/25/7.Growth_curves")
saveDir <- headDir

metadata_path <- paste0(dir,"/work/Output-1/0.Site-info/")
metadata_file_name <- "names of treatments per site 2025 metadata and other info.xlsx"

## JACKIE - Change the saved directory

# saveDir <- paste0("C:/Users/ouz001/working_from_home_post_Sep2022/sandy_soils_output1_app/Pre_processing_v2/",
#                   site, "/preprocessing_output")
#saveDir <- paste0("//fs1-cbr.nexus.csiro.au/{lw-soildatarepo}/work/Shiny/Apps/Stirling/GRDCSandySoilsII/Output1Viewer/Current/Files/",site_1)



################################################################################
######################## 1) Read in metadata info file names and path ##########
################################################################################

file_path_details <- readxl::read_excel(
  paste0(metadata_path,"names of treatments per site 2025 metadata and other info.xlsx"),
  sheet = "file location etc") %>% 
  filter(Site == site)

seasons <- readxl::read_excel(
  paste0(metadata_path,"names of treatments per site 2025 metadata and other info.xlsx"),
  sheet = "seasons") %>% 
  filter(Site == site) %>% 
  filter(Year == 2025)


soil_path <- file_path_details %>%  
  filter(variable == "location of key soil tif") %>% pull("file path")
zone_shapefile_path <- 
  file_path_details %>%  
  filter(variable == "location of zone shp") %>% pull("file path")
zone_raster_path <- file_path_details %>%  
  filter(variable == "location of zone tif") %>% pull("file path")
bounary_shapefile_path <- file_path_details %>%  
  filter(variable == "boundary") %>% pull("file path")
trial.plan_shapefile_path <- file_path_details %>%  
  filter(variable == "trial.plan") %>% pull("file path")


################################################################################
######################## 1) Read in Paddock maps and info ######################
################################################################################


soil <- rast(file.path(readDir, soil_path))
zones <- rast(file.path(readDir, zone_raster_path))

zones.sf <- st_read(paste0(readDir,zone_shapefile_path))
boundary   <- suppressMessages(st_read(file.path(readDir, bounary_shapefile_path), quiet = TRUE))
trial.plan <- suppressMessages(st_read(file.path(readDir, trial.plan_shapefile_path), quiet = TRUE))

################################################################################
######################## 2) Paddock Information  ###############################
################################################################################

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
  stopifnot(st_crs(x$boundary)$epsg == 7854, st_crs(x$trial_plan)$epsg == 7854)
  stopifnot(is.double(x$seasons$Year))
  invisible(x)
}
validate_site(site.info)

# --- save ---
saveRDS(site.info, file.path(saveDir, "site_info.rds"))


