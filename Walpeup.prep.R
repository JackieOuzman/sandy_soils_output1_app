rm(list=ls())
library(terra)
library(sf)


### Pre processing data for the site viewer app ################################


### Background #################################################################
#This is Stirling script which Jackie has hacked at 



### Site details ###############################################################
site_name <- "1.Walpeup_MRS125"
year <- "24"
plant.date <- "29/05/2024"
crop.type <- "Lentils"

## Location of input and saved data ############################################
headDir <- paste0("//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}/work/Output-1/", site_name)

Growth_Curves <- paste0(headDir,"/10.Analysis/", year,"/InSeason/Growth_Curves/")

saveDir <- paste0("//fs1-cbr.nexus.csiro.au/{lw-soildatarepo}/work/Shiny/Apps/Stirling/GRDCSandySoilsII/Output1Viewer/Current/Pre_processing/",site_name)



## Import boundary, trial plan and zones #######################################
boundary <- st_read(paste0(headDir,"/1.Paddock_Boundary/Walpeup_MRS125_Boundary_Masked_4326.shp"))
trial.plan <- st_read(paste0(headDir,'/5.Trial_Plan/FINAL-Trial-Plan/GIS/MRS125_PlotStrips_FINAL_wgs84.shp'))
zones <- rast(paste0(headDir,"/3.Covariates/6.Clusters_Zones/FINAL/MRS125_Zones_round_wgs84_smooth.tif"))


# Stirling I need help with this - do we need it and where is it made ##########
#soil <- rast("//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}/work/Output-1/SiteViewer/Files/Walpeup/keysoil.tif")
#names(soil) <- c("Surface-pH","Subsoil-Clay%","Repellence","DepthToFiz","DepthToClay","DepthToNeutral")





## Read in Growth Curve data - 1) ndvi imagery, 2) structured data - av. ndvi by treat for each date. 
folders_GC_dates <- list.dirs(paste0(Growth_Curves), full.names = FALSE, recursive = FALSE)
folders_GC_dates
folders_GC_dates <- folders_GC_dates[grepl("^\\d{8}$", folders_GC_dates)]

# get the most recent date and folder
latest_GC_dates <- folders_GC_dates[which.max(as.integer(folders_GC_dates))]

# Stirling I need help with this - where is this made ? Also is it ok does it need an x and y clm?
growth.curves.ndvi <- readRDS(paste0(Growth_Curves,latest_GC_dates, "/growth_curve_dat.rds"))


# rds_path_plot_dat <- file.path(paste0(headDir,'/10.Analysis/24/InSeason/Growth_Curves/'), latest_folder, "growth_curve_dat.rds")
# growth.curves.dat <- readRDS(rds_path_plot_dat)

# emergence.dat <- read.csv(paste0(headDir,'/7.In_Season_data/24/1.Emergence/emergence_rStructered.csv'))
# biomass.dat <- read.csv(paste0(headDir,'/7.In_Season_data/24/4.Biomass/modelled_biomass_rStructered.csv'))
# harvest.dat <- c()



# Stirling is this just converting to a raster with different geometry? - need more info for this step
# Stirling also comment above a regarding soil - do we need this how is it made?

## Need to save rasters seperately from other objects, so they load properly
#soil.res <- terra::resample(soil,growth.curves.ndvi)

zones.res <- terra::resample(zones,growth.curves.ndvi,method='near') 
  
#writeRaster(soil.res,paste0(saveDir,'/Files/Walpeup/Walpeup_soil.tif'),overwrite=T)
writeRaster(zones.res,paste0(saveDir,'/Files/Walpeup/Walpeup_zones.tif'),overwrite=T)
writeRaster(growth.curves.ndvi,paste0(saveDir,'/Files/Walpeup/Walpeup_ndvi.tif'),overwrite=T)

all.dat <- list(boundary,trial.plan,growth.curves.dat,plant.date,crop.type)

names(all.dat) <- c("boundary","trial plan","growth curve data","plant date","crop type")

saveRDS(all.dat,paste0(saveDir,'/Files/Walpeup/Walpeup_dat.rds'))




#boundary, trial plan, zones/soil stack, imagery raster stack, curve ggplot data (x2 - all and zones)
#Build a list
