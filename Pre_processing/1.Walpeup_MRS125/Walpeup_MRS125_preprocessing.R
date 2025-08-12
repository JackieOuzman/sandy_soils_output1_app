rm(list=ls())
#The purpose of this script is to pre-process all data required for The Site Viewer RShiny
# NOTE: THE CRS OF ALL LAYERS MUST BE EPSG 4326 (WGS84)
library(terra)
library(sf)
library(dplyr)

readDir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}/work/Output-1/1.Walpeup_MRS125"
saveDir <- "//fs1-cbr.nexus.csiro.au/{lw-soildatarepo}/work/Shiny/Apps/Stirling/GRDCSandySoilsII/Output1Viewer/Current/Files/Walpeup_MRS125"

#Data to compile:
#1) Paddock maps (zones and soil, as .tifs)
#2) Paddock info (trial plan, boundary, crop types and plant dates for each season)
#3) Planet satellite imagery
#4) Growth curve summary data - computed from satellite imagery

################################################################################
######################## 1) Read in Paddock maps ###############################
################################################################################

soil <- rast(paste0(readDir,"/9.Maps/Soil/keymaps/keysoil.tif"))
names(soil) <- c("Surface-pH","Subsoil-Clay%","Repellence","DepthToFiz","DepthToClay","DepthToNeutral")
zones <- rast(paste0(readDir,"/3.Covariates/6.Clusters_Zones/FINAL/MRS125_Zones_round_wgs84_smooth.tif"))

################################################################################
######################## 2) Paddock Information  ###############################
################################################################################

boundary <- st_read(paste0(readDir,"/1.Paddock_Boundary/Walpeup_MRS125_Boundary_Masked_4326.shp"))
trial.plan <- st_read(paste0(readDir,'/5.Trial_Plan/FINAL-Trial-Plan/GIS/MRS125_PlotStrips_FINAL_wgs84.shp'))
## Note: treat_desc must be in trial.plan

crop.types <- list("Lentils","Wheat","TBC","TBC")
names(crop.types) <- c("2024","2025","2026","2027")

plant.dates <- list("29/05/2024","29/05/2024",NA,NA)
names(plant.dates) <- c("2024","2025","2026","2027")

harvest.dates <- list(NA,NA,NA,NA)
names(harvest.dates) <- c("2024","2025","2026","2027")

################################################################################
##################### 3) Planet satellite imagery  #############################
################################################################################
seasons <- c("24","25")

for (i in 1:length(seasons)){
  season.i <- seasons[i]
  
  ##Read in Planet
  planet_data.fls <- list.files(
    path = paste0(readDir, '/7.In_Season_data/',season.i,'/2.Satellite_Imagery/Planet/PSScene'),
    pattern = "3B_AnalyticMS_SR_8b_clip.tif",
    full.names = TRUE
  )
  
  # Exclude any files that contain '.xml' in the name
  planet_data.fls <- planet_data.fls[!grepl("\\.xml", planet_data.fls)]
  planet_data <- lapply(planet_data.fls,rast)
  
  NDVI <- function(raster_stack) {
    red_band <- raster_stack[[6]]  # 6th band is red
    nir_band <- raster_stack[[8]]  # 8th band is NIR
    
    ndvi <- (nir_band - red_band) / (nir_band + red_band)
    return(ndvi)
  }
  
  Walpaup_NDVI.1<-lapply(planet_data, NDVI)
  
  template_raster <- Walpaup_NDVI.1[[1]]
  
  Walpaup_NDVI <- Walpaup_NDVI.1
  for (i in 2:length(Walpaup_NDVI.1)){
    Walpaup_NDVI[[i]] <- terra::project(Walpaup_NDVI.1[[i]],template_raster)
    print(i)
  }
  
  satellite_data.rast <- rast(Walpaup_NDVI)
  satellite_data.rast.4326 <- terra::project(satellite_data.rast,'epsg:4326')
  
  ##Get names as dates
  date.strings.planet <- str_extract(basename(planet_data.fls), "^\\d{8}")
  print(date.strings.planet)
  
  ################################################################################
  ndvi.4326.all <- satellite_data.rast.4326
  
  plot(ndvi.4326.all)
  rm.lyrs <- NULL #c(1:12,14) #OR NULL     #manually remove images from a long time before planting
  if(length(rm.lyrs)==0){
    ndvi.4326 <- ndvi.4326.all
    dates.dts.planet <- as.Date(date.strings.planet, format="%Y%m%d")
    
  }else{
    ndvi.4326 <- ndvi.4326.all[[-rm.lyrs]]
    dates.dts.planet <- as.Date(date.strings.planet[-rm.lyrs], format="%Y%m%d")
    
  }
  
  plant.date <- "20250529"
  plant.date.dts <- as.Date(plant.date, format="%Y%m%d")
  
  #Calculate the no.days since planting
  difference <- as.numeric(difftime(dates.dts.planet, plant.date.dts, units="days"))
  #difference <- as.numeric(difftime(dates.dts.planet, plant.date.dts, units="days"))
  
  # Print the difference
  print(difference)
  
  ndvi.images <- ndvi.4326
  names(ndvi.images) <- dates.dts.planet
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ## Create Growth Curves for treatments (not split by zone)
  rasters <- ndvi.4326
  #rasters <- planet_data.rast.4326
  polygons <- strips
  
  # Initialize an empty matrix to store results
  result_matrix <- matrix(NA, nrow = nlyr(rasters), ncol = nrow(polygons))
  
  # Loop through each layer of the SpatRaster
  for (i in 1:nlyr(rasters)) {
    # Extract the i-th layer
    layer <- rasters[[i]]
    
    # Calculate the average value for each polygon
    avg_values <- terra::extract(layer, polygons, fun = mean, na.rm = TRUE)
    
    # Store the results in the matrix
    result_matrix[i, ] <- avg_values[,2]
    print(i)
  }
  
  # Set row and column names
  rownames(result_matrix) <- difference
  colnames(result_matrix) <- polygons$treat_desc
  result_df <- as.data.frame(result_matrix)
  mean(as.numeric(result_df[1,]))
  
  # Convert the result data frame into a long format
  result_long <- pivot_longer(result_df, cols = everything(), names_to = "treat_desc", values_to = "value")
  # Example x-axis vector
  x_vector <- difference
  result_long$x <- rep(x_vector, each = ncol(result_df))
  
  ## Create ggplot
  dat <- result_long
  buffer.idx <- which(dat$treat_desc %in% c("Buffer", "Outside Control"))
  dat.clean <- dat[-buffer.idx,]
  
  #dat.clean <- dat
  
  ggplot(dat.clean, aes(x = x, y = value, color = treat_desc, group = treat_desc)) +
    geom_line() +
    labs(title = "NDVI Timeseries",
         x = "Days after planting", y = "Average NDVI", color = "Treatment") +
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5))
  
  
  
  
}

