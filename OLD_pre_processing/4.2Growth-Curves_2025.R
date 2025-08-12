rm(list=ls())
library(sf)
library(terra)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
headDir <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}/work/Output-1/1.Walpeup_MRS125"

## Read in files
boundary <- st_read(paste0(headDir,'/1.Paddock_Boundary/Walpeup_MRS125_Boundary_Masked_4326.shp'))
strips <- st_read(paste0(headDir,"/5.Trial_Plan/FINAL-Trial-Plan/GIS/MRS125_Strips_FINAL_wgs84.shp"))
strips <- st_make_valid(strips)
zones <- rast(paste0(headDir,'/3.Covariates/6.Clusters_Zones/FINAL/MRS125_NEWZONES_round_wgs84.tif'))

############################################################################
##Read in Planet
planet_data.fls <- list.files(
  path = paste0(headDir, '/7.In_Season_data/25/2.Satellite_Imagery/Planet/Walpeup_old_upto_25June2025_RAW_psscene_analytic_8b_sr_udm2/PSScene'),
  pattern = "3B_AnalyticMS_SR_8b_clip.tif",
  full.names = TRUE
)

# Exclude any files that contain '.xml' in the name
planet_data.fls <- planet_data.fls[!grepl("\\.xml", planet_data.fls)]

planet_data <- lapply(planet_data.fls,rast)

NDVI <- function(raster_stack) {
  
  # Access the red and NIR bands from the raster stack
  red_band <- raster_stack[[6]]  # 6th band is red
  nir_band <- raster_stack[[8]]  # 8th band is NIR
  
  # Calculate NDVI
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



ggplot(dat.clean, aes(x = x, y = value, color = treat_desc, group = treat_desc)) +
  geom_smooth(method = "gam", span = 0.3, se = FALSE) +  # Smoothed curves
  labs(
    title = "NDVI Timeseries",
    x = "Days after planting",
    y = "Average NDVI",
    color = "Treatment"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(dat.clean, aes(x = x, y = value, color = treat_desc, group = treat_desc)) +
  # Control group (bold black line)
  geom_smooth(data = subset(dat.clean, treat_desc == "Control"), 
              method = "gam", span = 0.3, se = FALSE, 
              color = "black", size = 1.5) +  
  # Other treatments (excluding Control)
  geom_smooth(data = subset(dat.clean, treat_desc != "Control"), 
              method = "gam", span = 0.3, se = FALSE) +  
  # Labels
  labs(
    title = "NDVI Timeseries",
    x = "Days after planting",
    y = "Average NDVI",
    color = "Treatment"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

folder.name <- format(Sys.Date(), "%Y%m%d")
#
#saveDir <- paste0(headDir,'/10.Analysis/24/InSeason/Growth_Curves/',folder.name)
saveDir <- paste0('C:/Users/ouz001/working_from_home_post_Sep2022/sandy_soils_output1_app/pre_processing/10.Analysis/24/InSeason/Growth_Curves/'
                  ,folder.name)

if (!dir.exists(saveDir)) {dir.create(saveDir, recursive = TRUE)}

## Write Data
saveRDS(dat.clean,paste0(saveDir,'/growth_curve_dat.rds'))
saveRDS(ndvi.images,paste0(saveDir,'/growth_curve_rasters.rds'))
