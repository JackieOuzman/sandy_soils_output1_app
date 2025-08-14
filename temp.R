
library(shiny)
library(shinydashboard)
#library(htmltools)
library(leaflet)
library(plotly)
library(readr)
library(dplyr)
library(terra)
library(raster)  # Needed for rendering raster in leaflet
library(sf)

###############################################################################
#### Central location of the files the app will draw on - id the directory here ####
###############################################################################

node_name <- Sys.info()["nodename"]

if (node_name=="TOPAZ-GW" ) {
  location_files_for_app <- 'B:/Shiny/Apps/Stirling/GRDCSandySoilsII/Output1Viewer/Current/Files/'
  
} else {
  location_files_for_app <- "/datasets/work/lw-soildatarepo/work/Shiny/Apps/Stirling/GRDCSandySoilsII/Output1Viewer/Current/Files/"} # I am not sure this will work?

###############################################################################
#### Site details #####
###############################################################################
site_1 <-"1.Walpeup_MRS125"
site_2 <-"2.Crystal_Brook_Brians_House"
yr1 <- "2024"
yr2 <- "2025"

###############################################################################
#### import files #####
###############################################################################
site1_soil.rast <- rast(paste0(location_files_for_app,site_1,"/","soil.tif"))
site1_zones.rast <- rast(paste0(location_files_for_app,site_1,"/","zones.tif"))

site2_soil.rast <- rast(paste0(location_files_for_app,site_2,"/","soil.tif"))
site2_zones.rast <- rast(paste0(location_files_for_app,site_2,"/","zones.tif"))

###############################################################################
NDVI_most_recent_site1 <- readRDS(paste0(location_files_for_app,site_1,"/", yr2, "/", "ndvi_stack_", yr2, ".rds" ))

NDVI_yr1_site1 <- subset(NDVI_most_recent_site1, grep(yr1, names(NDVI_most_recent_site1))) #filter by a pattern in layer names
NDVI_yr2_site1 <- subset(NDVI_most_recent_site1, grep(yr2, names(NDVI_most_recent_site1))) #filter by a pattern in layer names


NDVI_most_recent_site2 <- readRDS(paste0(location_files_for_app,site_2,"/", yr2, "/", "ndvi_stack_", yr2, ".rds" ))

NDVI_yr1_site2 <- subset(NDVI_most_recent_site2, grep(yr1, names(NDVI_most_recent_site2))) #filter by a pattern in layer names
NDVI_yr2_site2 <- subset(NDVI_most_recent_site2, grep(yr2, names(NDVI_most_recent_site2))) #filter by a pattern in layer names

###############################################################################

site.data_site1 <- readRDS(paste0(location_files_for_app,site_1,"/", "site_info.rds"))
site.data_site2 <- readRDS(paste0(location_files_for_app,site_2,"/", "site_info.rds"))

###############################################################################
#paste0(location_files_for_app,site_1,"/", yr2, "/", "ndvi_growth_curves_", yr2, ".csv" )
growth_curve_data_site1_yr2 <- 
  read_csv(
    paste0(location_files_for_app,site_1,"/", yr2, "/", "ndvi_growth_curves_", yr2, ".csv" )) %>% 
    dplyr::mutate (site = site_1,
                 year = yr2)

growth_curve_data_site1_yr2



#map is not rendering - why?

test_boundary <-  site.data_site1$boundary
test_boundary
plot(test_boundary)

test_trial_plan <-  site.data_site1$trial_plan
test_trial_plan
plot(test_trial_plan)
  
  output$map <- renderLeaflet({
    map <- Leaflet(
    bbox <- st_bbox(site.data_site1$boundary)
    xmin <- as.numeric(bbox["xmin"])
    xmax <- as.numeric(bbox["xmax"])
    ymin <- as.numeric(bbox["ymin"])
    ymax <- as.numeric(bbox["ymax"]))


    
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addPolygons(data = site.data_site1$boundary, color = "blue", weight = 2, fill = FALSE, group = "Boundary") %>%
      addPolygons(data = site.data_site1$`trial plan`,
                  color = "orange",
                  weight = 2,
                  fillOpacity = 0,
                  label = ~treat_desc,
                  group = "Treatments") %>%
      addLayersControl(
        overlayGroups = c("Boundary", "Treatments", "Soil Layer"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      fitBounds(lng1 = xmin, lat1 = ymin, lng2 = xmax, lat2 = ymax)

###############################################################################
str(growth_curve_data_site1_yr2)


    
      
        
        dat.clean <- growth_curve_data_site1_yr2  # Replace with your actual tibble name
        dat.clean
        str(dat.clean)
        
        p <- ggplot(dat.clean, aes(x = dap, y = ndvi, color = treat_desc, group = treat_desc)) +
          # Bold black control line
          geom_smooth(data = subset(dat.clean, grepl("control", treat_desc, ignore.case = TRUE)),,
                      method = "gam", span = 0.3, se = FALSE,
                      color = "black", size = 1.5) +
          # Other treatments
          geom_smooth(data = subset(dat.clean, !grepl("control", treat_desc, ignore.case = TRUE)),
                      method = "gam", span = 0.3, se = FALSE) +
          labs(
            title = "NDVI Timeseries",
            subtitle = "test",
            #subtitle = paste0("Site: ", dat.clean$site, "Year: ",  dat.clean$year ),
            x = "Days after planting",
            y = "Average NDVI",
            color = "Treatment"
          ) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5))
        p

        ggplotly(p)
      #})

        
        
###############################################################################
str(site.data_site1)        


        
site.data_site1_df <- as.data.frame(site.data_site1$seasons)        
site.data_site1__yr2_df <- 
  dplyr::filter(site.data_site1_df, year == 2025)         
        
          output$crop_type <- renderValueBox({
            valueBox(
              value = site.data_site1__yr2_df$crop_type,
              subtitle = "Crop Type 2025",
              icon = icon("seedling"),
              color = "green"
            )
          })

          output$plant_date <- renderValueBox({
            valueBox(
              value = site.data_site1__yr2_df$plant_date,
              subtitle = "Plant Date 2025",
              icon = icon("calendar-alt"),
              color = "blue"
            )