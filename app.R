rm(list = ls())

# Load libraries
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

#location_files_for_app <- 'B:/Shiny/Apps/Stirling/GRDCSandySoilsII/Output1Viewer/Current/Files/'  
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
### UI #####
###############################################################################
ui <- dashboardPage(
  dashboardHeader(title = "SSII Output 1 Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Walpeup MRS125", 
               tabName = "Walpeup_MRS125", 
               icon = icon("map")),
      menuItem("Crystal Brook Brians House", 
               tabName = "Crystal_Brook_Brians_House", 
               icon = icon("map"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content - Walpeup MRS125
      tabItem(tabName = "Walpeup_MRS125",
              tags$h2("Walpeup MRS125", style = "text-align:center; padding-top:10px; padding-bottom:14px;"),
              tabBox(
                id = "season_tabs_walpeup", width = 12,
                tabPanel("All",
                         fluidRow(
                           box(width = 12, leafletOutput("map_site1", height = 400))
                         ),
                         fluidRow(
                           box(width = 4,
                               selectInput("soil_layer", "Select Soil Layer to Plot", choices = names(site1_soil.rast))),
                           box(width = 4,
                               selectInput("ndvi_date", "Select NDVI Date", choices = names(NDVI_yr2_site1))),
                           box(width = 4,
                               radioButtons("ndvi_scale_type", "NDVI Scale:",
                                            choices = c("Fixed (0–1)" = "fixed",
                                                        "Dynamic (per image)" = "dynamic"),
                                            selected = "fixed"))
                         ),
                         fluidRow(
                           valueBoxOutput("crop_type"),
                           valueBoxOutput("plant_date")
                         ),
                         fluidRow(
                           box(width = 6, plotlyOutput("ndvi_curve_plot"))
                         )
              
                
                )
              )
      ),
      
      # Second tab content - Crystal Brook Brians House
      tabItem(tabName = "Crystal_Brook_Brians_House",
              tags$h2("Crystal Brook Brians House", style = "text-align:center; padding-top:10px; padding-bottom:14px;"),
              tabBox(
                id = "season_tabs_crystal", width = 12,
                tabPanel("All",
                         fluidRow(
                           box(width = 12, leafletOutput("map_site2", height = 400))
                         ),
                         fluidRow(
                           box(width = 4,
                               selectInput("soil_layer", "Select Soil Layer to Plot", choices = names(site2_soil.rast))),
                           box(width = 4,
                               selectInput("ndvi_date", "Select NDVI Date", choices = names(NDVI_yr2_site2))),
                           box(width = 4,
                               radioButtons("ndvi_scale_type", "NDVI Scale:",
                                            choices = c("Fixed (0–1)" = "fixed",
                                                        "Dynamic (per image)" = "dynamic"),
                                            selected = "fixed"))
                         ),
                         fluidRow(
                           valueBoxOutput("crop_type"),
                           valueBoxOutput("plant_date")
                         ),
                         fluidRow(
                           box(width = 6, plotlyOutput("ndvi_curve_plot"))
                         )
                ),
                
                
              )
      )
    )
  )
)

###############################################################################
### Server #####
############################################################################### 
 server <- function(input, output, session) {

### map for site 1
  output$map_site1 <- renderLeaflet({
    bbox <- st_bbox(site.data_site1$boundary)
    xmin <- as.numeric(bbox["xmin"])
    xmax <- as.numeric(bbox["xmax"])
    ymin <- as.numeric(bbox["ymin"])
    ymax <- as.numeric(bbox["ymax"])

    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addPolygons(data = site.data_site1$boundary, color = "blue", weight = 2, fill = FALSE, group = "Boundary") %>%
      addPolygons(data = site.data_site1$trial_plan,
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
  })
  
  
  ### map for site 2  
  
  output$map_site2 <- renderLeaflet({
    bbox <- st_bbox(site.data_site2$boundary)
    xmin <- as.numeric(bbox["xmin"])
    xmax <- as.numeric(bbox["xmax"])
    ymin <- as.numeric(bbox["ymin"])
    ymax <- as.numeric(bbox["ymax"])
    
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addPolygons(data = site.data_site2$boundary, color = "blue", weight = 2, fill = FALSE, group = "Boundary") %>%
      addPolygons(data = site.data_site2$trial_plan,
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
  })
  
 } #end of server




  # observe({
  #   req(input$ndvi_date)
  #   selected_raster <- NDVI_yr2[[input$ndvi_date]]
  #   leaflet_raster <- raster(selected_raster)  # terra → raster for leaflet
  # 
  #   # Determine scale type
  #   if (input$ndvi_scale_type == "fixed") {
  #     pal <- colorNumeric(palette = "YlGn", domain = c(0, 1), na.color = "transparent")
  #     legend_vals <- c(0, 1)
  #   } else {
  #     raster_vals <- values(leaflet_raster)
  #     pal <- colorNumeric(palette = "YlGn", domain = raster_vals, na.color = "transparent")
  #     legend_vals <- raster_vals
  #   }
  # 
  #   leafletProxy("map") %>%
  #     clearImages() %>%
  #     clearControls() %>%
  #     addRasterImage(leaflet_raster, colors = pal, opacity = 0.6, project = TRUE) %>%
  #     addLegend(pal = pal, values = legend_vals, title = "NDVI", labFormat = labelFormat())
  # })
  # 
  # observe({
  #   req(input$soil_layer)
  #   selected_soil <- site1_soil.rast[[input$soil_layer]]
  #   soil_raster <- raster(selected_soil)
  # 
  #   # Create color palette for soil layer
  #   soil_pal <- colorNumeric(palette = "viridis", domain = values(soil_raster), na.color = "transparent")
  # 
  #   leafletProxy("map") %>%
  #     clearGroup("Soil Layer") %>%
  #     clearControls() %>%
  #     addRasterImage(soil_raster, colors = soil_pal, opacity = 0.7, project = TRUE, group = "Soil Layer") %>%
  #     addLegend(pal = soil_pal, values = values(soil_raster), title = input$soil_layer,
  #               position = "bottomleft", group = "Soil Layer")
  # })
  # 
 
#   
#   
#   
#   output$ndvi_curve_plot <- renderPlotly({
#     dat.clean <- site.data$`growth curve data`  # Replace with your actual tibble name
#     
#     p <- ggplot(dat.clean, aes(x = x, y = value, color = treat_desc, group = treat_desc)) +
#       # Bold black control line
#       geom_smooth(data = subset(dat.clean, treat_desc == "Control"), 
#                   method = "gam", span = 0.3, se = FALSE, 
#                   color = "black", size = 1.5) +  
#       # Other treatments
#       geom_smooth(data = subset(dat.clean, treat_desc != "Control"), 
#                   method = "gam", span = 0.3, se = FALSE) +  
#       labs(
#         title = "NDVI Timeseries",
#         x = "Days after planting",
#         y = "Average NDVI",
#         color = "Treatment"
#       ) +
#       theme_minimal() +
#       theme(plot.title = element_text(hjust = 0.5))
#     
#     ggplotly(p)
#   })
#   
#   
#   output$crop_type <- renderValueBox({
#     valueBox(
#       value = site.data$`crop type`,
#       subtitle = "Crop Type",
#       icon = icon("seedling"),
#       color = "green"
#     )
#   })
#   
#   output$plant_date <- renderValueBox({
#     valueBox(
#       value = site.data$`plant date`,
#       subtitle = "Plant Date",
#       icon = icon("calendar-alt"),
#       color = "blue"
#     )
#   })
#   
#   
#   


# Run the app
shinyApp(ui, server)
