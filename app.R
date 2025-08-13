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

growth_curve_data_site1_yr1 <- 
  read_csv(
    paste0(location_files_for_app,site_1,"/", yr1, "/", "ndvi_growth_curves_", yr1, ".csv" )) %>% 
  dplyr::mutate (site = site_1,
                 year = yr1)

growth_curve_data_site1_yr2 <- 
  read_csv(
    paste0(location_files_for_app,site_1,"/", yr2, "/", "ndvi_growth_curves_", yr2, ".csv" )) %>% 
  dplyr::mutate (site = site_1,
                 year = yr2)


growth_curve_data_site2_yr1 <- 
  read_csv(
    paste0(location_files_for_app,site_2,"/", yr1, "/", "ndvi_growth_curves_", yr1, ".csv" )) %>% 
  dplyr::mutate (site = site_2,
                 year = yr1)

growth_curve_data_site1_yr2 <- 
  read_csv(
    paste0(location_files_for_app,site_2,"/", yr2, "/", "ndvi_growth_curves_", yr2, ".csv" )) %>% 
  dplyr::mutate (site = site_2,
                 year = yr2)

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
                               selectInput("soil_layer_site1", "Select Soil Layer to Plot", choices = names(site1_soil.rast))),
                           box(width = 4,
                               selectInput("ndvi_date_site1", "Select NDVI Date", choices = names(NDVI_yr2_site1))),
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
                           box(width = 12, plotlyOutput("ndvi_curve_plot_site1_yr2"))
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
                               selectInput("soil_layer_site2", "Select Soil Layer to Plot", choices = names(site2_soil.rast))),
                           box(width = 4,
                               selectInput("ndvi_date_site2", "Select NDVI Date", choices = names(NDVI_yr2_site2))),
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
                           box(width = 12, plotlyOutput("ndvi_curve_plot_site2_yr2"))
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
  


################################################################################
### Reactive code 
################################################################################
## reactive for site 1 NDVI
  observe({
    req(input$ndvi_date_site1)
    selected_raster <- NDVI_yr2_site1[[input$ndvi_date_site1]]
    leaflet_raster_site1 <- raster(selected_raster)  # terra → raster for leaflet

    # Determine scale type
    if (input$ndvi_scale_type == "fixed") {
      pal <- colorNumeric(palette = "YlGn", domain = c(0, 1), na.color = "transparent")
      legend_vals <- c(0, 1)
    } else {
      raster_vals <- values(leaflet_raster_site1)
      pal <- colorNumeric(palette = "YlGn", domain = raster_vals, na.color = "transparent")
      legend_vals <- raster_vals
    }

    leafletProxy("map_site1") %>%
      clearImages() %>%
      clearControls() %>%
      addRasterImage(leaflet_raster_site1, colors = pal, opacity = 0.6, project = TRUE) %>%
      addLegend(pal = pal, values = legend_vals, title = "NDVI", labFormat = labelFormat())
  })
  
## reactive for site 1 Soil layer
  observe({
    req(input$soil_layer_site1)
    selected_soil <- site1_soil.rast[[input$soil_layer_site1]]
    soil_raster_site1 <- raster(selected_soil)

    # Create color palette for soil layer
    soil_pal <- colorNumeric(palette = "viridis", domain = values(soil_raster_site1), na.color = "transparent")

    leafletProxy("map_site1") %>%
      clearGroup("Soil Layer") %>%
      clearControls() %>%
      addRasterImage(soil_raster_site1, colors = soil_pal, opacity = 0.7, project = TRUE, group = "Soil Layer") %>%
      addLegend(pal = soil_pal, values = values(soil_raster_site1), title = input$soil_layer_site1,
                position = "bottomleft", group = "Soil Layer")
  })

  
################################################################################ 
## reactive for site 2 NDVI
  observe({
    req(input$ndvi_date_site2)
    selected_raster <- NDVI_yr2_site2[[input$ndvi_date_site2]]
    leaflet_raster_site2 <- raster(selected_raster)  # terra → raster for leaflet
    
    # Determine scale type
    if (input$ndvi_scale_type == "fixed") {
      pal <- colorNumeric(palette = "YlGn", domain = c(0, 1), na.color = "transparent")
      legend_vals <- c(0, 1)
    } else {
      raster_vals <- values(leaflet_raster_site2)
      pal <- colorNumeric(palette = "YlGn", domain = raster_vals, na.color = "transparent")
      legend_vals <- raster_vals
    }
    
    leafletProxy("map_site2") %>%
      clearImages() %>%
      clearControls() %>%
      addRasterImage(leaflet_raster_site2, colors = pal, opacity = 0.6, project = TRUE) %>%
      addLegend(pal = pal, values = legend_vals, title = "NDVI", labFormat = labelFormat())
  })
  
  ## reactive for site 2 Soil layer
  observe({
    req(input$soil_layer_site2)
    selected_soil <- site2_soil.rast[[input$soil_layer_site2]]
    soil_raster_site2 <- raster(selected_soil)
    
    # Create color palette for soil layer
    soil_pal <- colorNumeric(palette = "viridis", domain = values(soil_raster_site2), na.color = "transparent")
    
    leafletProxy("map_site2") %>%
      clearGroup("Soil Layer") %>%
      clearControls() %>%
      addRasterImage(soil_raster_site2, colors = soil_pal, opacity = 0.7, project = TRUE, group = "Soil Layer") %>%
      addLegend(pal = soil_pal, values = values(soil_raster_site2), title = input$soil_layer_site1,
                position = "bottomleft", group = "Soil Layer")
  })
 
################################################################################ 
## Growth curves
################################################################################
####for site 1 year 2   
output$ndvi_curve_plot_site1_yr2 <- renderPlotly({
dat.clean <- growth_curve_data_site1_yr2  


p <- ggplot(dat.clean, aes(x = dap, y = ndvi, color = treat_desc, group = treat_desc)) +
  # Bold black control line
  geom_smooth(data = subset(dat.clean, treat_desc == "Control"),
              method = "gam", span = 0.3, se = FALSE,
              color = "black", size = 1.5) +
  # Other treatments
  geom_smooth(data = subset(dat.clean, treat_desc != "Control"),
              method = "gam", span = 0.3, se = FALSE) +
  labs(
    title = "NDVI Timeseries (2025)",
    x = "Days after planting",
    y = "Average NDVI",
    color = "Treatment"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(p)  
}) 
  

  
####for site 2 year 2   
  output$ndvi_curve_plot_site2_yr2 <- renderPlotly({
    dat.clean <- growth_curve_data_site1_yr2  
    
    
    p <- ggplot(dat.clean, aes(x = dap, y = ndvi, color = treat_desc, group = treat_desc)) +
      # Bold black control line
      geom_smooth(data = subset(dat.clean, treat_desc == "Control"),
                  method = "gam", span = 0.3, se = FALSE,
                  color = "black", size = 1.5) +
      # Other treatments
      geom_smooth(data = subset(dat.clean, treat_desc != "Control"),
                  method = "gam", span = 0.3, se = FALSE) +
      labs(
        title = "NDVI Timeseries (2025)",
        x = "Days after planting",
        y = "Average NDVI",
        color = "Treatment"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    ggplotly(p)  
  }) 
    
 } #end of server


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
