# Load libraries
library(shiny)
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

# if (node_name=="TOPAZ-GW" ) {
#   location_files_for_app <- 'B:/Shiny/Apps/Stirling/GRDCSandySoilsII/Output1Viewer/Current/Files/'
# } else {
#   location_files_for_app <- "/datasets/work/lw-soildatarepo/work/Shiny/Apps/Stirling/GRDCSandySoilsII/Output1Viewer/Current/Files/"
# }

location_files_for_app <- "/datasets/work/lw-soildatarepo/work/Shiny/Apps/Stirling/GRDCSandySoilsII/Output1Viewer/Current/Files/"

###############################################################################
#### Site details - define all sites here ####
###############################################################################
# Named list of all sites
sites <- list(
  "site1" = "1.Walpeup_MRS125",
  "site2" = "2.Crystal_Brook_Brians_House",
  "site3" = "3.Wynarka_Mervs_West",
  "site4" = "4.Wharminda",
  "site5" = "5.Walpeup_Gums",
  "site6" = "6.CrystalBrook_Randals"
)

yr1 <- "2024"
yr2 <- "2025"

###############################################################################
### UI #####
###############################################################################
ui <- fluidPage(
  titlePanel("SSII Output 1 Dashboard"),
  
  # Site selection
  fluidRow(
    column(12,
           selectInput("site_select", "Select Site:",
                       choices = c("Walpeup MRS125" = "site1",
                                   "Crystal Brook Brians House" = "site2",
                                   "Wynarka Mervs West" = "site3",
                                   "Wharminda" = "site4",
                                   "Walpeup Gums" = "site5",
                                   "Crystal Brook Randals" = "site6"),
                       selected = "site1")
    )
  ),
  
  # Loading indicator
  fluidRow(
    column(12,
           conditionalPanel(
             condition = "$('html').hasClass('shiny-busy')",
             div("Loading data...", style = "color: blue; font-weight: bold;")
           )
    )
  ),
  
  # Site info boxes
  fluidRow(
    column(3,
           wellPanel(
             h4("2024 Info"),
             textOutput("crop_type_yr1"),
             br(),
             textOutput("plant_date_yr1")
           )
    ),
    column(3,
           wellPanel(
             h4("2025 Info"),
             textOutput("crop_type_yr2"),
             br(),
             textOutput("plant_date_yr2")
           )
    )
  ),
  
  # Map
  fluidRow(
    column(12,
           leafletOutput("map", height = 400)
    )
  ),
  
  # Controls
  fluidRow(
    column(4,
           selectInput("soil_layer", "Select Soil Layer to Plot", 
                       choices = NULL)),
    column(4,
           selectInput("ndvi_date", "Select NDVI Date", 
                       choices = NULL)),
    column(4,
           radioButtons("ndvi_scale_type", "NDVI Scale:",
                        choices = c("Fixed (0–1)" = "fixed",
                                    "Dynamic (per image)" = "dynamic"),
                        selected = "fixed"))
  ),
  
  # Growth curve plot
  fluidRow(
    column(12,
           plotlyOutput("ndvi_curve_plot")
    )
  )
)

###############################################################################
### Server #####
############################################################################### 
server <- function(input, output, session) {
  
  # Reactive function to load data based on site selection
  current_site_data <- reactive({
    req(input$site_select)
    
    # Get current site name from lookup
    current_site <- sites[[input$site_select]]
    
    # Import files for selected site
     withProgress(message = 'Loading site data...', {
    
     incProgress(0.1, detail = "Loading raster files...")
    # Soil and zones raster
    soil.rast <- rast(paste0(location_files_for_app, current_site, "/", "soil.tif"))
    zones.rast <- rast(paste0(location_files_for_app, current_site, "/", "zones.tif"))
    
     incProgress(0.2, detail = "Loading NDVI data...")
    # NDVI data
    NDVI_most_recent <- readRDS(paste0(location_files_for_app, current_site, "/", yr2, "/", "ndvi_stack_", yr2, ".rds"))
    # NDVI_yr1 <- subset(NDVI_most_recent, grep(yr1, names(NDVI_most_recent)))
    # NDVI_yr2 <- subset(NDVI_most_recent, grep(yr2, names(NDVI_most_recent)))
    
     incProgress(0.3, detail = "Loading site information...")
    # Site data
    site.data <- readRDS(paste0(location_files_for_app, current_site, "/", "site_info.rds"))
    site.data_df <- as.data.frame(site.data$seasons)
    site.data_yr1_df <- dplyr::filter(site.data_df, year == 2024)
    site.data_yr2_df <- dplyr::filter(site.data_df, year == 2025)
    
     incProgress(0.5, detail = "Loading growth curve data...")
    # Growth curve data
    # growth_curve_data_yr1 <- 
    #   read_csv(paste0(location_files_for_app, current_site, "/", yr1, "/", "ndvi_growth_curves_", yr1, ".csv"), show_col_types = FALSE) %>% 
    #   dplyr::mutate(site = current_site, year = yr1)
    
    growth_curve_data_yr2 <- 
      read_csv(paste0(location_files_for_app, current_site, "/", yr2, "/", "ndvi_growth_curves_", yr2, ".csv"), show_col_types = FALSE) %>% 
      dplyr::mutate(site = current_site, year = yr2)
    
     incProgress(1, detail = "Data loading complete!")
     })
    
    # Return list of all data
    list(
      soil_rast = soil.rast,
      zones_rast = zones.rast,
      #ndvi_yr1 = NDVI_yr1,
      #ndvi_yr2 = NDVI_yr2,
      NDVI_most_recent,
      site_data = site.data,
      site_data_yr1 = site.data_yr1_df,
      site_data_yr2 = site.data_yr2_df,
      #growth_data_yr1 = growth_curve_data_yr1,
      growth_data_yr2 = growth_curve_data_yr2,
      site_name = current_site
    )
  })
  
  # Update soil layer choices when site changes
  observe({
    site_data <- current_site_data()
    req(site_data$soil_rast)
    updateSelectInput(session, "soil_layer",
                      choices = names(site_data$soil_rast),
                      selected = names(site_data$soil_rast)[1])
  })
  
  # Update NDVI date choices when site changes
  observe({
    site_data <- current_site_data()
    req(site_data$NDVI_most_recent)
    updateSelectInput(session, "ndvi_date",
                      choices = names(site_data$NDVI_most_recent),
                      selected = names(site_data$NDVI_most_recent)[1])
  })
  
  # Render map
  output$map <- renderLeaflet({
    site_data <- current_site_data()
    req(site_data$site_data)
    
    bbox <- st_bbox(site_data$site_data$boundary)
    xmin <- as.numeric(bbox["xmin"])
    xmax <- as.numeric(bbox["xmax"])
    ymin <- as.numeric(bbox["ymin"])
    ymax <- as.numeric(bbox["ymax"])
    
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addPolygons(data = site_data$site_data$boundary, 
                  color = "blue", weight = 2, fill = FALSE, group = "Boundary") %>%
      addPolygons(data = site_data$site_data$trial_plan,
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
  
  # Reactive code for NDVI layer
  observe({
    req(input$ndvi_date)
    site_data <- current_site_data()
    req(site_data$NDVI_most_recent)
    
    # Check if the selected NDVI date exists in the current site's data
    if (!input$ndvi_date %in% names(site_data$NDVI_most_recent)) {
      return()  # Exit if the layer doesn't exist
    }
    
    selected_raster <- site_data$NDVI_most_recent[[input$ndvi_date]]
    leaflet_raster <- raster(selected_raster)
    
    # Determine scale type
    if (input$ndvi_scale_type == "fixed") {
      pal <- colorNumeric(palette = "YlGn", domain = c(0, 1), na.color = "transparent")
      legend_vals <- c(0, 1)
    } else {
      raster_vals <- values(leaflet_raster)
      pal <- colorNumeric(palette = "YlGn", domain = raster_vals, na.color = "transparent")
      legend_vals <- raster_vals
    }
    
    leafletProxy("map") %>%
      clearImages() %>%
      clearControls() %>%
      addRasterImage(leaflet_raster, colors = pal, opacity = 0.6, project = TRUE) %>%
      addLegend(pal = pal, values = legend_vals, title = "NDVI", labFormat = labelFormat())
  })
  
  # Reactive code for soil layer
  observe({
    req(input$soil_layer)
    site_data <- current_site_data()
    req(site_data$soil_rast)
    
    # Check if the selected soil layer exists in the current site's data
    if (!input$soil_layer %in% names(site_data$soil_rast)) {
      return()  # Exit if the layer doesn't exist
    }
    
    selected_soil <- site_data$soil_rast[[input$soil_layer]]
    soil_raster <- raster(selected_soil)
    
    # Create color palette for soil layer
    soil_pal <- colorNumeric(palette = "viridis", domain = values(soil_raster), na.color = "transparent")
    
    leafletProxy("map") %>%
      clearGroup("Soil Layer") %>%
      clearControls() %>%
      addRasterImage(soil_raster, colors = soil_pal, opacity = 0.7, project = TRUE, group = "Soil Layer") %>%
      addLegend(pal = soil_pal, values = values(soil_raster), title = input$soil_layer,
                position = "bottomleft", group = "Soil Layer")
  })
  
  # Growth curve plot
  output$ndvi_curve_plot <- renderPlotly({
    site_data <- current_site_data()
    req(site_data$growth_data_yr2)
    
    dat.clean <- site_data$growth_data_yr2
    dat.clean <- dat.clean %>% mutate(treat_desc_label = case_when(
      treat_desc == "Control (-Tillage -Lime).."  ~ "control",
      treat_desc == "Control.."                   ~ "control",
      treat_desc == "Control"                     ~ "control",
      treat_desc == "Control"                     ~ "control",
      .default = treat_desc
    ))
    
    # Clean site name by removing numbers, dots, and replacing underscores
    clean_site_name <- gsub("^\\d+\\.", "", site_data$site_name)  # Remove leading numbers and dot
    clean_site_name <- gsub("_", " ", clean_site_name)           # Replace underscores with spaces
    
    p <- ggplot(dat.clean, aes(x = dap, y = ndvi, color = treat_desc_label, group = treat_desc_label)) +
      # Bold black control line
      geom_smooth(data = dplyr::filter(dat.clean,  treat_desc_label == "control"),
                  method = "gam", span = 0.3, se = FALSE,
                  color = "black", size = 1.5) +
      # Other treatments
      geom_smooth(data = dplyr::filter(dat.clean,  treat_desc_label != "control"),
                  method = "gam", span = 0.3, se = FALSE) +
      labs(
        title = paste("NDVI Timeseries (2025) -", clean_site_name),
        x = "Days after planting",
        y = "Average NDVI",
        color = "Treatment"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    ggplotly(p)
  })
  
  # Info text outputs
  output$crop_type_yr1 <- renderText({
    site_data <- current_site_data()
    req(site_data$site_data_yr1)
    if(nrow(site_data$site_data_yr1) > 0) {
      paste("Crop Type:", site_data$site_data_yr1$crop_type)
    } else {
      "No data available"
    }
  })
  
  output$crop_type_yr2 <- renderText({
    site_data <- current_site_data()
    req(site_data$site_data_yr2)
    if(nrow(site_data$site_data_yr2) > 0) {
      paste("Crop Type:", site_data$site_data_yr2$crop_type)
    } else {
      "No data available"
    }
  })
  
  output$plant_date_yr1 <- renderText({
    site_data <- current_site_data()
    req(site_data$site_data_yr1)
    if(nrow(site_data$site_data_yr1) > 0) {
      paste("Plant Date:", site_data$site_data_yr1$plant_date)
    } else {
      "No data available"
    }
  })
  
  output$plant_date_yr2 <- renderText({
    site_data <- current_site_data()
    req(site_data$site_data_yr2)
    if(nrow(site_data$site_data_yr2) > 0) {
      paste("Plant Date:", site_data$site_data_yr2$plant_date)
    } else {
      "No data available"
    }
  })
}

# Run the app
shinyApp(ui, server)