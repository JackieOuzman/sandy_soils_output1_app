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
      
      
      
      
      incProgress(0.3, detail = "Loading site information...")
      # Site data
      site.data <- readRDS(paste0(location_files_for_app, current_site, "/", "site_info.rds"))
      site.data_df <- as.data.frame(site.data$seasons)
      site.data_yr1_df <- dplyr::filter(site.data_df, year == 2024)
      site.data_yr2_df <- dplyr::filter(site.data_df, year == 2025)
      
     
      
      incProgress(1, detail = "Data loading complete!")
    })
    
    # Return list of all data
    list(
      site_data = site.data,
      site_data_yr1 = site.data_yr1_df,
      site_data_yr2 = site.data_yr2_df,
      site_name = current_site
    )
  })
  
  # Update soil layer choices when site changes
 
  

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