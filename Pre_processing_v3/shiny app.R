# =============================================================================
# app.R — af-sandysoils-ii Shiny App
# =============================================================================
# Purpose:
#   Interactive viewer for NDVI growth curves and Sentinel imagery across
#   the 8 sandy soils trial sites. This version covers:
#     - Site and year dropdowns
#     - Leaflet map with Google satellite imagery, paddock boundary,
#       and treatment strips
#
# Data:
#   Reads from shiny_app_data/ folder produced by Script 4.
#   Expected structure:
#     shiny_app_data/
#     ├── site_metadata.csv
#     └── <site_name>/
#         ├── <year>/
#         │   ├── <site_name>_NDVI_treatment_only_DAP.csv
#         │   └── <site_name>_NDVI_treatment_zone_DAP.csv
#         └── shapefiles/
#             ├── boundary/
#             └── trial_plan/
#
# Author:  Jackie Ouzman, CSIRO Agriculture & Food
# Project: af-sandysoils-ii
# Created: June 2026
# =============================================================================

library(shiny)
library(dplyr)
library(sf)
library(leaflet)

# =============================================================================
# PATH TO DATA — adjust if app.R is not in the same folder as shiny_app_data/
# =============================================================================

data_dir <- "H:/Output-1/shiny_app_data"

# =============================================================================
# LOAD METADATA — used to populate dropdowns
# =============================================================================

site_metadata <- read.csv(file.path(data_dir, "site_metadata.csv"),
                          stringsAsFactors = FALSE) %>%
  mutate(sowing_date = as.Date(sowing_date))

# Site list for dropdown — preserves the natural site order
site_choices <- unique(site_metadata$site_name)

# Treatment colour lookup — named vector keyed by treat_desc
# Used to colour treatment strips on the map consistently
treat_colour_lookup <- site_metadata %>%
  dplyr::select(site_name, treat, treat_desc, hex) %>%
  distinct()

# =============================================================================
# HELPER: load shapefiles for a given site
# =============================================================================

load_shapefiles <- function(site_name) {
  
  shp_dir <- file.path(data_dir, site_name, "shapefiles")
  
  boundary <- tryCatch(
    sf::st_read(file.path(shp_dir, "boundary"),   quiet = TRUE),
    error = function(e) NULL
  )
  
  trial_plan <- tryCatch(
    sf::st_read(file.path(shp_dir, "trial_plan"), quiet = TRUE),
    error = function(e) NULL
  )
  
  # st_transform happens in the observe block — not here
  list(boundary = boundary, trial_plan = trial_plan)
}

# =============================================================================
# UI
# =============================================================================

ui <- fluidPage(
  
  titlePanel("Sandy Soils Trial Sites — NDVI Viewer"),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      selectInput(
        inputId  = "site",
        label    = "Site",
        choices  = site_choices,
        selected = site_choices[1]
      ),
      
      # Year dropdown — populated dynamically based on site selection
      uiOutput("year_ui"),
      
      hr(),
      helpText("Map shows paddock boundary and treatment strips.",
               "Treatment colours match growth curve plots.")
    ),
    
    mainPanel(
      width = 9,
      leafletOutput("map", height = "600px")
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  # --- Dynamic year dropdown ---
  # Years available depend on which site is selected
  output$year_ui <- renderUI({
    req(input$site)
    
    years_for_site <- site_metadata %>%
      dplyr::filter(site_name == input$site) %>%
      dplyr::pull(Year) %>%
      unique() %>%
      sort(decreasing = TRUE)   # most recent year first
    
    selectInput(
      inputId  = "year",
      label    = "Year",
      choices  = years_for_site,
      selected = years_for_site[1]
    )
  })
  
  # --- Load shapefiles reactively when site changes ---
  # Shapefiles are the same regardless of year so we only reload on site change
  shp <- reactive({
    req(input$site)
    load_shapefiles(input$site)
  })
  
  # --- Treatment colours for selected site ---
  treat_colours <- reactive({
    req(input$site)
    treat_colour_lookup %>%
      dplyr::filter(site_name == input$site) %>%
      dplyr::select(treat, treat_desc, hex) %>%
      distinct()
  })
  
  # --- Base map ---
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(
        provider = providers$Esri.WorldImagery,
        options  = providerTileOptions(maxZoom = 20)
      ) %>%
      setView(lng = 133.5, lat = -32.0, zoom = 6)  # rough centre of SA/Vic grain belt
  })
  
  # --- Update map when site changes ---
  observe({
    req(input$site, shp())
    
    boundary   <- shp()$boundary
    trial_plan <- shp()$trial_plan
    colours    <- treat_colours()
    
    proxy <- leafletProxy("map") %>%
      clearShapes() %>%
      clearControls()
    
    # --- Boundary ---
    if (!is.null(boundary)) {
      
      boundary_wgs84 <- sf::st_transform(boundary, crs = 4326)
      bbox           <- sf::st_bbox(boundary_wgs84)
      
      proxy <- proxy %>%
        addPolygons(
          data        = boundary_wgs84,
          color       = "white",
          weight      = 2,
          fill        = FALSE,
          group       = "Boundary"
        ) %>%
        flyToBounds(
          lng1 = bbox[["xmin"]],
          lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]],
          lat2 = bbox[["ymax"]]
        )
    }
    
    # --- Treatment strips ---
    if (!is.null(trial_plan)) {
      
      trial_wgs84 <- sf::st_transform(trial_plan, crs = 4326) %>%
        dplyr::filter(treat != "B") %>%
        left_join(colours, by = c("treat", "treat_desc"))
      
      treat_pal <- colorFactor(
        palette = colours$hex,
        levels  = colours$treat_desc
      )
      
      proxy <- proxy %>%
        addPolygons(
          data        = trial_wgs84,
          fillColor   = ~treat_pal(treat_desc),
          fillOpacity = 0.4,
          color       = "white",
          weight      = 1,
          label       = ~treat_desc,
          group       = "Treatments"
        ) %>%
        addLegend(
          position = "bottomright",
          colors   = colours$hex,
          labels   = colours$treat_desc,
          title    = "Treatment",
          opacity  = 0.8
        )
    }
  })
}
# =============================================================================
# RUN
# =============================================================================

shinyApp(ui = ui, server = server)
