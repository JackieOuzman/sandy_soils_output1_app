# =============================================================================
# app.R — af-sandysoils-ii Shiny App
# =============================================================================
# Purpose:
#   Interactive viewer for NDVI growth curves and Sentinel imagery across
#   the 8 sandy soils trial sites. This version covers:
#     - Site and year dropdowns
#     - Leaflet map with Google satellite imagery, paddock boundary,
#       and treatment strips
#     - Toggle between Google satellite and zone basemap
#       using hex colours from metadata
#
# Author:  Jackie Ouzman, CSIRO Agriculture & Food
# Project: af-sandysoils-ii
# Created: June 2026
# =============================================================================

library(shiny)
library(dplyr)
library(sf)
library(leaflet)
library(terra)
library(raster)

# =============================================================================
# PATH TO DATA
# =============================================================================

data_dir <- "H:/Output-1/shiny_app_data"

# =============================================================================
# LOAD METADATA
# =============================================================================

site_metadata <- read.csv(file.path(data_dir, "site_metadata.csv"),
                          stringsAsFactors = FALSE) %>%
  mutate(sowing_date  = as.Date(sowing_date),
         harvest_date = as.Date(harvest_date)) %>%
  dplyr::filter(!is.na(site_name)) %>%
  mutate(display_name = case_when(
    site_name == "Walpeup_MRS125"             ~ "Walpeup MRS125",
    site_name == "Crystal_Brook_Brians_House"  ~ "Crystal Brook Brians House",
    site_name == "Wynarka_Mervs_West"          ~ "Wynarka Mervs West",
    site_name == "Wharminda_Woodys"            ~ "Wharminda Woodys",
    site_name == "Walpeup_Gums"                ~ "Walpeup Gums",
    site_name == "Crystal_Brook_Randals"        ~ "Crystal Brook Randals",
    site_name == "Wharminda_Bonanza"            ~ "Wharminda Bonanza",
    site_name == "Wynarka_Tanks"                ~ "Wynarka Tanks",
    TRUE ~ site_name
  ))

# Named vector for selectInput: display_name shown, site_name returned
site_choices <- setNames(
  unique(site_metadata$site_name),
  unique(site_metadata$display_name)
)

# Treatment colour lookup
treat_colour_lookup <- site_metadata %>%
  dplyr::select(site_name, treat, treat_desc, hex) %>%
  distinct()

# Zone field lookup — one row per site
zone_field_lookup <- site_metadata %>%
  dplyr::select(site_name, zone_field) %>%
  distinct()

# Zone label lookup — includes hex colours from metadata
zone_label_lookup <- site_metadata %>%
  dplyr::select(site_name, zone, zone_label, zone_hex) %>%
  mutate(zone = as.character(zone)) %>%
  distinct()

# =============================================================================
# UI
# =============================================================================

ui <- fluidPage(
  
  # --- Title ---
  titlePanel("Sandy Soils Trial Sites — NDVI Viewer"),
  
  hr(),
  
  # --- Dropdowns and basemap selector side by side ---
  fluidRow(
    column(
      width = 3,
      selectInput(
        inputId  = "site",
        label    = "Site",
        choices  = site_choices,
        selected = site_choices[1]
      )
    ),
    column(
      width = 2,
      uiOutput("year_ui")
    ),
    column(
      width = 3,
      radioButtons(
        inputId  = "basemap",
        label    = "Base map",
        choices  = c("Google satellite" = "google",
                     "Zone map"         = "zones",
                     "NDVI"             = "ndvi"),
        selected = "google"
      )
    )
  ),
  column(
    width = 3,
    conditionalPanel(
      condition = "input.basemap == 'ndvi'",
      uiOutput("ndvi_date_ui")
    )
  ),
  
  hr(),
  
  # --- Help text ---
  fluidRow(
    column(
      width = 12,
      helpText("Map shows paddock boundary and treatment strips.",
               "Treatment colours match growth curve plots.")
    )
  ),
  
  hr(),
  
  # --- Map full width ---
  fluidRow(
    column(
      width = 12,
      leafletOutput("map", height = "600px")
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  # --- Dynamic year dropdown ---
  output$year_ui <- renderUI({
    req(input$site)
    
    years_for_site <- site_metadata %>%
      dplyr::filter(site_name == input$site) %>%
      dplyr::pull(Year) %>%
      unique() %>%
      sort(decreasing = TRUE)
    
    selectInput(
      inputId  = "year",
      label    = "Year",
      choices  = years_for_site,
      selected = years_for_site[1]
    )
  })
  
  # --- NDVI date dropdown ---
  # Reads available dates from the NDVI stack TIF filename pattern
  output$ndvi_date_ui <- renderUI({
    req(input$site, input$year, input$basemap == "ndvi")
    
    yr_dir   <- file.path(data_dir, input$site, as.character(input$year))
    tif_file <- file.path(yr_dir, paste0(input$site, "_NDVI_stack.tif"))
    
    cat("Looking for TIF at:", tif_file, "\n")   # debug line — check console
    cat("File exists:", file.exists(tif_file), "\n")
    
    if (!file.exists(tif_file)) {
      return(helpText("No NDVI image available for this site and year."))
    }
    
    r     <- terra::rast(tif_file)
    dates <- names(r)
    
    cat("Dates found:", length(dates), "\n")   # debug line
    
    selectInput(
      inputId  = "ndvi_date",
      label    = "NDVI image date",
      choices  = dates,
      selected = dates[length(dates)]
    )
  })
  
  
  # --- Load shapefiles reactively when site changes ---
  shp <- reactive({
    req(input$site)
    
    shp_dir <- file.path(data_dir, input$site, "shapefiles")
    
    boundary <- tryCatch(
      sf::st_read(file.path(shp_dir, "boundary"),   quiet = TRUE),
      error = function(e) NULL
    )
    
    trial_plan <- tryCatch(
      sf::st_read(file.path(shp_dir, "trial_plan"), quiet = TRUE),
      error = function(e) NULL
    )
    
    zones <- tryCatch(
      sf::st_read(file.path(shp_dir, "zones"), quiet = TRUE),
      error = function(e) NULL
    )
    
    list(boundary = boundary, trial_plan = trial_plan, zones = zones)
  })
  
  # --- Treatment colours for selected site ---
  treat_colours <- reactive({
    req(input$site)
    treat_colour_lookup %>%
      dplyr::filter(site_name == input$site) %>%
      dplyr::select(treat, treat_desc, hex) %>%
      distinct()
  })
  
  # --- Zone field for selected site ---
  zone_field <- reactive({
    req(input$site)
    zone_field_lookup %>%
      dplyr::filter(site_name == input$site) %>%
      dplyr::pull(zone_field) %>%
      .[1]
  })
  
  # --- Zone labels and hex colours for selected site ---
  zone_labels <- reactive({
    req(input$site)
    zone_label_lookup %>%
      dplyr::filter(site_name == input$site) %>%
      dplyr::select(zone, zone_label, zone_hex) %>%
      mutate(zone = as.character(zone)) %>%
      distinct()
  })
  
  # --- Base map (rendered once, updated via proxy) ---
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(
        provider = providers$Esri.WorldImagery,
        options  = providerTileOptions(maxZoom = 20)
      ) %>%
      setView(lng = 133.5, lat = -32.0, zoom = 6)
  })
  
  # --- Update map when site or basemap selection changes ---
  observe({
    req(input$site, input$basemap, shp())
    
    boundary   <- shp()$boundary
    trial_plan <- shp()$trial_plan
    zones      <- shp()$zones
    colours    <- treat_colours()
    zf         <- zone_field()
    zlabels    <- zone_labels()
    
    proxy <- leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      clearTiles()
    
    # --- Switch basemap tiles ---
    if (input$basemap == "google") {
      proxy <- proxy %>%
        addProviderTiles(
          provider = providers$Esri.WorldImagery,
          options  = providerTileOptions(maxZoom = 20)
        )
    } else {
      proxy <- proxy %>%
        addProviderTiles(
          provider = providers$Esri.WorldGrayCanvas
        )
    }
    
    # --- Zoom to site ---
    if (!is.null(boundary)) {
      boundary_wgs84 <- sf::st_transform(boundary, crs = 4326)
      bbox           <- sf::st_bbox(boundary_wgs84)
      
      proxy <- proxy %>%
        flyToBounds(
          lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
        )
    }
    
    # --- Zone basemap layer ---
    if (input$basemap == "zones" && !is.null(zones) && !is.na(zf)) {
      
      zones_wgs84 <- sf::st_transform(zones, crs = 4326) %>%
        mutate(zone = as.character(.data[[zf]])) %>%
        left_join(zlabels, by = "zone")
      
      # Use hex colours from metadata
      zone_pal <- colorFactor(
        palette = zlabels$zone_hex,
        levels  = zlabels$zone
      )
      
      proxy <- proxy %>%
        addPolygons(
          data        = zones_wgs84,
          fillColor   = ~zone_pal(zone),
          fillOpacity = 0.6,
          color       = "white",
          weight      = 1,
          label       = ~zone_label,
          group       = "Zones"
        ) %>%
        addLegend(
          position = "bottomleft",
          colors   = zlabels$zone_hex,
          labels   = zlabels$zone_label,
          title    = "Zone",
          opacity  = 0.8
        )
    }
    
    # --- NDVI raster layer ---
    if (input$basemap == "ndvi") {
      req(input$ndvi_date, input$year)
      
      yr_dir   <- file.path(data_dir, input$site, as.character(input$year))
      tif_file <- file.path(yr_dir, paste0(input$site, "_NDVI_stack.tif"))
      
      if (file.exists(tif_file)) {
        
        r <- terra::rast(tif_file)
        
        # Guard: only proceed if the selected date exists in this stack
        if (input$ndvi_date %in% names(r)) {
          
          r_layer <- r[[input$ndvi_date]]
          r_wgs84 <- terra::project(r_layer, "EPSG:4326")
          
          ndvi_pal <- colorNumeric(
            palette  = c("#8B4513", "#F5DEB3", "#FFFF00", "#90EE90", "#006400"),
            domain   = c(-0.2, 0.8),
            na.color = "transparent"
          )
          
          proxy <- proxy %>%
            addRasterImage(
              x       = raster::raster(r_wgs84),
              colors  = ndvi_pal,
              opacity = 0.8,
              group   = "NDVI"
            ) %>%
            addLegend(
              position = "bottomleft",
              pal      = ndvi_pal,
              values   = c(-0.2, 0.8),
              title    = paste0("NDVI<br>", input$ndvi_date),
              opacity  = 0.8
            )
        }
      }
    }
    
    # --- Boundary ---
    if (!is.null(boundary)) {
      boundary_wgs84 <- sf::st_transform(boundary, crs = 4326)
      
      proxy <- proxy %>%
        addPolygons(
          data    = boundary_wgs84,
          color   = "black",
          weight  = 5,
          fill    = FALSE,
          group   = "Boundary"
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
      
      # When NDVI is showing, strips are outline only with black border for visibility
      fill_opacity   <- ifelse(input$basemap == "ndvi", 0,     0.4)
      strip_colour   <- ifelse(input$basemap == "ndvi", "black", "white")
      
      proxy <- proxy %>%
        addPolygons(
          data        = trial_wgs84,
          fillColor   = ~treat_pal(treat_desc),
          fillOpacity = fill_opacity,
          color       = strip_colour,
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