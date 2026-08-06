# =============================================================================
# app.R — af-sandysoils-ii Shiny App
# =============================================================================
# Purpose:
#   Interactive viewer for NDVI growth curves and Sentinel imagery across
#   the 8 sandy soils trial sites. This version covers:
#     - Site and year dropdowns
#     - Leaflet map with Google satellite imagery, paddock boundary,
#       and treatment strips
#     - Toggle between Google satellite, zone basemap, and NDVI raster
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

#data_dir <- "H:/Output-1/shiny_app_data"
#App data now is copied over to new bowen dirctory which is just for NDVI app data
proj_dir_app_data <- "//fs1-cbr.nexus.csiro.au/{ss-output1-app-data}" 
output_root <-  file.path(proj_dir_app_data,"work", "Output-1", "shiny_app_data")
data_dir <-output_root
#data_dir <- "H:/Output-1/shiny_app_data"
#data on the internal server (this need to be changed to new Bowen)
#data_dir <- "/datasets/work/sc-shiny/work/live_apps/Jackie_Ouzman/SSII_Output1_NDVI_App/shiny_app_data"

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
# NDVI COLOUR PALETTE
# =============================================================================
# A perceptually sensible NDVI ramp:
#   deep red (bare/negative) → tan (sparse) → yellow-green (moderate) →
#   mid green → dark green (dense canopy)

ndvi_colours <- c(
  "#8B0000",   # deep red      — bare soil / negative NDVI
  "#CC4400",   # burnt orange  — very sparse
  "#E8A020",   # amber         — sparse / senescent
  "#F5E642",   # yellow        — moderate greenness
  "#A8CC30",   # yellow-green  — developing canopy
  "#4DB84A",   # mid green     — good canopy
  "#1A7A1A",   # dark green    — dense canopy
  "#004400"    # very dark     — maximum greenness
)

ndvi_domain <- c(-0.2, 0.8)

# =============================================================================
# UI
# =============================================================================

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("html { scroll-behavior: smooth; }"))
  ),
  
  # --- Title bar with CSIRO logo ---
  fluidRow(
    column(
      width = 12,
      style = "display: flex; align-items: center; gap: 16px; padding: 12px 15px 4px;",
      tags$img(
        src    = "https://www.csiro.au/~/media/Web-team/Images/CSIRO_Logo/CSIRO_Logo.svg",
        height = "48px",
        alt    = "CSIRO logo"
      ),
      ## NEW Aug26 ##
      tags$img(
        src    = "docs/GRDC_Logo_Primary_Default.jpg",
        height = "48px",
        alt    = "GRDC logo"
      ),
      ## NEW Aug26 ##
      tags$h3(
        "Sandy Soils Trial Sites — NDVI Viewer",
        style = "margin: 0; font-weight: 600;"
      ),
      # --- Push buttons to the right ---
      div(
        style = "margin-left: auto; display: flex; gap: 10px;",
        tags$a(
          href   = "docs/NDVI_Viewer_User_Guide.pdf",
          target = "_blank",
          class  = "btn btn-outline-secondary",
          style  = "border: 1px solid #2E7D32; color: #2E7D32; font-weight: 600;",
          icon("book"), " User Guide"
        ),
        tags$a(
          href   = "docs/NDVI_Viewer_Metadata.pdf",
          target = "_blank",
          class  = "btn btn-outline-secondary",
          style  = "border: 1px solid #2E7D32; color: #2E7D32; font-weight: 600;",
          icon("info-circle"), " Metadata"
        
        )
        ,
        tags$a(
          href   = "docs/NDVI_Viewer_Copyright_Disclaimer.pdf",
          target = "_blank",
          class  = "btn btn-outline-secondary",
          style  = "border: 1px solid #2E7D32; color: #2E7D32; font-weight: 600;",
          icon("copyright"), " Copyright/Disclaimer"
        )
      )
    )
  ),
  
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
                     "NDVI"             = "ndvi",
                     "None"             = "none"),
        selected = "google"
      )
    ),
    column(
      width = 3,
      conditionalPanel(
        condition = "input.basemap == 'ndvi'",
        uiOutput("ndvi_date_ui")
      )
    )
  ),
  
  hr(),
  
  # --- Season info bar ---
  fluidRow(
    column(
      width = 12,
      uiOutput("season_info")
    )
  ),
  
  hr(),
  
  fluidRow(
    column(
      width = 12,
      style = "text-align: center; padding: 8px 0;",
      tags$a(
        href  = "#growth_curves",
        style = "text-decoration: none; font-weight: 600; color: #2E7D32;",
        "Jump to growth curve plots ↓"
      )
    )
  ),
  
  # --- Map full width ---
  fluidRow(
    column(
      width = 12,
      leafletOutput("map", height = "600px")
    )
  ),
  
  
  
  hr(),
  
  # --- Growth curves heading ---
  fluidRow(
    column(
      width = 12,
      tags$h3(id = "growth_curves", "Growth Curves")
    )
  ),
  
  fluidRow(
    column(
      width = 3,
      radioButtons(
        inputId  = "growth_curve_type",
        label    = "Display by",
        choices  = c("Treatment only"       = "treatment",
                     "Treatment with zones" = "zone"),
        selected = "treatment"
      )
    ),
    column(
      width = 3,
      radioButtons(
        inputId  = "plot_type",
        label    = "Plot type",
        choices  = c("Growth curve"    = "growth_curve",    # ← AUC moved to last
                     "Cumulative NDVI" = "cumulative_ndvi",
                     ##NEW Aug 2026##
                     "Area Under Curve (AUC)"             = "AUC"),
                     ##NEW Aug 2026##
        selected = "growth_curve"
      )
    )
  ),
  
  fluidRow(
    column(
      width = 12,
      uiOutput("growth_curve_image")
    )
  )
  
)   # closing fluidPage

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  # Serve the data directory as a static resource path
  addResourcePath("plots", data_dir)
  addResourcePath("docs", file.path(data_dir, "docs"))
  
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
  output$ndvi_date_ui <- renderUI({
    req(input$site, input$year, input$basemap == "ndvi")
    
    yr_dir   <- file.path(data_dir, input$site, as.character(input$year))
    tif_file <- file.path(yr_dir, paste0(input$site, "_NDVI_stack.tif"))
    
    
    if (!file.exists(tif_file)) {
      return(helpText("No NDVI image available for this site and year."))
    }
    
    r     <- terra::rast(tif_file)
    
    # Mask zeros before calculating season range
    r_clean <- terra::classify(r, cbind(0, NA))
    
    # Store season min/max for use in the map render
    season_min <- terra::global(r_clean, fun = "min", na.rm = TRUE) |> min()
    season_max <- terra::global(r_clean, fun = "max", na.rm = TRUE) |> max()
    
    # Save to session for use in the map observer
    session$userData$ndvi_domain <- c(season_min, season_max)
    
    dates <- names(r)
    
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
  
  # --- Season info bar ---
  output$season_info <- renderUI({
    req(input$site, input$year)
    
    info <- site_metadata %>%
      dplyr::filter(site_name == input$site,
                    Year      == as.integer(input$year)) %>%
      dplyr::select(Year, crop, variety, sowing_date, harvest_date, season_note) %>%
      distinct() %>%
      .[1, ]
    
    crop_text <- if (!is.na(info$crop) && info$crop != "") {
      if (!is.na(info$variety) && info$variety != "")
        paste0(info$crop, " — ", info$variety)
      else
        info$crop
    } else {
      "Crop TBC"
    }
    
    sown_text <- if (!is.na(info$sowing_date))
      paste0("Sown ", format(as.Date(info$sowing_date), "%d %b %Y"))
    else
      "Sowing date TBC"
    
    harvest_text <- if (!is.na(info$harvest_date))
      paste0("Harvested ", format(as.Date(info$harvest_date), "%d %b %Y"))
    else
      NULL
    
    note_text <- if (!is.na(info$season_note) && info$season_note != "")
      info$season_note
    else
      NULL
    
    pieces <- c(as.character(info$Year), crop_text,
                sown_text, harvest_text, note_text)
    pieces <- pieces[!is.null(pieces)]
    
    tags$p(
      style = "font-size: 15px; color: #444; padding: 4px 0;",
      paste(pieces, collapse = "  |  ")
    )
  })
  
  # --- Base map (rendered once, updated via proxy) ---
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(scrollWheelZoom = FALSE)) %>%
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
    } else if (input$basemap == "none") {
      proxy <- proxy %>%
        addProviderTiles(
          provider = providers$CartoDB.PositronNoLabels
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
        
        if (input$ndvi_date %in% names(r)) {
          
          r_layer <- r[[input$ndvi_date]]
          
          # Mask tile-edge nodata (coded as 0) to NA
          r_layer <- terra::classify(r_layer, cbind(0, NA))
          
          r_wgs84 <- terra::project(r_layer, "EPSG:4326")
          
          # Use seasonal domain if available, fall back to fixed global domain
          domain_to_use <- if (!is.null(session$userData$ndvi_domain)) {
            session$userData$ndvi_domain
          } else {
            ndvi_domain   # your fixed c(-0.2, 0.8)
          }
          
          ndvi_pal <- colorNumeric(
            palette  = ndvi_colours,
            domain   = domain_to_use,
            na.color = "transparent"
          )
          
          proxy <- proxy %>%
            addRasterImage(
              x       = raster::raster(r_wgs84),
              colors  = ndvi_pal,
              opacity = 0.85,
              group   = "NDVI"
            ) %>%
            addLegend(
              position = "bottomleft",
              pal      = ndvi_pal,
              values   = domain_to_use,
              title    = paste0("NDVI<br>", input$ndvi_date, 
                                "<br><span style='font-size:10px; font-weight:normal; color:#666;'>",
                                "Scale fitted to ", input$year, " season</span>"),
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
        dplyr::filter(treat != "Buffer") %>%
        left_join(colours, by = c("treat", "treat_desc"))
      
      treat_pal <- colorFactor(
        palette = colours$hex,
        levels  = colours$treat_desc
      )
      
      fill_opacity <- ifelse(input$basemap %in% c("ndvi", "zones"), 0, 0.4)
      strip_colour <- ifelse(input$basemap %in% c("ndvi", "zones"), "black", "white")
      
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
  
  # --- Growth curve image display ---
  output$growth_curve_image <- renderUI({
    req(input$site, input$year, input$growth_curve_type, input$plot_type)
    
    png_name <- paste0(input$site, "_",
                       input$plot_type, "_",
                       input$growth_curve_type, ".png")
    
    png_path <- file.path(data_dir, input$site,
                          as.character(input$year), png_name)
    
    if (!file.exists(png_path)) {
      return(helpText(paste0("No plot available for this selection: ", png_name)))
    }
    
    url_path <- paste0("plots/", input$site, "/",
                       as.character(input$year), "/", png_name,
                       "?t=", as.numeric(Sys.time()))
    
    tags$img(
      src   = url_path,
      style = "width:100%; max-width:1000px;"
    )
  })
  
}  # closing server

# =============================================================================
# RUN
# =============================================================================

shinyApp(ui = ui, server = server)