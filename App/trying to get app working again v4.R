## temp of Temp app
library(shiny)
library(leaflet)
library(plotly)
library(readr)
library(dplyr)
library(terra)
library(raster)  # Needed for rendering raster in leaflet
library(sf)


location_files_for_app <- 'B:/Shiny/Apps/Stirling/GRDCSandySoilsII/Output1Viewer/Current/Files/'

site_select <-"2.Crystal_Brook_Brians_House"

yr1 <- "2024"
yr2 <- "2025"
###############################################################################
### Server #####
############################################################################### 
current_site <-site_select



  


soil.rast <- rast(paste0(location_files_for_app, current_site, "/", "soil.tif"))
zones.rast <- rast(paste0(location_files_for_app, current_site, "/", "zones.tif"))
NDVI_most_recent <- readRDS(paste0(location_files_for_app, current_site, "/", yr2, "/", "ndvi_stack_", yr2, ".rds"))


site.data <- readRDS(paste0(location_files_for_app, current_site, "/", "site_info.rds"))
site.data_df <- as.data.frame(site.data$seasons)
site.data_yr1_df <- dplyr::filter(site.data_df, year == 2024)
site.data_yr2_df <- dplyr::filter(site.data_df, year == 2025)

growth_curve_data_yr2 <- 
  read_csv(paste0(location_files_for_app, current_site, "/", yr2, "/", "NDVI_growth_curves_sentinel_", yr2, ".csv"), show_col_types = FALSE) %>% 
  dplyr::mutate(site = current_site, year = yr2)

current_site_data <- 
  list(
    soil_rast = soil.rast,
    zones_rast = zones.rast,
    ndvi_most_recent = NDVI_most_recent,  # Fixed: properly named in list
    site_data = site.data,
    site_data_yr1 = site.data_yr1_df,
    site_data_yr2 = site.data_yr2_df,
    growth_data_yr2 = growth_curve_data_yr2,
    site_name = current_site)

ndvi_dates <- names(current_site_data$ndvi_most_recent)


 # Render map
# Remove the incomplete ggplot line
bbox <- st_bbox(current_site_data$site_data$boundary)
xmin <- as.numeric(bbox["xmin"])
xmax <- as.numeric(bbox["xmax"])
ymin <- as.numeric(bbox["ymin"])
ymax <- as.numeric(bbox["ymax"])

map <- leaflet() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addPolygons(data = current_site_data$site_data$boundary, 
              color = "blue", weight = 2, fill = FALSE, group = "Boundary") %>%
  addPolygons(data = current_site_data$site_data$trial_plan,
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
  
map


##-------------------------------------------------------------##

# Reactive code for NDVI layer - FIXED
# observe({
#   req(input$ndvi_date)
#   site_data <- current_site_data()
#   req(site_data$ndvi_most_recent)  # 
#   
  # Check if the selected NDVI date exists in the current site's data
  if ( ndvi_dates%in% names(current_site_data$ndvi_most_recent)) {
    return()  # Exit if the layer doesn't exist
  }
  # 
  #selected_raster <- site_data$ndvi_most_recent[[input$ndvi_date]]
  selected_raster <- current_site_data$ndvi_most_recent["2025-10-05"] 
  leaflet_raster <- raster(selected_raster)
  
  # Determine scale type
  # if (input$ndvi_scale_type == "fixed") {
  #   pal <- colorNumeric(palette = "YlGn", domain = c(0, 1), na.color = "transparent")
  #   legend_vals <- c(0, 1)
  # } else {
    raster_vals <- values(leaflet_raster)
    pal <- colorNumeric(palette = "YlGn", domain = raster_vals, na.color = "transparent")
    legend_vals <- raster_vals
 # }
  
    
    map2 <- map %>%
      addRasterImage(leaflet_raster, colors = pal, opacity = 0.6, project = TRUE, group = "NDVI") %>%
      addLegend(pal = pal, values = values(leaflet_raster), title = "NDVI", labFormat = labelFormat())
    
  # leafletProxy("map") %>%
  #   clearImages() %>%
  #   clearControls() %>%
  #   addRasterImage(leaflet_raster, colors = pal, opacity = 0.6, project = TRUE) %>%
  #   addLegend(pal = pal, values = legend_vals, title = "NDVI", labFormat = labelFormat())




##---------------------------------------------------------------##


## are all the dates unique?


sort(ndvi_dates)
nm<- sort(names(NDVI_most_recent))
nm
duplicated(nm)
# # Growth curve plot




  
  # dat.clean <- current_site_data$growth_data_yr2
  # dat.clean <- dat.clean %>%
  #   dplyr::mutate(
  #     treat_desc_label = dplyr::case_when(
  #       treat_desc == "Control (-Tillage -Lime).." ~ "control",
  #       treat_desc == "Control.." ~ "control",
  #       treat_desc == "Control" ~ "control",
  #       TRUE ~ as.character(treat_desc)
  #     )
  #   )
 ##-------------------------------------------------------------### 
  ## Can I replace this my sentinel plotting code
  dat.clean <- current_site_data$growth_data_yr2
  
  #Palette (force Control = black if present)
  treat_lvls <- levels(factor(dat.clean$treat_desc))
  base_cols  <- hue_pal()(length(treat_lvls))
  names(base_cols) <- treat_lvls
  if ("Control (-Tillage -Lime)" %in% names(base_cols)) base_cols["Control (-Tillage -Lime)"] <- "black"
  
  ctrl_candidates <- c("Control (-Tillage -Lime)", "Control")
  ctrl_name <- intersect(ctrl_candidates, names(base_cols))[1]
  if (!is.na(ctrl_name)) base_cols[ctrl_name] <- "black"
  
  #Dates for the top axis
  plant_date <- current_site_data$site_data_yr2$plant_date 
  plant_date <- lubridate::ymd(plant_date)
  
  top_breaks <- as.numeric(seq(
    plant_date,
    plant_date + max(dat.clean$dap, na.rm = TRUE),
    by = "3 weeks"
  ))
  # 
  
  
  
  # Clean site name by removing numbers, dots, and replacing underscores
  clean_site_name <- gsub("^\\d+\\.", "", current_site_data$site_name)  # Remove leading numbers and dot
  clean_site_name <- gsub("_", " ", clean_site_name)           # Replace underscores with spaces
  
  #last_date_sen <- max(ndvi_dates)
  ndvi_names_1 <- names(current_site_data$ndvi_most_recent)
  ndvi_names_max <- max(ndvi_names_1)
  
  p <- ggplot(dat.clean, aes(dap, ratio, color = treat_desc, group = treat_desc)) +
    geom_smooth(method = "gam", formula = y ~ s(x, k = 8), se = FALSE, linewidth = 0.8) +
    scale_color_manual(values = base_cols) +
    scale_x_continuous(
      name = "Days after planting (DAP)",
      sec.axis = sec_axis(
        trans  = ~ as.numeric(plant_date) + .,
        name   = "Date",
        labels = function(x) format(as.Date(x, origin = "1970-01-01"), "%d-%b"),
        breaks = top_breaks
      )
    ) +
    labs(
      title = paste0(clean_site_name, yr2),
      y = paste0("Average ", "Sentinel NDVI"),
      color = "Treatment"
    ) +
    theme_minimal() +
    theme(
      plot.title       = element_markdown(hjust = 0.5, lineheight = 1.1),
      axis.title.x.top = element_text(size = 12, margin = ggplot2::margin(b = 10)),
      axis.text.x.top  = element_text(size = 12),
      axis.title.x     = element_text(size = 16),
      axis.text.x      = element_text(size = 16),
      axis.title.y     = element_text(size = 16),
      axis.text.y      = element_text(size = 16)
    ) +
    coord_cartesian(ylim = c(NA, max(dat.clean$ratio, na.rm = TRUE) + 0.08)) +
    annotate(
      "text",
      x = max(dat.clean$dap, na.rm = TRUE),
      y = max(dat.clean$ratio, na.rm = TRUE) + 0.05,
       label = paste("Latest cloud free image date:", ndvi_names_max),
      hjust = 1, vjust = 0, size = 4, color = "black"
    ) +
    #emphasise Control in the plot only (legend unchanged)
    geom_smooth(
      data = subset(dat.clean, treat_desc == "Control"),
      aes(dap, ratio, group = treat_desc),
      method = "gam", formula = y ~ s(x, k = 8), se = FALSE,
      color = "black", linewidth = 1.2, show.legend = FALSE
    )
  
  p
  
  ##-------------------------------------------------------------### 
  
  # Clean site name by removing numbers, dots, and replacing underscores
  clean_site_name <- gsub("^\\d+\\.", "", current_site_data$site_name)  # Remove leading numbers and dot
  clean_site_name <- gsub("_", " ", clean_site_name)           # Replace underscores with spaces
  
  p <- ggplot(dat.clean, aes(x = dap, y = ratio , color = treat_desc_label, group = treat_desc_label)) +
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
#})
