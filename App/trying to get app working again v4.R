## temp of Temp app

location_files_for_app <- 'B:/Shiny/Apps/Stirling/GRDCSandySoilsII/Output1Viewer/Current/Files/'

site_select <- "1.Walpeup_MRS125"

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

leaflet() %>%
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
  



# # Growth curve plot

  
  dat.clean <- current_site_data$growth_data_yr2
  dat.clean <- dat.clean %>%
    dplyr::mutate(
      treat_desc_label = dplyr::case_when(
        treat_desc == "Control (-Tillage -Lime).." ~ "control",
        treat_desc == "Control.." ~ "control",
        treat_desc == "Control" ~ "control",
        TRUE ~ as.character(treat_desc)
      )
    )
  
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
})
