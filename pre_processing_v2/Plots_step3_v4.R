

rm(list=ls())
# Pre-process data for the Site Viewer (all layers in EPSG:4326)
suppressPackageStartupMessages({
  library(terra)
  library(tidyverse)
  library(sf)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(lubridate)
  library(ggtext)
  library(scales)
  library(ggplot2)
  library(fs)
})

# ====================== Sites ======================
site <- "1.Walpeup_MRS125"
#site <- "2.Crystal_Brook_Brians_House"
#site <- "3.Wynarka_Mervs_West"
#site <- "4.Wharminda"
#site <- "5.Walpeup_Gums"
#site <- "6.Crystal_Brook_Randals"

# ====================== Year ======================
year_of_analysis <- 2025
# ====================== PATHS ======================
Dir <- paste0("C:/Users/ouz001/working_from_home_post_Sep2022/sandy_soils_output1_app/Pre_processing_v2/",
              site, "/preprocessing_output")

readDir <- paste0(Dir,"/", year_of_analysis)


metadata_path <- "//fs1-cbr.nexus.csiro.au/{lw-soildatarepo}/work/Shiny/Apps/Stirling/GRDCSandySoilsII/Output1Viewer/"




saveDir_year <- file.path(readDir, "plots")


# ====================== METADATA for site and other info ======================

file_path_details <- readxl::read_excel(
  paste0(metadata_path,"names of treatments per site 2025 metadata and other info.xlsx"),
  sheet = "location of file and details") %>% 
  filter(Site == site)
site.info <- readRDS(paste0(Dir, "/site_info.rds"))

# ====================== METADATA for sentinel ======================

metadata_sentinel <- read_csv(paste0(readDir, "/metadata_growth_curves_sentinel2025.csv"))

last_date_sen <-   metadata_sentinel[1,1]
# ====================== ratio to plot ======================
ratio_type <- 
   # "NDVI" 
  #"EVI2" 
  # "ExG" 
   "NDMI" 
  #"NDRE" 


# ====================== bring in data paths======================



ratio <- paste0(readDir,"/",ratio_type,
                           "_growth_curves_sentinel_",
                           year_of_analysis,
                           ".csv")

ratio_cummulative <- paste0(readDir,"/",ratio_type,
                           "_growth_curves_cumulative_sentinel_",
                           year_of_analysis,
                           ".csv")
#-----#
ratio_ZONE <- paste0(readDir,"/",ratio_type,
                "_growth_curves_sentinel_ZONE_",
                year_of_analysis,
               ".csv")


# ====================== DATA ======================
long_df_sen <- read_csv(ratio)
long_df_cum_sen <- read_csv(ratio_cummulative)
long_zone <- read_csv(ratio_ZONE)
# ====================== HELPERS FOR PLOTS ======================



#Plots (use numeric origin for sec.axis to avoid date hiccups)

#Palette (force Control = black if present)
treat_lvls <- levels(factor(long_df_sen$treat_desc))
base_cols  <- hue_pal()(length(treat_lvls))
names(base_cols) <- treat_lvls
if ("Control (-Tillage -Lime)" %in% names(base_cols)) base_cols["Control (-Tillage -Lime)"] <- "black"

# Dates for the top axis
seasons <- site.info$seasons

plant_date <-seasons %>% 
  filter(Site == site) %>% 
  filter(year == year_of_analysis) %>% 
  select(plant_date) 

plant_date$plant_date <- lubridate::ymd(plant_date$plant_date)

top_breaks <- as.numeric(seq(
  plant_date$plant_date,
  plant_date$plant_date + max(long_df_sen$dap, na.rm = TRUE),
  by = "3 weeks"
))


p_sen <- ggplot(long_df_sen, aes(dap, ratio, color = treat_desc, group = treat_desc)) +
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
    title = paste0("**<span style='font-size:18pt;'>", site,
                   "</span>**<br>Sentinel Timeseries (", year_of_analysis, ")"),
    y = paste0("Average ", ratio_type),
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
  coord_cartesian(ylim = c(NA, max(long_df_sen$ratio, na.rm = TRUE) + 0.08)) +
  annotate(
    "text",
    x = max(long_df_sen$dap, na.rm = TRUE),
    y = max(long_df_sen$ratio, na.rm = TRUE) + 0.05,
    label = paste("Latest cloud free image date:", 
                  format(last_date_sen$x, "%Y-%b-%d")),
                  hjust = 1, vjust = 0, size = 4, color = "black"
  ) +
  #emphasise Control in the plot only (legend unchanged)
  geom_smooth(
    data = subset(long_df_sen, treat_desc == "Control"),
    aes(dap, ratio, group = treat_desc),
    method = "gam", formula = y ~ s(x, k = 8), se = FALSE,
    color = "black", linewidth = 1.2, show.legend = FALSE
  )

p_sen


p_cum_sen <- ggplot(long_df_cum_sen, aes(x = dap, y = cum_ratio, color = treat_desc, group = treat_desc)) +
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
    title = paste0("**<span style='font-size:18pt;'>", site,
                   "</span>**<br>Sentinel Cumulative (AUC) — ", year_of_analysis),
    y = paste0("Cumulative ", ratio_type, " (AUC)"),
    color = "Treatment"
  ) +
  theme_minimal() +
  theme(
    plot.title       = ggtext::element_markdown(hjust = 0.5, lineheight = 1.1),
    axis.title.x.top = element_text(size = 12, margin = ggplot2::margin(b = 10)),
    axis.text.x.top  = element_text(size = 12),
    axis.title.x     = element_text(size = 16),
    axis.text.x      = element_text(size = 16),
    axis.title.y     = element_text(size = 16),
    axis.text.y      = element_text(size = 16)
  ) +
  coord_cartesian(ylim = c(NA, max(long_df_cum_sen$cum_ratio, na.rm = TRUE) * 1.05)) +
  annotate(
    "text",
    x = max(long_df_cum_sen$dap, na.rm = TRUE),
    y = max(long_df_cum_sen$cum_ratio, na.rm = TRUE) * 1.03,
    label = paste("Latest cloud free image date:", 
                  format(last_date_sen$x, "%Y-%b-%d")),
    hjust = 1, vjust = 0, size = 4, color = "black"
    
  ) +
  # emphasise Control in the plot only (legend unchanged)
  geom_smooth(
    data = subset(long_df_cum_sen, treat_desc == "Control"),
    aes(dap, cum_ratio, group = treat_desc),
    method = "gam", formula = y ~ s(x, k = 8), se = FALSE,
    color = "black", linewidth = 1.2, show.legend = FALSE
  )

p_cum_sen

# --- Save outputs in year folder ---



out_plot_sentinel     <- file.path(saveDir_year, paste0(ratio_type, "_growth_curves_sentinel", year_of_analysis, ".png"))
out_plot_cum_sentinel <- file.path(saveDir_year, paste0(ratio_type, "_growth_curves_cumulative_sentinel", year_of_analysis, ".png"))


ggsave(out_plot_sentinel,     p_sen,     width = 8, height = 5, dpi = 300)
ggsave(out_plot_cum_sentinel, p_cum_sen, width = 8, height = 5, dpi = 300)







##################################################################################
##                  ZONES
################################################################################

### HELPERS 

legend_rows <- 2  # ← tweak to 1/2/3 to control how “chunky” the legend is

# --- Palette (Control → black, robust to label variants) ---
treat_lvls <- levels(factor(long_zone$treat_desc))
base_cols  <- scales::hue_pal()(length(treat_lvls)); names(base_cols) <- treat_lvls

ctrl_candidates <- c("Control (-Tillage -Lime)", "Control")
ctrl_name <- intersect(ctrl_candidates, names(base_cols))[1]
if (!is.na(ctrl_name)) base_cols[ctrl_name] <- "black"


# Zone labels used only for facet strip text
zone_labels <- file_path_details %>% 
  filter(Site == site ) %>% 
  select(`zone label names` )

list_of_zone_labels_1 <- as.list(zone_labels)
list_of_zone_labels_2 <- unlist(strsplit(list_of_zone_labels_1$`zone label names`, ","))
names(list_of_zone_labels_2) <- unique(long_zone$zone_id)

 

# --- Plot: 3 zones in one row, compact legend, bigger x-axis fonts ---
p_zone_wide <- ggplot(long_zone, aes(dap, ratio, color = treat_desc, group = treat_desc)) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 8), se = FALSE, linewidth = 0.9) +
  scale_color_manual(values = base_cols) +
  scale_x_continuous(
    name = "Days after planting (DAP)",
    # sec.axis = sec_axis(
    #   #trans  = ~ as.numeric(plant_date) + .,
    #   #name   = "Date",
    #   labels = function(x) format(as.Date(x, origin = "1970-01-01"), "%d-%b"),
    #   breaks = top_breaks
    # )
  ) +
  facet_wrap(~zone_id, labeller = as_labeller(list_of_zone_labels_2))+
  labs(
    title = paste0("**<span style='font-size:18pt;'>", site,
                   "</span>**<br>Sentinel by Zone (", year_of_analysis, ")"),
    y = paste0("Average ", ratio_type )
  ) +
  theme_minimal() +
  theme(
    plot.title       = ggtext::element_markdown(hjust = 0.5, lineheight = 1.1),
    strip.text       = element_text(size = 12, face = "bold"),
    legend.position  = "bottom",
    legend.box       = "vertical",
    legend.justification = "center",
    # bigger x-axis fonts
    axis.text.x      = element_text(size = 14),
    axis.title.x     = element_text(size = 16),
    axis.text.x.top  = element_text(size = 8),
    axis.title.x.top = element_text(size = 12)
  ) 

p_zone_wide

# Emphasise Control in-plot only (legend unchanged)
if (!is.na(ctrl_name)) {
  p_zone_wide <- p_zone_wide +
    geom_smooth(
      data = subset(long_zone, treat_desc == ctrl_name),
      aes(dap, ratio, group = treat_desc),
      method = "gam", formula = y ~ s(x, k = 8), se = FALSE,
      color = "black", linewidth = 1.2, show.legend = FALSE
    )
}

p_zone_wide <- p_zone_wide +
  labs(
    subtitle = paste("Latest cloud free image date:", 
                             format(last_date_sen$x, "%Y-%b-%d"))
  )

p_zone_wide


ggsave(
  paste0(
    saveDir_year,
    "/",
    ratio_type,
    "_growth_curves_sentinel_2025_byzone.png"),
              p_zone_wide,
              width = 8,
              height = 5, dpi = 300)


