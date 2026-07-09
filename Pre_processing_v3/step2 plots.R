# =============================================================================
# SCRIPT 2: NDVI Growth Curve Plots — af-sandysoils-ii
# =============================================================================
# Purpose:
#   Reads the two CSV files produced by Script 1 and generates four sets
#   of plots for a selected site:
#     1. NDVI growth curves (mean NDVI vs days after planting)
#     2. Cumulative NDVI (running sum vs DAP)
#     3. NDVI growth curves faceted by treatment (control as reference)
#     4. Cumulative NDVI faceted by treatment (control as reference)
#     5. AUC bar charts (one value per treatment x zone combination)
#
#   Plot types 1, 2, and 5 are produced twice:
#     a. Treatment only (all treatments, no zone split) — from treatment_only CSV
#     b. Treatment x zone (faceted by zone) — from treatment_zone CSV
#
#   Plot types 3 and 4 are treatment-only, with each active treatment in its
#   own facet panel and the Control curve repeated in every panel as a grey
#   dashed reference line. This makes treatment vs control differences easier
#   to read than the overlapping multi-line format.
#
#   Plots are displayed in the RStudio viewer and saved as PNG files.
#
# Inputs:
#   - <site_name>_NDVI_treatment_only_DAP.csv
#   - <site_name>_NDVI_treatment_zone_DAP.csv
#   Both from: headDir/7.In_Season_data/YY/8.Sentinel_QGIS_Jackie/Growth_curves_output/
#
# Outputs (saved to same Growth_curves_output folder):
#   - <site_name>_growth_curve_treatment.png        all treatments overlaid
#   - <site_name>_growth_curve_zone.png             faceted by soil zone
#   - <site_name>_growth_curve_by_treatment.png     faceted by treatment, control ref
#   - <site_name>_cumulative_ndvi_treatment.png     all treatments overlaid
#   - <site_name>_cumulative_ndvi_zone.png          faceted by soil zone
#   - <site_name>_cumulative_ndvi_by_treatment.png  faceted by treatment, control ref
#   - <site_name>_AUC_treatment.png                 bar chart, all treatments
#   - <site_name>_AUC_zone.png                      bar chart, faceted by zone
#
# Notes:
#   - Control is excluded from facet panels in plots 3 and 4 but appears as
#     a grey dashed reference line in every panel via a ghost dataset built
#     with tidyr::crossing().
#   - dplyr::select() is called explicitly throughout to avoid masking by
#     the raster package if loaded in the same session.
#
# Author:  Jackie Ouzman, CSIRO Agriculture & Food
# Project: af-sandysoils-ii
# Created: June 2025
# Modified: June 2026 — added facet-by-treatment plots (1c, 2c) with
#                        control reference line; added dplyr:: prefix to
#                        select() calls to avoid raster package masking
# =============================================================================
# =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readxl)
  library(stringr)
})

# =============================================================================
# USER INPUT — change site number and year  only
# =============================================================================
year_of_analysis <- 2025

site_number_input <- 6  # 1 through 8

# =============================================================================
# SITE LOOKUP TABLE
# =============================================================================

site_lookup <- data.frame(
  id = 1:8,
  site_number = c(
    "1.Walpeup_MRS125",
    "2.Crystal_Brook_Brians_House",
    "3.Wynarka_Mervs_West",
    "4.Wharminda_Woodys",
    "5.Walpeup_Gums",
    "6.Crystal_Brook_Randals",
    "7.Wharminda_Bonanza",
    "8.Wynarka_Tanks"
  ),
  site_name = c(
    "Walpeup_MRS125",
    "Crystal_Brook_Brians_House",
    "Wynarka_Mervs_West",
    "Wharminda_Woodys",
    "Walpeup_Gums",
    "Crystal_Brook_Randals",
    "Wharminda_Bonanza",
    "Wynarka_Tanks"
  ),
  stringsAsFactors = FALSE
)

site_row    <- site_lookup[site_lookup$id == site_number_input, ]
site_number <- site_row$site_number
site_name   <- site_row$site_name
cat("Site selected:", site_number, "\n")

# =============================================================================
# PATHS
# =============================================================================


yr_short         <- substr(as.character(year_of_analysis), 3, 4)

dir           <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
headDir       <- file.path(dir, "work", "Output-1", site_number)
metadata_path <- file.path(dir, "work", "Output-1", "0.Site-info")
metadata_file <- "names of treatments per site 2025 metadata and other info.xlsx"

saveDir <- file.path(headDir,
                     "7.In_Season_data", yr_short,
                     "8.Sentinel_QGIS_Jackie",
                     "Growth_curves_output")

# =============================================================================
# READ TREATMENT COLOUR LOOKUP FROM METADATA
# =============================================================================

treat_colours <- readxl::read_excel(
  file.path(metadata_path, metadata_file),
  sheet = "treatment names"
) %>%
  filter(Site == site_number) %>%
  dplyr::select(treat, treat_desc = `Treatment Name`, hex = Hex) %>%
  distinct()

cat("Treatment colours loaded for", nrow(treat_colours), "treatments\n")

# Named vector for ggplot scale_colour_manual — keyed by treat_desc
colour_vec <- setNames(treat_colours$hex, treat_colours$treat_desc)

# =============================================================================
# READ ZONE LABELS FROM METADATA
# =============================================================================

zone_labels <- readxl::read_excel(
  file.path(metadata_path, metadata_file),
  sheet = "zone_details"
) %>%
  filter(Site == site_number) %>%
  dplyr::select(zone = `zone names`, zone_label = `zone label names`) %>%
  dplyr::mutate(zone = as.character(zone))

cat("Zone labels loaded for", nrow(zone_labels), "zones\n")

# =============================================================================
# READ CSVs
# =============================================================================

cat("\nReading CSV files...\n")

treat_only_file <- file.path(saveDir,
                             paste0(site_name, "_NDVI_treatment_only_DAP.csv"))
treat_zone_file <- file.path(saveDir,
                             paste0(site_name, "_NDVI_treatment_zone_DAP.csv"))

if (!file.exists(treat_only_file)) stop("File not found: ", treat_only_file)
if (!file.exists(treat_zone_file)) stop("File not found: ", treat_zone_file)

dat_treat <- read.csv(treat_only_file) %>%
  dplyr::mutate(date = as.Date(date))

dat_zone <- read.csv(treat_zone_file) %>%
  dplyr::mutate(
    date = as.Date(date),
    zone = as.character(zone)
  ) %>%
  left_join(zone_labels, by = "zone")

cat("Treatment-only rows:", nrow(dat_treat), "\n")
cat("Treatment x zone rows:", nrow(dat_zone), "\n")

# =============================================================================
# READ SOWING DATE (needed for secondary x-axis date labels)
# =============================================================================

seasons <- readxl::read_excel(
  file.path(metadata_path, metadata_file),
  sheet = "seasons"
) %>%
  filter(Site == site_number, Year == year_of_analysis)

sow_raw <- seasons$`Sowing date`[1]

plant_date <- if (inherits(sow_raw, "Date") || inherits(sow_raw, "POSIXct")) {
  as.Date(sow_raw)
} else if (is.numeric(sow_raw)) {
  as.Date(sow_raw, origin = "1899-12-30")
} else {
  sow_char <- trimws(as.character(sow_raw))
  if (grepl("^\\d{5}$", sow_char)) {
    as.Date(as.numeric(sow_char), origin = "1899-12-30")
  } else {
    lubridate::parse_date_time(
      sow_char,
      orders = c("dmy", "ymd", "mdy", "d-m-Y", "d/m/Y", "Y-m-d"),
      quiet  = TRUE
    ) %>% as.Date()
  }
}

cat("Sowing date:", format(plant_date), "\n")

# =============================================================================
# CALCULATE CUMULATIVE NDVI
# =============================================================================

dat_treat <- dat_treat %>%
  arrange(treat, DAP) %>%
  group_by(treat, treat_desc) %>%
  dplyr::mutate(cumulative_ndvi = cumsum(ifelse(is.na(mean_ndvi), 0, mean_ndvi))) %>%
  ungroup()

dat_zone <- dat_zone %>%
  arrange(treat, zone, DAP) %>%
  group_by(treat, treat_desc, zone, zone_label) %>%
  dplyr::mutate(cumulative_ndvi = cumsum(ifelse(is.na(mean_ndvi), 0, mean_ndvi))) %>%
  ungroup()

# =============================================================================
# ADAPTIVE LOESS SPAN — scales with number of image dates available
# =============================================================================

n_images   <- n_distinct(dat_treat$date)
loess_span <- dplyr::case_when(
  n_images <= 8  ~ 0.75,
  n_images <= 12 ~ 0.50,
  TRUE           ~ 0.25
)
cat("Image dates available:", n_images, "| loess span set to:", loess_span, "\n")



# =============================================================================
# SHARED THEME
# =============================================================================

theme_ndvi <- function() {
  theme_bw() +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(size = 10, colour = "grey40"),
      axis.title       = element_text(size = 11),
      axis.text        = element_text(size = 9),
      legend.title     = element_text(size = 10, face = "bold"),
      legend.text      = element_text(size = 9),
      legend.position  = "right",
      strip.text       = element_text(face = "bold", size = 10),
      strip.background = element_rect(fill = "grey92", colour = NA),
      panel.grid.minor = element_blank()
    )
}

# Helper to save and print a plot
save_plot <- function(p, filename, width = 22, height = 14) {
  out_path <- file.path(saveDir, filename)
  ggsave(out_path, plot = p, width = width, height = height,
         units = "cm", dpi = 200, bg = "white")
  cat("Saved:", out_path, "\n")
  print(p)
}

# =============================================================================
# PLOT 1A: GROWTH CURVE — TREATMENT ONLY
# =============================================================================

cat("\n--- Plot 1a: Growth curve (treatment only) ---\n")

# Three date labels for secondary axis: min, mid, max DAP
dap_date_lookup <- dat_treat %>%
  distinct(DAP, date) %>%
  arrange(DAP)

dap_min  <- min(dap_date_lookup$DAP)
dap_max  <- max(dap_date_lookup$DAP)
dap_mid  <- dap_date_lookup$DAP[which.min(abs(dap_date_lookup$DAP - mean(c(dap_min, dap_max))))]

three_breaks <- dap_date_lookup %>%
  filter(DAP %in% c(dap_min, dap_mid, dap_max))

# Wrap image dates across two lines
image_dates_vec <- dat_treat %>%
  distinct(date) %>%
  arrange(date) %>%
  pull(date) %>%
  format("%d %b")

n    <- length(image_dates_vec)
half <- ceiling(n / 2)
image_dates_caption <- paste0(
  "Image dates: ",
  paste(image_dates_vec[1:half],     collapse = "  |  "),
  "\n             ",
  paste(image_dates_vec[(half+1):n], collapse = "  |  ")
)

p1a <- ggplot(dat_treat,
              aes(x        = DAP,
                  y        = mean_ndvi,
                  colour   = treat_desc,
                  group    = treat_desc,
                  linewidth = treat_desc,
                  linetype  = treat_desc)) +
  geom_smooth(method = "loess", span = loess_span, se = FALSE) +
  geom_vline(
    data        = dap_date_lookup,
    aes(xintercept = DAP),
    inherit.aes = FALSE,
    colour      = "grey75",
    linewidth   = 0.3,
    linetype    = "11"
  ) +
  scale_colour_manual(values = colour_vec, name = NULL) +
  scale_linewidth_manual(
    values = c("Control" = 2.0,
               setNames(rep(1.0, nrow(treat_colours) - 1),
                        treat_colours$treat_desc[treat_colours$treat_desc != "Control"])),
    guide  = "none"
  ) +
  scale_linetype_manual(
    values = c("Control" = "11",
               setNames(rep("solid", nrow(treat_colours) - 1),
                        treat_colours$treat_desc[treat_colours$treat_desc != "Control"])),
    guide  = "none"
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 8),
    sec.axis = sec_axis(
      transform = ~ . + as.numeric(plant_date),
      name      = NULL,
      breaks    = three_breaks$DAP + as.numeric(plant_date),
      labels    = format(three_breaks$date, "%d %b")
    )
  ) +
  scale_y_continuous(
    limits = c(0, 0.85),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  labs(
    title    = paste0(site_name, " \u2014 NDVI Growth Curves"),
    subtitle = paste0(year_of_analysis, " season | All treatments"),
    x        = "Days after planting (DAP)",
    y        = "Mean NDVI"
  ) +
  theme_ndvi() +
  theme(
    legend.position       = "bottom",
    legend.key.width      = unit(1.5, "cm"),
    legend.text           = element_text(size = 9),
    legend.spacing.x      = unit(0.5, "cm"),
    axis.text.x.top       = element_text(size = 10, face = "bold", vjust = 0.5),
    axis.ticks.x.top      = element_line(linewidth = 1.0, colour = "grey30"),
    axis.ticks.length.x.top = unit(0.2, "cm"),
    panel.grid.major.y       = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y       = element_blank()
  )+
  guides(colour = guide_legend(override.aes = list(linewidth = 2)))


p1a


save_plot(p1a, paste0(site_name, "_growth_curve_treatment.png"))

# =============================================================================
# PLOT 1B: GROWTH CURVE — TREATMENT x ZONE (FACETED BY ZONE)
# =============================================================================
cat("\n--- Plot 1b: Growth curve (treatment x zone) ---\n")

# Use same date lookup but from dat_zone
dap_date_lookup_z <- dat_zone %>%
  distinct(DAP, date) %>%
  arrange(DAP)

dap_min_z <- min(dap_date_lookup_z$DAP)
dap_max_z <- max(dap_date_lookup_z$DAP)
dap_mid_z <- dap_date_lookup_z$DAP[which.min(abs(dap_date_lookup_z$DAP - mean(c(dap_min_z, dap_max_z))))]

three_breaks_z <- dap_date_lookup_z %>%
  filter(DAP %in% c(dap_min_z, dap_mid_z, dap_max_z))

p1b <- ggplot(dat_zone,
              aes(x         = DAP,
                  y         = mean_ndvi,
                  colour    = treat_desc,
                  group     = treat_desc,
                  linewidth = treat_desc,
                  linetype  = treat_desc)) +
  geom_smooth(method = "loess", span = loess_span, se = FALSE) +
  geom_vline(
    data        = dap_date_lookup_z,
    aes(xintercept = DAP),
    inherit.aes = FALSE,
    colour      = "grey75",
    linewidth   = 0.3,
    linetype    = "11"
  ) +
  facet_wrap(~ zone_label, ncol = 1) +
  #facet_wrap(~ zone_label) +
  scale_colour_manual(values = colour_vec, name = NULL) +
  scale_linewidth_manual(
    values = c("Control" = 2.0,
               setNames(rep(1.0, nrow(treat_colours) - 1),
                        treat_colours$treat_desc[treat_colours$treat_desc != "Control"])),
    guide  = "none"
  ) +
  scale_linetype_manual(
    values = c("Control" = "11",
               setNames(rep("solid", nrow(treat_colours) - 1),
                        treat_colours$treat_desc[treat_colours$treat_desc != "Control"])),
    guide  = "none"
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 8),
    sec.axis = sec_axis(
      transform = ~ . + as.numeric(plant_date),
      name      = NULL,
      breaks    = three_breaks_z$DAP + as.numeric(plant_date),
      labels    = format(three_breaks_z$date, "%d %b")
    )
  ) +
  scale_y_continuous(
    limits = c(0, 0.85),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  labs(
    title    = paste0(site_name, " \u2014 NDVI Growth Curves by Zone"),
    subtitle = paste0(year_of_analysis, " season | Faceted by soil zone"),
    x        = "Days after planting (DAP)",
    y        = "Mean NDVI"
  ) +
  theme_ndvi() +
  theme(
    #legend.position          = "bottom",
    legend.position          = "right",
    legend.key.width         = unit(1.5, "cm"),
    legend.text              = element_text(size = 9),
    legend.spacing.x         = unit(0.5, "cm"),
    axis.text.x.top          = element_text(size = 10, face = "bold", vjust = 0.5),
    axis.ticks.x.top         = element_line(linewidth = 1.0, colour = "grey30"),
    axis.ticks.length.x.top  = unit(0.2, "cm"),
    panel.grid.major.y       = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y       = element_blank(),
    strip.text               = element_text(face = "bold", size = 10)
  )+
  guides(colour = guide_legend(override.aes = list(linewidth = 2)))
p1b

save_plot(p1b, paste0(site_name, "_growth_curve_zone.png"),
          width = 22, height = 8 * length(unique(dat_zone$zone)))



# =============================================================================
# PLOT 1C: GROWTH CURVE — FACETED BY TREATMENT (control in every panel)
# =============================================================================

cat("\n--- Plot 1c: Growth curve (faceted by treatment, control as reference) ---\n")

# Active treatments only (exclude Control from facet panels)
active_treats <- treat_colours %>%
  filter(treat_desc != "Control") %>%
  pull(treat_desc)

# Ghost dataset: Control repeated for every active treatment panel
control_ghost <- dat_treat %>%
  filter(treat_desc == "Control") %>%
  tidyr::crossing(facet_treat = active_treats)

# Main dataset: active treatments only, with facet variable
dat_treat_facet <- dat_treat %>%
  filter(treat_desc != "Control") %>%
  dplyr::mutate(facet_treat = treat_desc)

p1c <- ggplot() +
  # Control ghost line (grey dashed) in every panel
  geom_smooth(
    data    = control_ghost,
    aes(x = DAP, y = mean_ndvi, group = treat_desc),
    method  = "loess", span = loess_span, se = FALSE,
    colour  = colour_vec["Control"],
    linewidth = 1.2,
    linetype  = "11"
  ) +
  # Active treatment line
  geom_smooth(
    data    = dat_treat_facet,
    aes(x = DAP, y = mean_ndvi, colour = treat_desc, group = treat_desc),
    method  = "loess", span = loess_span, se = FALSE,
    linewidth = 1.0,
    linetype  = "solid"
  ) +
  geom_vline(
    data        = dap_date_lookup,
    aes(xintercept = DAP),
    inherit.aes = FALSE,
    colour      = "grey75",
    linewidth   = 0.3,
    linetype    = "11"
  ) +
  facet_wrap(~ facet_treat, ncol = 2) +
  scale_colour_manual(values = colour_vec, name = NULL) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 6),
    sec.axis = sec_axis(
      transform = ~ . + as.numeric(plant_date),
      name      = NULL,
      breaks    = three_breaks$DAP + as.numeric(plant_date),
      labels    = format(three_breaks$date, "%d %b")
    )
  ) +
  scale_y_continuous(
    limits = c(0, 0.85),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  labs(
    title    = paste0(site_name, " \u2014 NDVI Growth Curves by Treatment"),
    subtitle = paste0(year_of_analysis, " season | Grey dashed = Control reference"),
    x        = "Days after planting (DAP)",
    y        = "Mean NDVI"
  ) +
  theme_ndvi() +
  theme(
    legend.position          = "none",   # colour is redundant with facet label
    axis.text.x.top          = element_text(size = 9, face = "bold", vjust = 0.5),
    axis.ticks.x.top         = element_line(linewidth = 1.0, colour = "grey30"),
    axis.ticks.length.x.top  = unit(0.2, "cm"),
    panel.grid.major.y       = element_blank(),
    panel.grid.major.x       = element_blank(),
    strip.text               = element_text(face = "bold", size = 10)
  )

p1c

save_plot(p1c, paste0(site_name, "_growth_curve_by_treatment.png"),
          width = 22, height = 8 * ceiling(length(active_treats) / 2))


# =============================================================================
# PLOT 2A: CUMULATIVE NDVI — TREATMENT ONLY
# =============================================================================

cat("\n--- Plot 2a: Cumulative NDVI (treatment only) ---\n")

p2a <- ggplot(dat_treat,
              aes(x         = DAP,
                  y         = cumulative_ndvi,
                  colour    = treat_desc,
                  group     = treat_desc,
                  linewidth = treat_desc,
                  linetype  = treat_desc)) +
  geom_line() +
  geom_vline(
    data        = three_breaks,
    aes(xintercept = DAP),
    inherit.aes = FALSE,
    colour      = "grey75",
    linewidth   = 0.3,
    linetype    = "11"
  ) +
  scale_colour_manual(values = colour_vec, name = NULL) +
  scale_linewidth_manual(
    values = c("Control" = 2.0,
               setNames(rep(1.0, nrow(treat_colours) - 1),
                        treat_colours$treat_desc[treat_colours$treat_desc != "Control"])),
    guide  = "none"
  ) +
  scale_linetype_manual(
    values = c("Control" = "11",
               setNames(rep("solid", nrow(treat_colours) - 1),
                        treat_colours$treat_desc[treat_colours$treat_desc != "Control"])),
    guide  = "none"
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 8),
    sec.axis = sec_axis(
      transform = ~ . + as.numeric(plant_date),
      name      = NULL,
      breaks    = three_breaks$DAP + as.numeric(plant_date),
      labels    = format(three_breaks$date, "%d %b")
    )
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  labs(
    title    = paste0(site_name, " \u2014 Cumulative NDVI"),
    subtitle = paste0(year_of_analysis, " season | All treatments"),
    x        = "Days after planting (DAP)",
    y        = "Cumulative NDVI"
  ) +
  theme_ndvi() +
  theme(
    legend.position          = "bottom",
    legend.key.width         = unit(1.5, "cm"),
    legend.text              = element_text(size = 9),
    legend.spacing.x         = unit(0.5, "cm"),
    axis.text.x.top          = element_text(size = 10, face = "bold", vjust = 0.5),
    axis.ticks.x.top         = element_line(linewidth = 1.0, colour = "grey30"),
    axis.ticks.length.x.top  = unit(0.2, "cm"),
    panel.grid.major.y       = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y       = element_blank()
  )+
  guides(colour = guide_legend(override.aes = list(linewidth = 2)))

p2a

save_plot(p2a, paste0(site_name, "_cumulative_ndvi_treatment.png"))

# =============================================================================
# PLOT 2B: CUMULATIVE NDVI — TREATMENT x ZONE
# =============================================================================

cat("\n--- Plot 2b: Cumulative NDVI (treatment x zone) ---\n")

p2b <- ggplot(dat_zone,
              aes(x         = DAP,
                  y         = cumulative_ndvi,
                  colour    = treat_desc,
                  group     = treat_desc,
                  linewidth = treat_desc,
                  linetype  = treat_desc)) +
  geom_line() +
  geom_vline(
    data        = three_breaks,
    aes(xintercept = DAP),
    inherit.aes = FALSE,
    colour      = "grey75",
    linewidth   = 0.3,
    linetype    = "11"
  ) +
  #facet_wrap(~ zone_label, ncol = 1) +
  facet_wrap(~ zone_label) +
  scale_colour_manual(values = colour_vec, name = NULL) +
  scale_linewidth_manual(
    values = c("Control" = 2.0,
               setNames(rep(1.0, nrow(treat_colours) - 1),
                        treat_colours$treat_desc[treat_colours$treat_desc != "Control"])),
    guide  = "none"
  ) +
  scale_linetype_manual(
    values = c("Control" = "11",
               setNames(rep("solid", nrow(treat_colours) - 1),
                        treat_colours$treat_desc[treat_colours$treat_desc != "Control"])),
    guide  = "none"
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 8),
    sec.axis = sec_axis(
      transform = ~ . + as.numeric(plant_date),
      name      = NULL,
      breaks    = three_breaks$DAP + as.numeric(plant_date),
      labels    = format(three_breaks$date, "%d %b")
    )
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  labs(
    title    = paste0(site_name, " \u2014 Cumulative NDVI by Zone"),
    subtitle = paste0(year_of_analysis, " season | Faceted by soil zone"),
    x        = "Days after planting (DAP)",
    y        = "Cumulative NDVI"
  ) +
  theme_ndvi() +
  theme(
    legend.position          = "bottom",
    legend.key.width         = unit(1.5, "cm"),
    legend.text              = element_text(size = 9),
    legend.spacing.x         = unit(0.5, "cm"),
    axis.text.x.top          = element_text(size = 9, face = "bold", vjust = 0.5, angle = 45),
    axis.ticks.x.top         = element_line(linewidth = 1.0, colour = "grey30"),
    axis.ticks.length.x.top  = unit(0.1, "cm"),
    panel.grid.major.y       = element_blank(),
    panel.grid.major.x       = element_blank(),
    panel.grid.minor.y       = element_blank(),
    strip.text               = element_text(face = "bold", size = 10)
  ) +
  guides(colour = guide_legend(override.aes = list(linewidth = 2)))


p2b


save_plot(p2b, paste0(site_name, "_cumulative_ndvi_zone.png"),
          width = 22, height = 8 * length(unique(dat_zone$zone)))


# =============================================================================
# PLOT 2C: CUMULATIVE NDVI — FACETED BY TREATMENT (control in every panel)
# =============================================================================

cat("\n--- Plot 2c: Cumulative NDVI (faceted by treatment, control as reference) ---\n")

control_ghost_cum <- dat_treat %>%
  filter(treat_desc == "Control") %>%
  tidyr::crossing(facet_treat = active_treats)

dat_treat_facet_cum <- dat_treat %>%
  filter(treat_desc != "Control") %>%
  dplyr::mutate(facet_treat = treat_desc)

p2c <- ggplot() +
  geom_line(
    data      = control_ghost_cum,
    aes(x = DAP, y = cumulative_ndvi, group = treat_desc),
    colour    = colour_vec["Control"],
    linewidth = 1.2,
    linetype  = "11"
  ) +
  geom_line(
    data      = dat_treat_facet_cum,
    aes(x = DAP, y = cumulative_ndvi, colour = treat_desc, group = treat_desc),
    linewidth = 1.0,
    linetype  = "solid"
  ) +
  geom_vline(
    data        = three_breaks,
    aes(xintercept = DAP),
    inherit.aes = FALSE,
    colour      = "grey75",
    linewidth   = 0.3,
    linetype    = "11"
  ) +
  facet_wrap(~ facet_treat, ncol = 2) +
  scale_colour_manual(values = colour_vec, name = NULL) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 6),
    sec.axis = sec_axis(
      transform = ~ . + as.numeric(plant_date),
      name      = NULL,
      breaks    = three_breaks$DAP + as.numeric(plant_date),
      labels    = format(three_breaks$date, "%d %b")
    )
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  labs(
    title    = paste0(site_name, " \u2014 Cumulative NDVI by Treatment"),
    subtitle = paste0(year_of_analysis, " season | Grey dashed = Control reference"),
    x        = "Days after planting (DAP)",
    y        = "Cumulative NDVI"
  ) +
  theme_ndvi() +
  theme(
    legend.position          = "none",
    axis.text.x.top          = element_text(size = 9, face = "bold", vjust = 0.5),
    axis.ticks.x.top         = element_line(linewidth = 1.0, colour = "grey30"),
    axis.ticks.length.x.top  = unit(0.2, "cm"),
    panel.grid.major.y       = element_blank(),
    panel.grid.major.x       = element_blank(),
    strip.text               = element_text(face = "bold", size = 10)
  )

p2c

save_plot(p2c, paste0(site_name, "_cumulative_ndvi_by_treatment.png"),
          width = 22, height = 8 * ceiling(length(active_treats) / 2))
# =============================================================================
# PLOT 3A: AUC BAR CHART — TREATMENT ONLY
# =============================================================================

cat("\n--- Plot 3a: AUC bar chart (treatment only) ---\n")

auc_treat <- dat_treat %>%
  distinct(treat, treat_desc, AUC) %>% 
  filter(treat_desc != "Buffer") |> 
  filter(treat_desc  !="Outside Control")

p3a <- ggplot(auc_treat,
              aes(x = reorder(treat_desc, AUC), y = AUC,
                  fill = treat_desc)) +
  geom_col(width = 0.7, colour = "white") +
  geom_text(aes(label = round(AUC, 1)),
            hjust = -0.15, size = 3.2, colour = "grey30") +
  scale_fill_manual(values = colour_vec, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  coord_flip() +
  labs(
    title    = paste0(site_name, " \u2014 Area Under NDVI Curve (AUC)"),
    subtitle = paste0(year_of_analysis, " season | All treatments"),
    x        = "AUC (NDVI-days)",
    y        = NULL,
    caption  = "AUC = area under the NDVI growth curve (trapezoidal rule).\nHigher values indicate greater cumulative canopy greenness across the season,\nreflecting both peak NDVI and duration of green cover."
  ) +
  theme_ndvi() +
  theme(panel.grid.major.y = element_blank())

p3a

save_plot(p3a, paste0(site_name, "_AUC_treatment.png"),
          width = 20, height = 12)

# =============================================================================
# PLOT 3B: AUC BAR CHART — TREATMENT x ZONE
# =============================================================================

cat("\n--- Plot 3b: AUC bar chart (treatment x zone) ---\n")

auc_zone <- dat_zone %>%
  distinct(treat, treat_desc, zone, zone_label, AUC) %>% 
  filter(treat_desc != "Buffer") |> 
  filter(treat_desc  !="Outside Control")

p3b <- ggplot(auc_zone,
              aes(x = reorder(treat_desc, AUC), y = AUC,
                  fill = treat_desc)) +
  geom_col(width = 0.7, colour = "white") +
  geom_text(aes(label = round(AUC, 1)),
            hjust = -0.15, size = 3.2, colour = "grey30") +
  scale_fill_manual(values = colour_vec, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  coord_flip() +
  facet_wrap(~ zone_label, ncol = 1) +
  labs(
    title    = paste0(site_name, " — Area Under NDVI Curve (AUC) by Zone"),
    subtitle = paste0(year_of_analysis, " season | Faceted by soil zone"),
    x        = "AUC (NDVI-days)",
    y        = NULL,
    caption  = "AUC = area under the NDVI growth curve (trapezoidal rule).\nHigher values indicate greater cumulative canopy greenness across the season,\nreflecting both peak NDVI and duration of green cover."
  ) +
    
  theme_ndvi() +
  theme(panel.grid.major.y = element_blank())
p3b
save_plot(p3b, paste0(site_name, "_AUC_zone.png"),
          width = 20, height = 8 * length(unique(auc_zone$zone)))



cat("\n=== Script 2 complete ===\n")
cat("All plots saved to:", saveDir, "\n")

