
# =============================================================================
# SCRIPT 0: PRE-CHECK OF SENTINEL DOWNLOADS — af-sandysoils-ii
# =============================================================================
# Purpose:
#   Run BEFORE Script 1, each time new Sentinel images are downloaded.
#   For every site and year it reads the "Sentinel_All_Results*.csv" file(s)
#   saved with the download and checks that the downloaded data is complete:
#     - search window end date, first/last image, last clear image
#     - sowing and harvest dates (from the metadata "seasons" sheet)
#     - how many clear images (cloud prob <= cloud_prob_max) the CSV lists,
#       how many NDVI TIFs are in the NDVI subfolder, and which clear dates
#       have no TIF (i.e. images that did not get downloaded)
#     - a flag column summarising any problem
#
# Inputs:
#   - <site>/7.In_Season_data/YY/8.Sentinel_QGIS_Jackie/Sentinel_All_Results*.csv
#   - <site>/7.In_Season_data/YY/8.Sentinel_QGIS_Jackie/NDVI/*NDVI*10m.tif
#   - Metadata sheet "seasons" (Sowing date, Harvest date)
#
# Output:
#   A table printed to the console (nothing is written to disk).
#
# How to read the flag column:
#   "NDVI TIFs missing"          clear dates in the CSV with no TIF in NDVI folder
#   "ends >14d before harvest"   last clear image is well before harvest (2025)
#   "in-season: >21d old"        no harvest date yet and last clear image is old
#   "ok"                         nothing to chase
#
# Note: file names can be wrong (a file called "..._to_2026_10_15" actually
#   searched to 15 Sep). This script reads the real search window inside
#   the file ("search date" column).
#
# Author:  Jackie Ouzman, CSIRO Agriculture & Food
# Project: af-sandysoils-ii
# Created: Oct 2026
# =============================================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(stringr)
  library(lubridate)
})

# =============================================================================
# USER SETTINGS
# =============================================================================
cloud_prob_max <- 30          # same rule as the download: SC CLOUDY PROB PERCENT <= 30
years_to_check <- c("25", "26")
flag_days_before_harvest <- 14
flag_days_in_season      <- 21

# =============================================================================
# PATHS
# =============================================================================
dir       <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"
root      <- file.path(dir, "work", "Output-1")
meta_path <- file.path(root, "0.Site-info",
                       "names of treatments per site 2025 metadata and other info.xlsx")

sites <- c("1.Walpeup_MRS125", "2.Crystal_Brook_Brians_House", "3.Wynarka_Mervs_West",
           "4.Wharminda_Woodys", "5.Walpeup_Gums", "6.Crystal_Brook_Randals",
           "7.Wharminda_Bonanza", "8.Wynarka_Tanks")

folders <- expand.grid(site = sites, yy = years_to_check, stringsAsFactors = FALSE)
folders$path <- file.path(root, folders$site, "7.In_Season_data", folders$yy,
                          "8.Sentinel_QGIS_Jackie")

# =============================================================================
# SOWING AND HARVEST DATES
# =============================================================================
to_date <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXct")) as.Date(x)
  else if (is.numeric(x)) as.Date(x, origin = "1899-12-30")
  else as.Date(lubridate::parse_date_time(trimws(as.character(x)),
                                          orders